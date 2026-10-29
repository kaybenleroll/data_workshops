# data_classes: the region x line-of-business loss-class model

Only `exposure_df.csv` is data. It is a byte-identical copy of the generator's Phase 1 file
`construct_synthetic_reinsurance_data/examples/mega/csv/exposure_df.csv` (mega tier, seed 2003;
sha256 `e726e3c383f9c2b3e1605aab280acc4784c9af21fe44b120cf1773020bcd2e7e`, recorded in the generator's
`examples/PROVENANCE.json`). Everything else is generated at render time from fixed seeds by
`class_model.R` and `class_engine.R` in the parent directory. Nothing generated is committed.

## What is simulated

| Piece | Rule | Function |
|---|---|---|
| Regions | 6: North America, Europe, UK & Ireland, Asia Pacific, Latin America, Middle East & Africa, with exposure weights and an adjacency list | `region_table()` |
| Loss class | **region x line of business** (the generator's 20 lines, named from `lob_df`, e.g. `Property Cat 1.1 \| Europe`). Peril is not part of the key | `class_table(lob_df, seed)` |
| Sparsity | each line operates in a deterministic subset of 2-5 regions (class seed 2003: 65 classes). A line's premium and expected losses (`exposure_df`) are split over its regions by region weight | |
| Perils | Windstorm, Earthquake, Flood, Fire, Marine, Liability, Cyber. A peril is an **event attribute**: it fixes frequency (Poisson), the footprint (regions) and which lines are hit (hit probability and relative severity per line group) | `peril_table()`, `peril_vulnerability()` |
| Events | peril events hit **many classes at once** (natural-cat perils: property-group lines in 1-3 adjacent regions; Liability and Cyber: casualty and specialty lines in 1-2 regions; Fire and Marine: one region, one to a few lines), severity lognormal (heavy tail, truncated). Plus about 400 single-class background events per period (small, 1-3 risks) | `simulate_events(class_table, n_periods, seed)` |
| Risks | each (event, class) loss splits into 1-40 **risks** (building/location level) with lognormal weights (sigma 1.4), so the largest risk can be a big share. Count grows with severity and class premium | `split_into_risks(class_losses, seed)` |
| Treaty filters | a treaty covers a set of lines x a set of regions, derived from the generator's footprint (`coverage_df`, for per-risk, per-occurrence, clash) or declared scope (lob / segment / group). About 35% of treaties other than quota shares, group quota shares and ILW are restricted to 1-3 of the regions where their lines operate, chosen by hashing the tower or treaty id. All layers of a tower share one filter | `treaty_filters()` |
| Bridge | filters expanded against the class table give (treaty, class): many-to-many. A treaty's subject loss for an event is the sum over its covered classes | `treaty_class_bridge()` |

Seeds: class table and treaty filters use `class_seed = 2003` (the generator's seed); events and risk splits
use `sim_seed = 2017` (chosen from a scan of seeds 2001-2030 for a largest event of a few billion: 2.4bn
earthquake over 10 classes). RNG streams are separate per component (`seed + 11, 21, 22, 23, 24, 31`), the
caller's `.Random.seed` is restored, and treaty restrictions use a string hash, so results do not depend on call order.
`class_world()` builds everything in about 1 second; one engine run takes about 0.2 seconds.

## Calibration

Background events reproduce each class's expected attritional loss. Peril losses are normalised per class
(`class_norm()`) so the expected annual peril loss of every class equals 1.3 x its expected large + cat loss
in `exposure_df`, whatever the footprint rules; the realised result is heavy tailed. Seed 2017: total gross
23.1bn over 10 periods (the generator's mega run: 26.6bn), largest event 2.37bn (earthquake, 10 classes, 122 risks),
about 9,500 loss rows.

## The engine: explicit, non-transitive inuring

`class_engine.R`. For every loss row (event, class, risk) and treaty T:

    subject(T) = max(0, gross - sum of ceded(P) over the DIRECT inuring parents P of T)

then T applies its terms to the subject loss by kind: row-wise (quota share, group quota share, surplus: flat
`cession_pct`; per-risk XoL: attachment and limit per risk row), event-aggregate (per-occurrence XoL, clash: layer
terms on the event sum of subject, allocated back pro rata) and period-aggregate (aggregate XL, stop loss: once
per period on the period sum, allocated pro rata). Reinstatements: per layer and period, total recovery is capped
at limit x (1 + reinstatements); events are folded in `event_seq` order and the running total resets each period.

* **Edges express "net of".** The inuring table (`from_treaty_id -> to_treaty_id`: from inures to the benefit of
  to) lists every direct relationship: an edge means the child's subject loss is reduced by the parent's recovery.
  Edges are treaty-to-treaty only (the generator has no class-scoped edges), explicit and non-transitive: A->B and
  B->C does not make C net of A unless A->C is listed. The table is read exactly as written; the primary run never
  completes, infers or closes it. The engine has no `chain_rank`, no residual column and no tie-break: any
  topological order of the table gives the same ledger (checked on 20 random orders).
* **Simultaneous recovery on one loss is normal** (peer layers of a tower, a quota share and an XoL), so two
  treaties with overlapping classes do not need an edge. **Validity is outcome-based**: (a) net >= 0 on every loss
  row (total ceded <= gross) is a hard invariant, and a violation is a data error (an invalid contract), flagged
  by the engine with counts and listed by `outcome_violations()` (rows, events, treaty pairs by readable name),
  never clamped; (b) each treaty's recovery is <= its own subject loss, and caps and reinstatement limits hold.
  The minimum-retention rule is deliberately set to 0 for this talk.
* **Primary table.** The generator's `edges_df` (133 layer-level edges), mapped to units (a tower is one unit,
  tower layers parallel, edges to or from any layer apply to the tower, ILW edges dropped, duplicates merged, the
  2-cycles that contraction creates resolved by dropping the 5 unit edges that run backwards in chain rank) is the
  primary table if its outcome has no row with net < 0. For this class model it does not: 71 unit edges over 58 units
  link 66 of 251 overlapping pairs (26%; the same 66 of 257 at LOB level, regions ignored), and 2,219 of 9,518 rows
  end with net < 0 (3.27bn, 14% of gross; 150 of the 185 unlinked pairs are involved, mostly proportional covers
  stacking: a quota share, a surplus and a group quota share that each cede on the same rows without being net of
  one another). That is reported as a data error, and the primary table becomes the **repair** built by
  `inuring_table()`: one direct edge for every pair of indemnity units (towers contracted, ILW excluded) with
  overlapping class sets, oriented from the lower to the higher generator `chain_rank` (ties by treaty id), no
  transitive shortcuts. `chain_rank` is used only there, as the stand-in for inuring wording in real contracts. The
  repair has net >= 0 on every row and gives the same ledger as the earlier residual-ledger engine in rank order
  (`run_residual_engine()`, kept for comparison). The generator's own flow code does not read `edges_df`
  (generator issue #177); this engine does, which is the point of the talk. A thin inuring table can break
  validity: the contrast is `contrast_inuring()`; the proper home for the missing relationships is generator
  issues #208 and #211.
* **Peer exceptions** (`peer_exception()`, `pick_peer_exceptions()`, `pick_valid_peer_exceptions()`, `apply_peer_exceptions()`): a
  treaty that is deliberately not net of an earlier peer is simply the table without that edge. The three hash-chosen
  same-type picks of `pick_peer_exceptions()` together break validity (164 rows with net < 0). Valid exceptions exist:
  `pick_valid_peer_exceptions()` searches deterministically (proportional same-type pairs first, then any same-type
  pair, then any edge; at most 300 edges), tests every candidate with the engine on top of the ones already accepted and
  keeps up to three with net >= 0 on every row and a visible effect on the treaty's ceded. For class seed 2003 and sim seed
  2017 it finds three after testing six edges (a group quota share, a quota share and a surplus). `check_classes.R` reports them.
* **Invariants as a function**: `engine_invariants(world, res)` returns the live checks used on the slides (accounting,
  net >= 0, subject and caps, determinism, order independence).

## Dropped or changed against the generator

* ILW (industry-index trigger, off-ledger): ignored.
* Premium-linked quota-share caps: dropped. Surplus is a flat `cession_pct` as in the generator's flows.
* The generator's loss model (events typed attritional / large / cat, one loss row per event, treaty applicability
  hard-coded by treaty type x loss type) is replaced by classes, perils and risks.
* The generator's per-risk detail within a cell does not exist there; risks here are simulated.

The proper versions of these (class table, peril footprints, risks, per-treaty class filters) are tracked as
generator issues #205-#211 on `kaybenleroll/construct_synthetic_reinsurance_data`.

## Checks

`Rscript check_classes.R` from the talk directory prints PASS/FAIL for: bridge sanity, DAG and order checks,
accounting (gross = sum of ceded + net on every row; net >= 0 on the primary table; ceded <= subject loss for every
treaty and row), caps and reinstatement limits, determinism (md5 of the ledger), input-order and
topological-order independence, and equivalence of the repair table with the residual engine. It also runs the
generator's table and reports its outcome, applies the peer exceptions, and prints the headline numbers.

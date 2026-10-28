# data_mega: mega-scale synthetic reinsurance set with a full loss year-set

Read-only csv copies (mode 444) of one generator run. Used by `recover_functions.R` and
`reconcile_mega.R` in the parent directory. Do not edit; regenerate with the command below.

## Provenance

| Item | Value |
|---|---|
| Generator | `construct_synthetic_reinsurance_data` (`scripts/generate.R`) |
| Generator commit | `700ad46327c6aee8a0332416f994715e4bcff10d` (2026-10-27, "Install libglpk40 so igraph loads in the image") |
| Generator image | `localhost/kaybenleroll/construct_synthetic_reinsurance_data:latest` (image id `fac18740af75...`, built 2026-10-27) |
| Tier / seed / phase / periods | `mega` / 2003 / 2 (Phase 1 portfolio + Phase 2 events and flows) / 10 |
| Format | csv |
| Source run | `construct_synthetic_reinsurance_data/data-generated/mega-2003-p2-10/csv` |

```
Rscript scripts/generate.R --seed=2003 --tier=mega --phase=2 --periods=10 --format=csv --out=<dir>
```

run through the generator's container. Determinism was confirmed by the generator's owner (a second run
with the same arguments gave identical files). The eight Phase 1 files below marked `P1` are byte-identical
to the generator's committed `examples/mega/csv` (checked against the sha256 values in the generator's
`examples/PROVENANCE.json`, which records generator sha `72e3ba3406faaa3e8bc2b461c17a5ab7e26c357d` and
`--seed=2003 --tier=mega`). The Phase 2 files (`event`, `loss`, `period`, `flow`, `treaty_period`) are not in
the committed examples.

Kept: only what the engine and the reconciliation need. Dropped from the run: `exposure_df.csv`,
`needs_df.csv`.

## File inventory

| File | Rows | P1 | Used for |
|---|---:|:-:|---|
| `treaty_df.csv` | 127 | P1 | treaty terms, towers, chain rank |
| `edges_df.csv` | 133 | P1 | inuring DAG (between treaties; tower layers are separate nodes) |
| `coverage_df.csv` | 291 | P1 | footprint LOBs for xol_per_risk, xol_per_occurrence, clash |
| `premium_df.csv` | 127 | P1 | net subject premium (reinstatement premium only) |
| `lob_df.csv` | 20 | P1 | LOB to segment, LOB group |
| `segment_df.csv` | 10 | P1 | segment to division |
| `division_df.csv` | 5 | P1 | division to portfolio |
| `portfolio_df.csv` | 1 | P1 | portfolio (group scope) |
| `period_df.csv` | 10 | | period order |
| `event_df.csv` | 871 | | events: 200 attritional, 645 large, 25 cat, 1 systemic |
| `loss_df.csv` | 890 | | gross loss cells (200 attritional, 645 large, 45 cat) |
| `flow_df.csv` | 7,031 | | the generator's answer (reconciliation target only) |
| `treaty_period_df.csv` | 1,270 | | the generator's per treaty-period reinstatement use (reconciliation target only) |

## What a loss row is

`loss_df` has one row per (event, LOB, loss_type): the gross loss of that event in that LOB. Treaties never
see "an event"; they see the cells of an event that their loss-class rule and their region let through.

- `loss_type` equals the event's `event_type` on every event in this run: no event has more than one loss type.
- **attritional**: one event per (period, LOB), an annual aggregate attritional loss (200 = 10 periods x 20 LOBs),
  one cell each.
- **large**: a single-LOB event (645 of them), one cell each.
- **cat**: the only class that spans LOBs. 25 events; 21 have loss rows (5 with one LOB, 10 with two, 4 with
  three, 2 with four); 4 cat events and the one `systemic` event have no loss rows (the ILW treaties are still
  evaluated on those cat events, because they pay on `industry_loss`, not on the ledger).
- The recovery engine's `loss_anatomy()` returns these tables, the column lists, three example events and a
  per-treaty "who sees what" table for each.

## Column dictionary (tables the engine uses)

`treaty_df` (one row per treaty; a tower is several rows sharing `tower_id`)

| Column | Meaning |
|---|---|
| `treaty_id`, `tower_id`, `layer_index`, `n_layers` | key; tower id (NA if not a tower); layer 1 is the lowest |
| `treaty_type` | quota_share, group_quota_share, surplus, xol_per_risk, xol_per_occurrence, clash, aggregate_xl, stop_loss, ilw |
| `scope_type`, `scope_id` | declared scope: `lob`, `segment` or `group` and its id (used by proportional, aggregate, stop-loss) |
| `chain_rank` | position in the generator's application order, one per layer (ties broken by `treaty_id`) |
| `cession_pct` | flat cession for quota_share, group_quota_share, surplus |
| `attachment`, `limit` | layer attachment and limit (USD); for surplus `limit = surplus_lines x retention`, not enforced in flows |
| `reinstatements` | paid reinstatements for xol_per_risk, xol_per_occurrence, clash: cap `(reinstatements + 1) x limit` per treaty-period |
| `premium_linked_limit`, `premium_linked_limit_factor` | quota-share annual cap switch; `limit` is the cap |
| `industry_trigger` | ILW trigger on `event_df.industry_loss` (ILW pays `limit`) |
| `rol` | rate on line (structurally derived) |

`edges_df`: `from_treaty_id` inures to the benefit of `to_treaty_id` (from is applied first); also
`chain_rank_from`, `chain_rank_to`, `rank_gap`, `edge_kind` (adjacent/skip). All 133 edges run forward in chain rank.

`coverage_df` (`treaty_id`, `lob_id`, `source`): footprint LOBs, replicated for each layer of a tower.

`event_df`: `event_id`, `period_id`, `event_type`, `event_seq` (order within the period), `event_severity_factor`
and `systemic_multiplier` (cat only, may be NA), `industry_loss` (cat only; ILW trigger index).

`loss_df`: `event_id`, `period_id`, `lob_id`, `loss_type`, `gross_loss`.

`period_df`: `period_id`, `period_index`. `lob_df`: `lob_id`, `segment_id`, `lob_group`, loss-split columns.
`segment_df`: `segment_id`, `division_id`. `division_df`: `division_id`, `portfolio_id`.
`premium_df`: `treaty_id`, `net_subject_premium`, `rol`, ... (only `net_subject_premium` is used).

`flow_df` (the generator's ledger, one row per event x treaty x LOB cell): `period_id`, `event_id`, `treaty_id`,
`lob_id`, `loss_type`, `chain_rank` (tower-pinned), `is_indemnity`, `subject_loss_in`, `ceded`, `retained_after`,
`reinstatement_consumed` (`ceded / limit` per row, XoL types only). `treaty_period_df`: `reinstatements_used`,
`reinstatement_premium`, `aggregate_consumed` per treaty and period.

## Applicability (encoded as data in `recover_functions.R`: `responds`, `treaty_rules`)

Checked against the generator (`lib/lib_flows.R` dispatch, `lib/lib_terms.R` `.terms_applicability()`).

| treaty_type | attritional | large | cat | region | cadence | cap state |
|---|:-:|:-:|:-:|---|---|---|
| quota_share, group_quota_share | yes | yes | yes | declared scope | per event, per cell, flat `cession_pct` | premium-linked cap (only if `premium_linked_limit`), running total per treaty-period |
| surplus | yes | yes | yes | declared scope | per event, per cell, flat `cession_pct` | none (the `premium_linked_limit` flag on surplus rows is ignored by the generator) |
| xol_per_risk | no | yes | no | coverage footprint | per event, layers share the aggregated residual | reinstatement cap per layer, treaty, period |
| xol_per_occurrence, clash | no | no | yes | coverage footprint | as above | as above |
| aggregate_xl, stop_loss | yes | yes | yes | declared scope | once per period on the whole-period residual | none (one application per period is the cap) |
| ilw | no | no | no | none | pays `limit` if `industry_loss >= industry_trigger` on every cat event | off-ledger, `is_indemnity = FALSE` |

Towers: every layer applies its own attachment and limit to the same pre-tower residual (layers do not erode
each other); the recovery is written back to the cells pro rata to their residual. No reinstatement or cap state
carries between periods.

## Deviations from the generator

- **Numerical: none found.** The engine reproduces all 7,031 `flow_df` rows (`ceded`, `retained_after`,
  `subject_loss_in`, `reinstatement_consumed`) and all 1,270 `treaty_period_df` rows. Largest relative difference 5.4e-13
  (csv rounding of the stored values), largest absolute difference 3.6e-7 on a ceded amount.
- **Order is derived from the DAG, not read from `chain_rank`.** Kahn's algorithm on the treaty-level `edges_df` graph with
  `(chain_rank, treaty_id)` as the tie-break gives exactly the generator's order, because every edge runs forward in chain rank.
  Towers are then placed atomically at their first (layer 1) position. That placement breaks 14 of the 133 layer-level edges
  (5 tower or stop-loss pairs): an outside treaty sits between two layers of a tower. If towers were contracted to a node
  first, the graph would contain three 2-cycles, so the order cannot be a strict topological order of the contracted graph.
- **Event-major folding.** The generator loops treaty-major; the engine folds event by event within each run of
  consecutive per-event treaties, and period by period for annual covers. Results are identical because loss cells of
  different events are disjoint and all cap state is per treaty-period.
- Premium-linked cap and exhaustion follow the generator's sequential cell-by-cell rule; exhaustion sums `ceded` by
  (event, treaty) before comparing with the limit (relative tolerance 1e-9).

## Caveats

- `rol`, attachment and limit are structurally derived by the generator from the portfolio, not sourced actuarial figures.
  Treat the amounts as realistic in shape, not as market data.
- The generator's own flow walk ignores `edges_df` (it orders by `chain_rank`); see issue #177 on
  `kaybenleroll/construct_synthetic_reinsurance_data`. The DAG is therefore a faithful description of the order only because
  the edges are consistent with chain rank, not because the generator reads them.
- Attritional loss is an annual aggregate per (period, LOB), not a stream of claims; the aggregate and stop-loss covers
  therefore see it as one cell.
- One cat event in P001 is 6.0bn gross, about 23% of the ten-period gross; period 1 is not typical.

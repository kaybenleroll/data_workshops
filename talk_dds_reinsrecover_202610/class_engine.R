# class_engine.R -- recovery engine for the region x line loss-class model, with explicit inuring.
#
# Sourceable, no side effects beyond defining functions (and a private environment `.old_engine`
# holding the previous engine's helpers, so that recover_functions.R is reused, not modified, and
# its function names are not overwritten). Needs dplyr, tibble, tidyr, readr, igraph (namespaced).
# Depends on class_model.R (sourced here if its helpers are missing).
#
# Pipeline (every step is a pure function; state is threaded through Reduce, never mutated):
#   class_world()                    -> data, class table, filters, bridge, inuring tables, loss rows, primary choice
#   generator_inuring(data)          -> the generator's edges_df as a unit-level table (primary if its outcome is valid)
#   inuring_table(treaty_df, bridge) -> repair: one explicit direct edge per overlapping indemnity pair (primary if not)
#   run_inuring_engine(world, inuring) -> list(rows, ledger, events, treaty_period, order, inuring); flags invalid outcomes
#   outcome_violations(res) inuring_diagnostics(world, inuring, res) -> net < 0 rows/events/pairs; pairs with/without edge
#   peer_exception() pick_peer_exceptions() pick_valid_peer_exceptions() apply_peer_exceptions() -> remove an edge: treaty not net of that parent
#   run_residual_engine(world)       -> the earlier residual-ledger engine in chain_rank order (comparison only)
#   gross_by() net_by() recoveries_by() exhaustion() treaty_reach() class_treaty_matrix() loss_anatomy2()
#   inuring_stats() interaction_coverage() compare_inuring() over_recovery() over_recovery_pairs()
#   contrast_inuring() order_sensitivity()
#
# SEMANTICS
#   A loss row = one RISK of one class in one event (risk_id, gross_loss). A treaty T sees the rows whose
#   class is in its bridge classes. INURING IS EXPLICIT AND NOT TRANSITIVE: the inuring table lists every
#   direct relationship (from inures to the benefit of to). For a row r and treaty T
#       subject_r(T) = max(0, gross_r - sum over DIRECT parents P of T of ceded_r(P))
#   (a parent only reduces rows it recovered on: ceded_r(P) is 0 elsewhere). T then applies its terms to
#   subject_r(T) by kind. Net to the cedant = gross_r - sum over all treaties of ceded_r(T).
#   VALIDITY IS OUTCOME-BASED. Two treaties recovering at the same time on one loss is normal (peer layers,
#   a quota share and an XoL), so an overlapping pair does not need an edge: an edge only says that one
#   treaty's recovery reduces the other's subject loss ("net of"). The invariants are (a) net >= 0 on every
#   loss row (total ceded <= gross); a violation is a DATA ERROR (an invalid contract), flagged with the
#   offending rows, events and treaty pairs (outcome_violations()) and never clamped; (b) each treaty's
#   recovery <= its own subject loss, and caps and reinstatement limits hold. The table is read exactly as
#   written: never completed, inferred or made transitive in the primary run. The minimum-retention rule is
#   deliberately set to 0 for this talk.
#   The engine has no chain_rank, no residual column and no tie-break: any topological order of the table
#   gives the same ledger (order_sensitivity() checks this). chain_rank appears only in inuring_table()
#   (the repair table: orientation of each overlapping pair, the stand-in for real inuring wording) and, as a
#   cycle resolver, in normalise_inuring() when tower contraction of a table creates cycles.
#   Tables are mapped to units: a tower is one unit, edges to/from any layer of a tower apply to the tower
#   as a unit, edges inside a tower are dropped (layers are parallel), ilw is dropped (non-indemnity).
#   ROW-WISE   quota_share, group_quota_share, surplus: ceded = cession_pct x subject (flat, as the
#              generator does for surplus; the premium-linked quota-share cap is dropped).
#   ROW-WISE   xol_per_risk: each risk's subject runs through the layer terms (attachment, limit).
#   EVENT-AGG  xol_per_occurrence, clash: layer terms on the SUM of subject over covered rows in the
#              event; the recovery is allocated back to rows pro rata to subject.
#   PERIOD-AGG aggregate_xl, stop_loss: layer terms once per period on the sum of subject over covered
#              rows; allocated pro rata.
#   Towers: all layers read the same subject loss; attachments are absolute (layers do not erode each other).
#   Reinstatements (xol_per_risk, xol_per_occurrence, clash): per layer (= treaty row) and period the
#   total recovery is capped at limit x (1 + reinstatements). Events are folded in event_seq order and the
#   running total resets each period: event e gets min(theoretical_e, cap - used_before_e). For per-risk
#   layers the unit of the fold is the event: theoretical recovery of the event = sum over risks of
#   min(max(risk - attachment, 0), limit); when the cap binds all risks of the event are scaled by one
#   common factor. Each risk's recovery is always <= limit; per-event totals of per-risk layers may exceed it.
#   Aggregate and stop-loss layers: one application per period, so recovery <= limit, no further cap.
#   Note: the generator's own flow code does not read edges_df (issue #177); this engine does.

if (!exists("with_seed", mode = "function") && file.exists("class_model.R")) source("class_model.R")

# The previous (cell-based) engine, loaded into a private environment: its helpers (units, Kahn order,
# edge_violations) are reused; its gross_by/net_by/recoveries_by/exhaustion keep working for old-style
# results through the dispatching versions below.
.old_engine <- new.env(parent = globalenv())
if (file.exists("recover_functions.R")) sys.source("recover_functions.R", envir = .old_engine)

# ---- Inputs ----------------------------------------------------------------------------------------
load_class_inputs <- function(dir = "data_mega", classes_dir = "data_classes") {
  rd <- function(f) readr::read_csv(file.path(dir, f), show_col_types = FALSE, guess_max = Inf, progress = FALSE)
  list(treaty_df = rd("treaty_df.csv"), edges_df = rd("edges_df.csv"), coverage_df = rd("coverage_df.csv"),
       lob_df = rd("lob_df.csv"), segment_df = rd("segment_df.csv"), period_df = rd("period_df.csv"),
       exposure_df = read_exposure(file.path(classes_dir, "exposure_df.csv")))
}

# Everything the engine and the slides need, from two seeds: class_seed fixes the class table and the
# regional restrictions; sim_seed fixes events and risk splits. Primary table (`inuring`): the generator's
# edges_df if its outcome has no row with net < 0, otherwise the locally built complete table (a repair of the
# generator's table); `inuring_choice` records the decision and both outcomes are always available.
class_world <- function(class_seed = 2003, sim_seed = 2017, n_periods = 10, dir = "data_mega", classes_dir = "data_classes") {
  data <- load_class_inputs(dir, classes_dir)
  ct <- class_table(data$lob_df, class_seed, data$exposure_df)
  filters <- treaty_filters(data$treaty_df, data$coverage_df, ct, class_seed)
  bridge <- treaty_class_bridge(filters, ct)
  sim <- simulate_events(ct, n_periods, sim_seed)
  gen <- generator_inuring(data)
  loc <- inuring_table(data$treaty_df, bridge)
  lr <- split_into_risks(sim$class_losses, sim_seed)
  w <- list(data = data, class_table = ct, filters = filters, bridge = bridge, class_seed = class_seed,
            sim_seed = sim_seed, n_periods = n_periods, inuring_generator = gen, inuring_local = loc, inuring = gen,
            events_sim = sim$events, class_losses = sim$class_losses, loss_rows = lr)
  oc <- over_recovery(run_inuring_engine(w, gen, on_invalid = "ignore"))
  w$inuring_choice <- list(which = if (oc$n_rows_over == 0) "generator" else "local", generator_outcome = oc)
  if (oc$n_rows_over > 0) w$inuring <- loc
  w
}

# ---- Treaty kinds ----------------------------------------------------------------------------------
engine_kinds <- tibble::tribble(
  ~treaty_type,         ~kind,    ~capped,
  "quota_share",        "flat",   FALSE,
  "group_quota_share",  "flat",   FALSE,
  "surplus",            "flat",   FALSE,
  "xol_per_risk",       "risk",   TRUE,
  "xol_per_occurrence", "event",  TRUE,
  "clash",              "event",  TRUE,
  "aggregate_xl",       "period", FALSE,
  "stop_loss",          "period", FALSE,
  "ilw",                "none",   FALSE)

# ---- Kernels (pure: subject and terms in, ceded matrix rows x layers out) ---------------------------
gsum <- function(x, g, n) { out <- numeric(n); s <- rowsum(x, g); out[as.integer(rownames(s))] <- s[, 1]; out }

# Running total of theoretical recoveries per period, clipped at cap: the closed form of the fold
# `act_e = min(theo_e, cap - used_before_e)`. `theo` is ordered by event_seq within period.
cap_fold <- function(theo, period, cap) {
  cum <- stats::ave(theo, period, FUN = cumsum)
  pmin(cum, cap) - pmin(cum - theo, cap)
}

kernel_flat <- function(r, terms) matrix(r * dplyr::coalesce(terms$cession_pct[1], 0), ncol = 1)

# kind: "risk" | "event" | "period"; r subject of covered rows; ev, per: event / period index of each row;
# ev_period: period index of every event.
kernel_layered <- function(kind, r, ev, per, ev_period, terms) {
  g <- if (kind == "period") per else ev
  u <- sort(unique(g)); gi <- match(g, u); G <- length(u)
  R <- gsum(r, gi, G)
  pg <- if (kind == "period") u else ev_period[u]
  cols <- lapply(seq_len(nrow(terms)), function(l) {
    att <- terms$attachment[l]; lim <- terms$limit[l]
    cap <- (dplyr::coalesce(terms$reinstatements[l], 0) + 1) * lim
    if (kind == "risk") {
      theo <- pmin(pmax(r - att, 0), lim); T <- gsum(theo, gi, G)
      act <- cap_fold(T, pg, cap)
      theo * ifelse(T > 0, act / T, 0)[gi]
    } else {
      theo <- pmin(pmax(R - att, 0), lim)
      act <- if (kind == "event") cap_fold(theo, pg, cap) else theo
      r * ifelse(R > 0, act / R, 0)[gi]
    }
  })
  matrix(unlist(cols), nrow = length(r))
}

# Apply a treaty unit to the covered rows i with subject r: ceded matrix rows x layers, never above r.
unit_ceded <- function(unit, i, r, ctx) {
  rows <- ctx$rows
  m <- if (unit$kind == "flat") kernel_flat(r, unit$terms)
       else kernel_layered(unit$kind, r, rows$event_idx[i], rows$period_idx[i], ctx$ev_period, unit$terms)
  tot <- rowSums(m)
  m * ifelse(tot > r, r / tot, 1)                           # rounding guard: never cede more than the subject
}

chunk <- function(i, unit, r, m) {
  L <- length(unit$tidx); n <- length(i)
  list(row = rep(i, L), tidx = rep(unit$tidx, each = n), subject = rep(r, L), ceded = as.vector(m))
}

# ---- Units and inuring tables ------------------------------------------------------------------------
engine_units <- function(data) {
  units <- .old_engine$recovery_units(data)
  t <- data$treaty_df
  lapply(seq_len(nrow(units)), function(k) {
    tidx <- match(units$treaty_ids[[k]], t$treaty_id)
    ty <- units$treaty_type[k]
    list(key = units$unit_key[k], treaty_type = ty, kind = engine_kinds$kind[match(ty, engine_kinds$treaty_type)],
         tidx = tidx, anchor = t$treaty_id[tidx[1]], name = t$name[tidx[1]], rank = units$chain_rank[k],
         terms = t[tidx, c("attachment", "limit", "reinstatements", "cession_pct")])
  })
}

unit_table <- function(data) {
  u <- engine_units(data)
  tibble::tibble(unit = vapply(u, `[[`, "", "key"), anchor = vapply(u, `[[`, "", "anchor"),
                 treaty_type = vapply(u, `[[`, "", "treaty_type"), name = vapply(u, `[[`, "", "name"),
                 rank = vapply(u, `[[`, 0, "rank"), n_layers = vapply(u, function(x) length(x$tidx), 1L))
}

empty_inuring <- function() tibble::tibble(from_treaty_id = character(), to_treaty_id = character())

# Map any inuring table (from_treaty_id -> to_treaty_id, layer or anchor ids) to the engine's unit graph:
# tower layers collapse to one unit; edges inside a tower and edges touching ilw are dropped; duplicates
# merge; if tower contraction made the graph cyclic, edges that run backwards in unit chain_rank are dropped
# (they are what atomic tower placement breaks). Returns the unit-level table with an `audit` attribute.
normalise_inuring <- function(inuring, data) {
  ut <- unit_table(data); t <- data$treaty_df
  lk <- stats::setNames(dplyr::coalesce(t$tower_id, t$treaty_id), t$treaty_id)
  e <- tibble::tibble(from_unit = unname(lk[inuring$from_treaty_id]), to_unit = unname(lk[inuring$to_treaty_id]))
  n_raw <- nrow(e)
  e <- e[!is.na(e$from_unit) & !is.na(e$to_unit), ]
  n_within <- sum(e$from_unit == e$to_unit); e <- e[e$from_unit != e$to_unit, ]
  ilw <- ut$unit[ut$treaty_type == "ilw"]
  n_ilw <- sum(e$from_unit %in% ilw | e$to_unit %in% ilw); e <- e[!(e$from_unit %in% ilw | e$to_unit %in% ilw), ]
  n_before_dedupe <- nrow(e); e <- dplyr::distinct(e)
  n_dup <- n_before_dedupe - nrow(e)
  rk <- stats::setNames(ut$rank, ut$unit); an <- stats::setNames(ut$anchor, ut$unit)
  n_back <- 0L
  is_dag <- function(d) igraph::is_dag(igraph::graph_from_data_frame(d, directed = TRUE, vertices = data.frame(name = ut$unit)))
  if (!is_dag(e)) {
    back <- rk[e$from_unit] > rk[e$to_unit] | (rk[e$from_unit] == rk[e$to_unit] & an[e$from_unit] > an[e$to_unit])
    n_back <- sum(back); e <- e[!back, ]
    stopifnot(is_dag(e))
  }
  e <- e[order(rk[e$to_unit], an[e$to_unit], rk[e$from_unit], an[e$from_unit], method = "radix"), ]
  mi <- function(u) match(u, ut$unit)
  out <- tibble::tibble(from_treaty_id = an[e$from_unit], to_treaty_id = an[e$to_unit],
                        from_unit = e$from_unit, to_unit = e$to_unit,
                        from_type = ut$treaty_type[mi(e$from_unit)], to_type = ut$treaty_type[mi(e$to_unit)],
                        from_name = ut$name[mi(e$from_unit)], to_name = ut$name[mi(e$to_unit)])
  attr(out, "audit") <- tibble::tibble(edges_in = n_raw, within_tower = n_within, touching_ilw = n_ilw,
                                       duplicates_merged = n_dup, backward_dropped = n_back, edges_out = nrow(out))
  out
}

# The generator's own edges_df as the engine's inuring table (tower layers parallel, see normalise_inuring).
generator_inuring <- function(data) normalise_inuring(data$edges_df, data)

# Interacting pairs: units whose class sets overlap (both indemnity). One row per unordered pair.
interaction_pairs <- function(bridge, treaty_df) {
  t <- treaty_df[treaty_df$treaty_type != "ilw", ]
  cls <- sort(unique(bridge$class_id))
  M <- matrix(0, nrow(t), length(cls))
  b <- bridge[bridge$treaty_id %in% t$treaty_id, ]
  M[cbind(match(b$treaty_id, t$treaty_id), match(b$class_id, cls))] <- 1
  O <- M %*% t(M)
  ij <- which(upper.tri(O) & O > 0, arr.ind = TRUE)
  tibble::tibble(a = t$treaty_id[ij[, 1]], b = t$treaty_id[ij[, 2]], type_a = t$treaty_type[ij[, 1]],
                 type_b = t$treaty_type[ij[, 2]], name_a = t$name[ij[, 1]], name_b = t$name[ij[, 2]],
                 n_shared_classes = as.integer(O[ij]))
}

# REPAIR inuring table (primary only if the generator's table gives net < 0 somewhere): one explicit direct edge for every pair of indemnity units (towers
# contracted, ilw excluded) whose class sets overlap, oriented from the lower to the higher generator
# chain_rank (ties by treaty_id). chain_rank is used ONLY here, as the stand-in for the inuring wording of
# real contracts. No transitive shortcuts. `seed` is unused (the construction is deterministic).
inuring_table <- function(treaty_df, bridge, seed = NULL) {
  data <- list(treaty_df = treaty_df)
  ut <- unit_table(data)
  p <- unit_pairs(bridge, treaty_df)
  rk <- stats::setNames(ut$rank, ut$anchor)
  swap <- rk[p$a] > rk[p$b] | (rk[p$a] == rk[p$b] & p$a > p$b)
  from <- ifelse(swap, p$b, p$a); to <- ifelse(swap, p$a, p$b)
  mi <- function(x) match(x, ut$anchor)
  out <- tibble::tibble(from_treaty_id = from, to_treaty_id = to, from_unit = ut$unit[mi(from)], to_unit = ut$unit[mi(to)],
                        from_type = ut$treaty_type[mi(from)], to_type = ut$treaty_type[mi(to)],
                        from_name = ut$name[mi(from)], to_name = ut$name[mi(to)], n_shared_classes = p$n_shared_classes)
  out[order(rk[out$to_treaty_id], out$to_treaty_id, rk[out$from_treaty_id], out$from_treaty_id, method = "radix"), ]
}

# Degree distributions and the treaties with the most inuring parents.
inuring_stats <- function(inuring, data, n_top = 5) {
  ut <- unit_table(data); ut <- ut[ut$treaty_type != "ilw", ]
  indeg <- tabulate(match(inuring$to_unit, ut$unit), nrow(ut)); outdeg <- tabulate(match(inuring$from_unit, ut$unit), nrow(ut))
  q <- function(x) c(min = min(x), median = stats::median(x), mean = round(mean(x), 2), max = max(x))
  top <- tibble::tibble(name = ut$name, treaty_type = ut$treaty_type, n_parents = indeg, n_children = outdeg)
  list(n_units = nrow(ut), n_edges = nrow(inuring), in_degree = q(indeg), out_degree = q(outdeg),
       n_isolated = sum(indeg == 0 & outdeg == 0),
       top_in_degree = utils::head(top[order(-top$n_parents, top$name), ], n_top))
}

# Interacting unit pairs (towers contracted, ilw excluded) whose class sets overlap: one row per unordered pair.
unit_pairs <- function(bridge, treaty_df) {
  ut <- unit_table(list(treaty_df = treaty_df))
  anchors <- treaty_df[match(ut$anchor[ut$treaty_type != "ilw"], treaty_df$treaty_id), ]
  interaction_pairs(bridge, anchors)
}
pair_key <- function(a, b) ifelse(a < b, paste(a, b), paste(b, a))

# How many interacting unit pairs (overlapping class sets) have an explicit edge (either direction)?
interaction_coverage <- function(world, inuring = world$inuring, bridge = world$bridge) {
  e <- normalise_inuring(inuring, world$data)
  p <- unit_pairs(bridge, world$data$treaty_df)
  has <- pair_key(p$a, p$b) %in% pair_key(e$from_treaty_id, e$to_treaty_id)
  p$has_edge <- has
  top <- p[!has, ] |> dplyr::mutate(pair_type = ifelse(type_a <= type_b, paste(type_a, type_b, sep = " + "), paste(type_b, type_a, sep = " + "))) |>
    dplyr::count(pair_type, sort = TRUE) |> utils::head(5)
  list(n_interacting = nrow(p), n_with_edge = sum(has), n_without_edge = sum(!has), pairs = p, top_missing_type_pairs = top,
       edges_without_overlap = sum(!pair_key(e$from_treaty_id, e$to_treaty_id) %in% pair_key(p$a, p$b)))
}

# PEER EXCEPTION: `treaty` is deliberately not net of `parent` -- simply remove the edge parent -> treaty (any
# layer ids; unit-level table in, unit-level table out). Nothing else changes; whether the outcome stays valid
# (net >= 0 on every row) is for outcome_violations() to say.
peer_exception <- function(inuring, treaty, parent, treaty_df) {
  e <- normalise_inuring(inuring, list(treaty_df = treaty_df))
  lk <- stats::setNames(dplyr::coalesce(treaty_df$tower_id, treaty_df$treaty_id), treaty_df$treaty_id)
  e[!(e$from_unit == unname(lk[parent]) & e$to_unit == unname(lk[treaty])), ]
}

# Deterministic choice of n peer exceptions: edges parent -> treaty in the primary table between treaties of the
# SAME type (stop-loss and aggregate targets excluded: they rarely pay), ordered by a hash of the pair, one per treaty.
pick_peer_exceptions <- function(world, n = 3) {
  cand <- world$inuring |> dplyr::filter(from_type == to_type, !to_type %in% c("stop_loss", "aggregate_xl"))
  cand$h <- hash_unif(paste(cand$from_treaty_id, cand$to_treaty_id), "peer")
  cand <- cand[order(cand$h, method = "radix"), ]
  cand <- cand[!duplicated(cand$to_treaty_id), ]
  utils::head(cand[c("from_treaty_id", "to_treaty_id", "from_name", "to_name", "from_type")], n)
}

# Deterministic SEARCH for peer exceptions that keep the outcome valid. Candidates are edges parent -> treaty of the
# primary table between treaties of the same type, in three tiers (both proportional of one kind -- quota share,
# group quota share or surplus; any other same-type pair; any edge), each ordered by a hash of the pair. Candidates are
# tested with the engine one after another ON TOP OF the exceptions already accepted (table without the accepted edges and
# this one: net >= 0 on every row?), so the picks are jointly valid. A pick needs a visible effect (the treaty's ceded
# changes) and a treaty of its own. Stops after `n` picks or `max_search` edges tested. Returns the picks with the treaty's
# ceded before (primary run) and after (all accepted exceptions so far), and the total net likewise; attributes
# `searched` (edges tested), `valid_found` (valid picks) and `candidates`.
pick_valid_peer_exceptions <- function(world, n = 3, max_search = 300, base = run_inuring_engine(world, on_invalid = "ignore")) {
  e <- normalise_inuring(world$inuring, world$data)
  same <- e$from_type == e$to_type
  prop <- same & e$to_type %in% c("quota_share", "group_quota_share", "surplus")
  e$tier <- ifelse(prop, 1L, ifelse(same, 2L, 3L))
  e$h <- hash_unif(paste(e$from_treaty_id, e$to_treaty_id), "peer")
  e <- e[order(e$tier, e$h, method = "radix"), ]
  e <- utils::head(e, max_search)
  ced_of <- function(r, tid) sum(r$ledger$ceded[r$ledger$treaty_id == tid])
  cur <- normalise_inuring(world$inuring, world$data)
  key <- function(d) paste(d$from_treaty_id, d$to_treaty_id)
  found <- list(); tested <- 0L
  for (i in seq_len(nrow(e))) {
    if (length(found) >= n) break
    if (e$to_treaty_id[i] %in% vapply(found, function(f) f$to_treaty_id, "")) next
    tested <- tested + 1L
    trial <- cur[key(cur) != key(e[i, ]), ]
    wx <- world; wx$inuring <- trial
    r <- run_inuring_engine(wx, on_invalid = "ignore")
    if (over_recovery(r)$n_rows_over > 0) next
    before <- ced_of(base, e$to_treaty_id[i]); after <- ced_of(r, e$to_treaty_id[i])
    if (abs(after - before) <= 1e-6 * max(1, before)) next
    cur <- trial
    found[[length(found) + 1L]] <- tibble::tibble(
      from_treaty_id = e$from_treaty_id[i], to_treaty_id = e$to_treaty_id[i], from_name = e$from_name[i], to_name = e$to_name[i],
      from_type = e$from_type[i], to_type = e$to_type[i], tier = e$tier[i], ceded_before = before, ceded_after = after,
      net_before = sum(base$rows$net_loss), net_after = sum(r$rows$net_loss))
  }
  out <- if (length(found)) dplyr::bind_rows(found) else tibble::tibble()
  attr(out, "searched") <- tested; attr(out, "valid_found") <- length(found); attr(out, "candidates") <- nrow(e)
  out
}

# A world with the exceptions applied (primary inuring table replaced).
apply_peer_exceptions <- function(world, exceptions = pick_peer_exceptions(world)) {
  w <- world
  w$inuring <- Reduce(function(tab, i) peer_exception(tab, exceptions$to_treaty_id[i], exceptions$from_treaty_id[i], world$data$treaty_df),
                      seq_len(nrow(exceptions)), world$inuring)
  w$exceptions <- exceptions
  w
}

# Overlapping unit pairs with and without a direct edge, and the pairs without an edge split into those that
# cause over-recovery on at least one row (using the run) and those that are harmless.
inuring_diagnostics <- function(world, inuring = world$inuring, res = run_inuring_engine(world, inuring, on_invalid = "ignore")) {
  ic <- interaction_coverage(world, inuring)
  bad <- over_recovery_pairs(res, n_top = Inf)
  bad_keys <- if (nrow(bad)) pair_key(bad$a, bad$b) else character()
  p <- ic$pairs[!ic$pairs$has_edge, ]
  harmful <- pair_key(p$a, p$b) %in% bad_keys
  list(n_overlapping = ic$n_interacting, n_with_edge = ic$n_with_edge, n_without_edge = ic$n_without_edge,
       n_without_edge_harmful = sum(harmful), n_without_edge_harmless = sum(!harmful),
       edges_between_disjoint_treaties = ic$edges_without_overlap, harmful_pairs = bad)
}

# Edges of table a that are in table b (same direction), reversed in b, or absent from b.
compare_inuring <- function(a, b) {
  ka <- paste(a$from_treaty_id, a$to_treaty_id); kb <- paste(b$from_treaty_id, b$to_treaty_id)
  kbr <- paste(b$to_treaty_id, b$from_treaty_id)
  tibble::tibble(n_a = nrow(a), n_b = nrow(b), a_in_b = sum(ka %in% kb), a_reversed_in_b = sum(ka %in% kbr),
                 a_absent_from_b = sum(!ka %in% kb & !ka %in% kbr), b_absent_from_a = sum(!kb %in% ka & !kb %in% paste(a$to_treaty_id, a$from_treaty_id)))
}

# ---- Orders --------------------------------------------------------------------------------------------
topo_order <- function(inuring, data) {
  ut <- unit_table(data)
  g <- igraph::graph_from_data_frame(inuring[c("from_unit", "to_unit")], directed = TRUE, vertices = data.frame(name = ut$unit))
  stopifnot(igraph::is_dag(g))
  names(igraph::topo_sort(g, mode = "out"))
}
rank_order <- function(data) { ut <- unit_table(data); ut$unit[order(ut$rank, ut$anchor, method = "radix")] }

# A random valid topological order (random choice among available nodes).
random_topo_order <- function(inuring, data, seed) {
  ut <- unit_table(data); n <- nrow(ut)
  from <- match(inuring$from_unit, ut$unit); to <- match(inuring$to_unit, ut$unit)
  ord <- with_seed(seed, Reduce(function(st, k) {
    avail <- which(st$indeg == 0L & !st$done)
    pick <- if (length(avail) == 1L) avail else sample(avail, 1)
    list(done = replace(st$done, pick, TRUE), indeg = st$indeg - tabulate(to[from == pick], n), order = c(st$order, pick))
  }, seq_len(n), list(done = rep(FALSE, n), indeg = tabulate(to, n), order = integer(0)))$order)
  ut$unit[ord]
}

# ---- Preparation and assembly ------------------------------------------------------------------------------
# Canonical row order (period, event_seq, event, class, risk): results never depend on input row order.
prepare_class_rows <- function(loss_rows, class_table) {
  lr <- loss_rows[order(loss_rows$period_id, loss_rows$event_seq, loss_rows$event_id, loss_rows$class_id,
                        loss_rows$risk_id, method = "radix"), ]
  periods <- sort(unique(lr$period_id))
  ev_key <- unique(lr[c("period_id", "event_seq", "event_id")])
  lr$row_id <- seq_len(nrow(lr))
  lr$period_idx <- match(lr$period_id, periods)
  lr$event_idx <- match(lr$event_id, ev_key$event_id)
  lr$class_idx <- match(lr$class_id, class_table$class_id)
  list(rows = lr, periods = periods, ev_period = match(ev_key$period_id, periods))
}

cover_matrix <- function(world) {
  t <- world$data$treaty_df; ct <- world$class_table
  m <- matrix(FALSE, nrow(ct), nrow(t))
  m[cbind(match(world$bridge$class_id, ct$class_id), match(world$bridge$treaty_id, t$treaty_id))] <- TRUE
  m
}

assemble_run <- function(world, prep, chunks, net, order, inuring, engine) {
  data <- world$data; t <- data$treaty_df
  units <- .old_engine$recovery_units(data)
  col <- function(nm) unlist(lapply(chunks, `[[`, nm), use.names = FALSE)
  lr <- col("row"); ti <- col("tidx"); subj <- col("subject"); ced <- col("ceded")
  o <- order(lr, ti, method = "radix")                      # canonical ledger order, independent of application order
  lr <- lr[o]; ti <- ti[o]; subj <- subj[o]; ced <- ced[o]
  rows <- prep$rows
  rank_of <- units$chain_rank[match(dplyr::coalesce(t$tower_id, t$treaty_id), units$unit_key)]
  capped <- engine_kinds$capped[match(t$treaty_type, engine_kinds$treaty_type)]
  ledger <- tibble::tibble(
    row_id = lr, treaty_idx = ti, period_id = rows$period_id[lr], event_id = rows$event_id[lr], event_seq = rows$event_seq[lr],
    peril = rows$peril[lr], class_id = rows$class_id[lr], region = rows$region[lr], lob_id = rows$lob_id[lr],
    lob_group = rows$lob_group[lr], group = rows$group[lr], risk_id = rows$risk_id[lr],
    treaty_id = t$treaty_id[ti], treaty_name = t$name[ti], treaty_type = t$treaty_type[ti],
    layer_index = t$layer_index[ti], limit = t$limit[ti], reinstatements = t$reinstatements[ti],
    subject_loss_in = subj, ceded = ced)
  ledger$retained_after <- ledger$subject_loss_in - ledger$ceded
  ledger$reinstatement_consumed <- ifelse(capped[ti] & t$limit[ti] > 0, ledger$ceded / t$limit[ti], NA_real_)
  rows$net_loss <- net
  structure(list(rows = rows, ledger = ledger, order = order, periods = prep$periods, inuring = inuring, engine = engine,
                 treaties = tibble::tibble(treaty_id = t$treaty_id, unit = dplyr::coalesce(t$tower_id, t$treaty_id)),
                 units = unit_table(data),
                 events = class_event_summary(rows, ledger), treaty_period = class_treaty_period(ledger, t)),
            class = "class_run")
}

# ---- The recovery: explicit inuring ---------------------------------------------------------------------------
# One unit: subject = max(0, gross - ceded by its direct parents); apply terms; record own ceded.
apply_unit_inuring <- function(state, unit, ctx) {
  if (unit$kind == "none") return(state)
  par <- ctx$parents[[unit$key]]
  pc <- if (length(par)) Reduce(`+`, state$ceded[par]) else 0
  subject <- pmax(0, ctx$rows$gross_loss - pc)
  i <- which(ctx$cover[ctx$rows$class_idx, unit$tidx[1]] & subject > 0)
  if (!length(i)) { st <- state$ceded; st[[unit$key]] <- 0; return(list(ceded = st, ledger = state$ledger)) }
  r <- subject[i]
  m <- unit_ceded(unit, i, r, ctx)
  own <- numeric(nrow(ctx$rows)); own[i] <- rowSums(m)
  st <- state$ceded; st[[unit$key]] <- own
  list(ceded = st, ledger = c(state$ledger, list(chunk(i, unit, r, m))))
}

# world: from class_world(); inuring: any inuring table (default: world$inuring); order: unit keys in
# application order (default: an igraph topological order of the table -- any valid order gives the same ledger).
run_inuring_engine <- function(world, inuring = world$inuring, order = NULL, loss_rows = world$loss_rows,
                               on_invalid = c("warn", "stop", "ignore")) {
  on_invalid <- match.arg(on_invalid)
  data <- world$data
  e <- normalise_inuring(inuring, data)
  if (is.null(order)) order <- topo_order(e, data)
  stopifnot(!anyNA(match(e$from_unit, order)), all(match(e$from_unit, order) < match(e$to_unit, order)))   # valid order
  prep <- prepare_class_rows(loss_rows, world$class_table)
  specs <- engine_units(data)
  specs <- specs[match(order, vapply(specs, `[[`, "", "key"))]
  ctx <- list(rows = prep$rows, cover = cover_matrix(world), ev_period = prep$ev_period,
              parents = split(e$from_unit, factor(e$to_unit, levels = unique(c(order, e$to_unit)))))
  zero <- stats::setNames(rep(list(numeric(nrow(prep$rows))), 0), character(0))
  fin <- Reduce(function(st, u) apply_unit_inuring(st, u, ctx), specs, list(ceded = zero, ledger = list()))
  tot <- if (length(fin$ceded)) Reduce(`+`, fin$ceded) else 0
  res <- assemble_run(world, prep, fin$ledger, prep$rows$gross_loss - tot, order, e, "inuring")
  oc <- over_recovery(res)
  if (oc$n_rows_over > 0 && on_invalid != "ignore") {
    msg <- sprintf(paste0("INVALID OUTCOME (data error): total recoveries exceed the loss on %d of %d rows (%d events), %.3fbn = %.2f%% of gross. ",
                          "See outcome_violations(); the inuring table is missing 'net of' relationships."),
                   oc$n_rows_over, oc$n_rows, length(unique(res$rows$event_id[res$rows$net_loss < -1e-9 * pmax(1, res$rows$gross_loss)])),
                   oc$amount_over / 1e9, 100 * oc$frac_of_gross)
    if (on_invalid == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  res
}

# ---- The earlier residual-ledger engine (comparison only) ------------------------------------------------------
# One residual column, treaties applied in chain_rank order: equivalent to the complete inuring table.
apply_unit_residual <- function(state, unit, ctx) {
  if (unit$kind == "none") return(state)
  i <- which(ctx$cover[ctx$rows$class_idx, unit$tidx[1]] & state$residual > 0)
  if (!length(i)) return(state)
  r <- state$residual[i]
  m <- unit_ceded(unit, i, r, ctx)
  newres <- state$residual; newres[i] <- r - rowSums(m)
  list(residual = newres, ledger = c(state$ledger, list(chunk(i, unit, r, m))))
}

run_residual_engine <- function(world, loss_rows = world$loss_rows) {
  data <- world$data
  units <- .old_engine$recovery_units(data)
  order <- .old_engine$order_chain_rank(units)
  prep <- prepare_class_rows(loss_rows, world$class_table)
  specs <- engine_units(data)
  specs <- specs[match(order, vapply(specs, `[[`, "", "key"))]
  ctx <- list(rows = prep$rows, cover = cover_matrix(world), ev_period = prep$ev_period)
  fin <- Reduce(function(st, u) apply_unit_residual(st, u, ctx), specs, list(residual = prep$rows$gross_loss, ledger = list()))
  assemble_run(world, prep, fin$ledger, fin$residual, order, NULL, "residual")
}

# ---- Run-level summaries ------------------------------------------------------------------------------------------
class_event_summary <- function(rows, ledger) {
  ced <- gsum(ledger$ceded, ledger$row_id, nrow(rows))
  rows |> dplyr::mutate(ceded_row = ced) |>
    dplyr::group_by(event_id, period_id, event_seq, peril, kind) |>
    dplyr::summarise(n_classes = dplyr::n_distinct(class_id), n_risks = dplyr::n(), gross = sum(gross_loss),
                     ceded = sum(ceded_row), net = sum(net_loss), .groups = "drop") |>
    (\(d) d[order(d$period_id, d$event_seq, method = "radix"), ])()
}

# Per (treaty, period): total ceded, limit, utilisation = ceded / limit (reinstatements used for capped kinds).
class_treaty_period <- function(ledger, treaty_df) {
  tp <- ledger |> dplyr::group_by(treaty_id, treaty_name, treaty_type, period_id) |>
    dplyr::summarise(subject_loss_in = sum(subject_loss_in), ceded = sum(ceded), .groups = "drop")
  m <- match(tp$treaty_id, treaty_df$treaty_id)
  capped <- engine_kinds$capped[match(tp$treaty_type, engine_kinds$treaty_type)]
  tp$limit <- treaty_df$limit[m]; tp$reinstatements <- treaty_df$reinstatements[m]
  tp$utilisation <- ifelse(is.na(tp$limit) | tp$limit <= 0, NA_real_, tp$ceded / tp$limit)
  tp$cap <- ifelse(capped, (dplyr::coalesce(tp$reinstatements, 0) + 1) * tp$limit, NA_real_)
  tp
}

# Rows where the treaties together ceded more than the gross loss (net < 0): a data error on any valid table.
over_recovery <- function(res, tol = 1e-9) {
  r <- res$rows
  neg <- r$net_loss < -tol * pmax(1, r$gross_loss)
  tibble::tibble(n_rows = nrow(r), n_rows_over = sum(neg), amount_over = sum(pmax(0, -r$net_loss[neg])),
                 total_gross = sum(r$gross_loss), frac_of_gross = amount_over / total_gross,
                 max_row_over = if (any(neg)) max(-r$net_loss[neg]) else 0)
}

# Where a thin table bites: for rows with net < 0, the pairs of units that both recovered on the
# row without a direct inuring edge between them (either direction). Counted over negative rows.
over_recovery_pairs <- function(res, n_top = 10) {
  neg_rows <- res$rows$row_id[res$rows$net_loss < -1e-9 * pmax(1, res$rows$gross_loss)]
  e <- res$inuring
  ek <- paste(e$from_unit, e$to_unit)
  l <- res$ledger[res$ledger$row_id %in% neg_rows & res$ledger$ceded > 0, c("row_id", "treaty_id", "treaty_name", "treaty_type", "ceded")]
  t <- res$treaties
  l$unit <- t$unit[match(l$treaty_id, t$treaty_id)]
  l <- l |> dplyr::group_by(row_id, unit) |> dplyr::summarise(ceded = sum(ceded), .groups = "drop")
  pr <- lapply(split(l$unit, l$row_id), function(u) if (length(u) > 1) t(utils::combn(sort(u), 2)) else NULL)
  pr <- do.call(rbind, pr)
  if (is.null(pr)) return(tibble::tibble())
  d <- tibble::tibble(a = pr[, 1], b = pr[, 2])
  d$has_edge <- paste(d$a, d$b) %in% ek | paste(d$b, d$a) %in% ek
  d <- d[!d$has_edge, ] |> dplyr::count(a, b, name = "n_negative_rows", sort = TRUE)
  if (is.finite(n_top)) d <- utils::head(d, n_top)
  ut <- res$units
  mi <- function(u) match(u, ut$unit)
  d |> dplyr::mutate(name_a = ut$name[mi(a)], type_a = ut$treaty_type[mi(a)], name_b = ut$name[mi(b)], type_b = ut$treaty_type[mi(b)],
                     a = ut$anchor[mi(a)], b = ut$anchor[mi(b)]) |>
    dplyr::select(a, b, name_a, type_a, name_b, type_b, n_negative_rows)
}

# The outcome check: rows where the treaties together ceded more than the gross loss (net < 0), the events
# they belong to and the treaty pairs (no edge between them) that both recovered on those rows.
outcome_violations <- function(res, n_top = 10, tol = 1e-9) {
  r <- res$rows
  neg <- r$net_loss < -tol * pmax(1, r$gross_loss)
  rows <- tibble::tibble(row_id = r$row_id[neg], event_id = r$event_id[neg], peril = r$peril[neg], class_id = r$class_id[neg],
                         risk_id = r$risk_id[neg], gross_loss = r$gross_loss[neg], ceded_total = r$gross_loss[neg] - r$net_loss[neg],
                         over_amount = -r$net_loss[neg])
  events <- rows |> dplyr::group_by(event_id, peril) |> dplyr::summarise(n_rows = dplyr::n(), over_amount = sum(over_amount), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(over_amount))
  list(valid = !any(neg), summary = over_recovery(res), rows = rows[order(-rows$over_amount), ], events = events,
       pairs = if (any(neg)) over_recovery_pairs(res, n_top) else tibble::tibble())
}

# ---- Summaries (dispatch: old cell-based results go to the previous engine's functions) --------------------------------
is_class_run <- function(res) inherits(res, "class_run")

gross_by <- function(res, ...) {
  if (!is_class_run(res)) return(.old_engine$gross_by(res, ...))
  res$rows |> dplyr::group_by(...) |> dplyr::summarise(gross = sum(gross_loss), .groups = "drop")
}
net_by <- function(res, ...) {
  if (!is_class_run(res)) return(.old_engine$net_by(res, ...))
  res$rows |> dplyr::group_by(...) |> dplyr::summarise(net = sum(net_loss), .groups = "drop")
}
recoveries_by <- function(res, ..., indemnity = TRUE) {
  if (!is_class_run(res)) return(.old_engine$recoveries_by(res, ..., indemnity = indemnity))
  res$ledger |> dplyr::group_by(...) |> dplyr::summarise(ceded = sum(ceded), .groups = "drop")
}
# gross, net, ceded side by side over shared grouping columns.
class_summary <- function(res, ...) {
  keys <- vapply(rlang::enquos(...), rlang::as_name, "")
  gross_by(res, ...) |> dplyr::left_join(net_by(res, ...), by = keys) |> dplyr::left_join(recoveries_by(res, ...), by = keys) |>
    dplyr::mutate(ceded = dplyr::coalesce(ceded, 0), net_gross = net / gross)
}

# Exhaustion: ceded summed at the right level BEFORE comparing with the limit (relative tolerance 1e-9).
#   level "risk"   xol_per_risk: one risk's recovery vs the layer limit
#   level "event"  xol_per_occurrence, clash: event recovery vs limit
#   level "period" aggregate_xl, stop_loss: period recovery vs limit
#   level "cap"    xol_per_risk / occurrence / clash: period recovery vs limit x (1 + reinstatements)
exhaustion <- function(res, data = NULL, tol = 1e-9) {
  if (!is_class_run(res)) return(.old_engine$exhaustion(res, data, tol))
  l <- res$ledger
  lvl <- function(d, keys, level, mult) {
    d |> dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "treaty_id", "treaty_name", "treaty_type", "limit", "reinstatements")))) |>
      dplyr::summarise(ceded = sum(ceded), .groups = "drop") |>
      dplyr::mutate(level = level, bound = limit * (if (mult) dplyr::coalesce(reinstatements, 0) + 1 else 1),
                    exhausted = ceded >= bound * (1 - tol))
  }
  dplyr::bind_rows(
    lvl(l[l$treaty_type == "xol_per_risk", ], "risk_id", "risk", FALSE),
    lvl(l[l$treaty_type %in% c("xol_per_occurrence", "clash"), ], "event_id", "event", FALSE),
    lvl(l[l$treaty_type %in% c("aggregate_xl", "stop_loss"), ], "period_id", "period", FALSE),
    lvl(l[l$treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash"), ], "period_id", "cap", TRUE))
}
exhaustion_summary <- function(ex) ex |> dplyr::group_by(treaty_type, level) |>
  dplyr::summarise(n = dplyr::n(), n_exhausted = sum(exhausted), .groups = "drop")

# Per event: how many classes, risks, treaties see it and pay on it.
treaty_reach <- function(res) {
  l <- res$ledger
  seen <- l |> dplyr::group_by(event_id) |>
    dplyr::summarise(n_treaties_seeing = dplyr::n_distinct(treaty_id), n_classes_covered = dplyr::n_distinct(class_id), .groups = "drop")
  paid <- l[l$ceded > 0, ] |> dplyr::group_by(event_id) |>
    dplyr::summarise(n_treaties_paying = dplyr::n_distinct(treaty_id), .groups = "drop")
  res$events |> dplyr::left_join(seen, by = "event_id") |> dplyr::left_join(paid, by = "event_id") |>
    dplyr::mutate(dplyr::across(c(n_treaties_seeing, n_classes_covered, n_treaties_paying), ~ dplyr::coalesce(.x, 0L)))
}

# class x treaty_type: number of covering treaties (from the bridge), or total ceded when `res` is given.
class_treaty_matrix <- function(bridge, treaty_df, class_table, res = NULL) {
  if (is.null(res)) {
    d <- bridge |> dplyr::left_join(treaty_df[c("treaty_id", "treaty_type")], by = "treaty_id") |>
      dplyr::count(class_id, treaty_type)
  } else d <- res$ledger |> dplyr::group_by(class_id, treaty_type) |> dplyr::summarise(n = sum(ceded), .groups = "drop")
  d |> tidyr::pivot_wider(names_from = treaty_type, values_from = n, values_fill = 0) |>
    dplyr::right_join(class_table["class_id"], by = "class_id") |>
    dplyr::mutate(dplyr::across(-class_id, ~ dplyr::coalesce(.x, 0)))
}

# Example events for slides: for each, the class rows (n risks, gross, ceded, net) and the treaties that saw it.
loss_anatomy2 <- function(res) {
  ev <- res$events
  pe <- ev[ev$kind == "peril", ]; bg <- ev[ev$kind == "background", ]
  sc <- pe[pe$n_classes == 1, ]
  pick <- c(largest = pe$event_id[which.max(pe$gross)],
            widest = (pe[order(-pe$n_classes, -pe$gross), ])$event_id[1],
            single_class = (sc[order(-sc$n_risks), ])$event_id[1],
            background = (bg[order(bg$gross), ])$event_id[nrow(bg) %/% 2])
  nrow_all <- nrow(res$rows)
  one <- function(id) {
    rows <- res$rows[res$rows$event_id == id, ]
    l <- res$ledger[res$ledger$event_id == id, ]
    ced_row <- gsum(l$ceded, l$row_id, nrow_all)[rows$row_id]
    classes <- rows |> dplyr::mutate(ceded = ced_row) |> dplyr::group_by(class_id, region, lob_id) |>
      dplyr::summarise(n_risks = dplyr::n(), gross = sum(gross_loss), largest_risk_share = max(gross_loss) / sum(gross_loss),
                       ceded = sum(ceded), net = sum(net_loss), .groups = "drop") |> dplyr::arrange(dplyr::desc(gross))
    treaties <- l |> dplyr::group_by(treaty_id, treaty_name, treaty_type, layer_index) |>
      dplyr::summarise(n_classes = dplyr::n_distinct(class_id), n_risks = dplyr::n(), subject_loss_in = sum(subject_loss_in),
                       ceded = sum(ceded), .groups = "drop") |>
      dplyr::mutate(ceded_share = ifelse(subject_loss_in > 0, ceded / subject_loss_in, NA_real_)) |> dplyr::arrange(dplyr::desc(ceded))
    list(event = ev[ev$event_id == id, ], classes = classes, treaties_seen = treaties,
         n_treaties_seeing = nrow(treaties), n_treaties_paying = sum(treaties$ceded > 0))
  }
  list(example_events = pick, examples = lapply(pick, one))
}

# ---- Comparing inuring tables and orders --------------------------------------------------------------------------------
# Compare two runs' ledgers on the union of (row, treaty) keys.
compare_runs <- function(a, b, tol = 1e-9) {
  key <- function(l) l$row_id * 1000 + l$treaty_idx
  k <- union(key(a$ledger), key(b$ledger))
  ca <- a$ledger$ceded[match(k, key(a$ledger))]; cb <- b$ledger$ceded[match(k, key(b$ledger))]
  ca[is.na(ca)] <- 0; cb[is.na(cb)] <- 0
  d <- abs(ca - cb)
  tr <- function(x) x$ledger |> dplyr::group_by(treaty_id) |> dplyr::summarise(c = sum(ceded), .groups = "drop")
  tt <- dplyr::full_join(tr(a), tr(b), by = "treaty_id", suffix = c("_a", "_b")) |>
    dplyr::mutate(dplyr::across(c(c_a, c_b), ~ dplyr::coalesce(.x, 0)))
  np <- dplyr::full_join(net_by(a, period_id), net_by(b, period_id), by = "period_id", suffix = c("_a", "_b"))
  list(max_abs_row_diff = max(d), frac_rows_differ = mean(d > tol * pmax(1, abs(ca))),
       max_abs_treaty_ceded_diff = max(abs(tt$c_a - tt$c_b)), max_abs_net_period_diff = max(abs(np$net_a - np$net_b)))
}

# Engine under the rank order and n_random random valid topological orders of `inuring`, each compared
# with the igraph topological order. If the table is explicit, every valid order must give the same ledger.
order_sensitivity <- function(world, inuring = world$inuring, n_random = 20, seed = 1) {
  e <- normalise_inuring(inuring, world$data)
  base <- run_inuring_engine(world, e, on_invalid = "ignore")
  orders <- c(list(rank = NULL), stats::setNames(lapply(seq_len(n_random), function(i) random_topo_order(e, world$data, seed * 1000 + i)),
                                                 sprintf("random_%02d", seq_len(n_random))))
  orders$rank <- {  # chain_rank order is a valid order only when the table is forward in rank
    ro <- rank_order(world$data)
    if (all(match(e$from_unit, ro) < match(e$to_unit, ro))) ro else NULL
  }
  orders <- orders[!vapply(orders, is.null, TRUE)]
  tab <- dplyr::bind_rows(lapply(orders, function(o) tibble::as_tibble_row(compare_runs(base, run_inuring_engine(world, e, o, on_invalid = "ignore")))), .id = "order")
  list(table = tab, base = base,
       worst = tab |> dplyr::summarise(dplyr::across(-order, max)))
}

# Contrast two inuring tables on the same losses: over-recovery under each and ceded by treaty type.
contrast_inuring <- function(world, a, b, labels = c("a", "b")) {
  ra <- run_inuring_engine(world, a, on_invalid = "ignore"); rb <- run_inuring_engine(world, b, on_invalid = "ignore")
  bt <- function(r, lab) r$ledger |> dplyr::group_by(treaty_type) |> dplyr::summarise(ceded = sum(ceded), .groups = "drop") |> dplyr::mutate(table = lab)
  by_type <- dplyr::bind_rows(bt(ra, labels[1]), bt(rb, labels[2])) |>
    tidyr::pivot_wider(names_from = table, values_from = ceded, values_fill = 0) |>
    dplyr::mutate(difference = .data[[labels[2]]] - .data[[labels[1]]])
  list(over_recovery = dplyr::bind_rows(dplyr::mutate(over_recovery(ra), table = labels[1]), dplyr::mutate(over_recovery(rb), table = labels[2])),
       ceded_by_type = by_type,
       total = tibble::tibble(table = labels, gross = c(sum(ra$rows$gross_loss), sum(rb$rows$gross_loss)),
                              ceded = c(sum(ra$ledger$ceded), sum(rb$ledger$ceded)), net = c(sum(ra$rows$net_loss), sum(rb$rows$net_loss))),
       runs = stats::setNames(list(ra, rb), labels))
}

# ---- Invariants ----------------------------------------------------------------------------------------------------------------
# The checks of check_classes.R as a function (also used by the slides): one row per check, with pass and detail.
# `order_check` is an order_sensitivity() result (computed here if NULL).
engine_invariants <- function(world, res, order_check = NULL, tol = 1e-9) {
  l <- res$ledger; r <- res$rows
  ced_row <- gsum(l$ceded, l$row_id, nrow(r))
  ex <- exhaustion(res)
  sig <- function(x) { f <- tempfile(fileext = ".bin"); on.exit(unlink(f))
    writeBin(serialize(x$ledger[c("row_id", "treaty_idx", "subject_loss_in", "ceded")], NULL, version = 3), f); unname(tools::md5sum(f)) }
  w2 <- class_world(world$class_seed, world$sim_seed, world$n_periods)
  if (is.null(order_check)) order_check <- order_sensitivity(world, world$inuring, n_random = 20)
  ow <- order_check$worst
  tibble::tribble(
    ~check, ~pass, ~detail,
    "Gross = ceded + net on every row", all(abs(r$gross_loss - ced_row - r$net_loss) <= tol * pmax(1, r$gross_loss)), sprintf("%d rows", nrow(r)),
    "Net >= 0 on every row", all(r$net_loss >= -tol * pmax(1, r$gross_loss)), sprintf("min net %.0f", min(r$net_loss)),
    "Ceded <= subject loss <= gross", all(l$ceded <= l$subject_loss_in * (1 + tol)) && all(l$subject_loss_in <= r$gross_loss[l$row_id] * (1 + tol)),
      sprintf("%d ledger rows", nrow(l)),
    "Limits and reinstatement caps hold", all(ex$ceded <= ex$bound * (1 + tol)), sprintf("%d limit checks", nrow(ex)),
    "Same seeds, same ledger", identical(sig(run_inuring_engine(w2, on_invalid = "ignore")), sig(res)), "md5 of the ledger",
    "Any application order, same ledger", ow$max_abs_row_diff <= 1e-6 && ow$frac_rows_differ == 0,
      sprintf("%d orders; max difference %.1e", nrow(order_check$table), ow$max_abs_row_diff))
}

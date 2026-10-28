# recover_functions.R -- DAG-driven functional recovery engine for the mega reinsurance set.
#
# Sourceable: defines data tables and functions only, no I/O and no library() calls on source.
# Needs dplyr, tibble, tidyr, readr, purrr, igraph (all namespaced below).
#
# Pipeline (every step is a pure function; state is threaded through Reduce, never mutated):
#   load_mega(dir)                 -> data   (list of tibbles, see data_mega/README.md)
#   recovery_units(data)           -> one row per processing unit (towers collapse to one unit)
#   order_kahn() / order_by_topo_sort() / order_chain_rank() -> three candidate application orders
#   recover_losses(data, order)    -> list(ledger, cells, treaty_period, order)
#   gross_by() net_by() recoveries_by() recovery_summary() exhaustion()  -> summaries
#   loss_anatomy(data, res)        -> what a loss row is, and who sees which event
#
# Semantics replicate the generator's lib_flows.R (tower layers share one pre-tower residual,
# pro-rata write-back, per-treaty-period reinstatement cap, annual aggregate/stop-loss applied once
# per period, premium-linked quota-share cap, ILW off-ledger). Deviations from the generator are
# listed in data_mega/README.md ("Deviations from the generator").
# data_synthreins/recompute_functions.R implements the same semantics for the seed-43 set via global
# lookup tables and per-step tibble filters; it is left untouched. The kernels here are a rewrite on
# integer indices so that the whole mega calculation runs in seconds.

# ---- Applicability, as data ---------------------------------------------------------------------
# Which loss classes a treaty type responds to. Verified against lib_flows.R: footprint types are
# dispatched on event type (xol_per_risk -> large, xol_per_occurrence/clash -> cat); declared-scope
# types take every loss class in scope; ilw never touches the ledger (industry-index trigger).
responds <- tibble::tribble(
  ~treaty_type,         ~attritional, ~large, ~cat,
  "quota_share",        TRUE,         TRUE,   TRUE,
  "group_quota_share",  TRUE,         TRUE,   TRUE,
  "surplus",            TRUE,         TRUE,   TRUE,
  "xol_per_risk",       FALSE,        TRUE,   FALSE,
  "xol_per_occurrence", FALSE,        FALSE,  TRUE,
  "clash",              FALSE,        FALSE,  TRUE,
  "aggregate_xl",       TRUE,         TRUE,   TRUE,
  "stop_loss",          TRUE,         TRUE,   TRUE,
  "ilw",                FALSE,        FALSE,  FALSE
) |>
  tidyr::pivot_longer(-treaty_type, names_to = "loss_type", values_to = "responds")

# How each treaty type is applied: where its cells come from (region), how often it fires
# (cadence), which arithmetic it uses (kernel) and which cap state it carries (cap).
treaty_rules <- tibble::tribble(
  ~treaty_type,         ~region,      ~cadence, ~kernel,    ~cap,
  "quota_share",        "declared",   "event",  "flat",     "premium_linked",
  "group_quota_share",  "declared",   "event",  "flat",     "premium_linked",
  "surplus",            "declared",   "event",  "flat",     "none",   # generator: flat cession_pct, no cap
  "xol_per_risk",       "footprint",  "event",  "layered",  "reinstatement",
  "xol_per_occurrence", "footprint",  "event",  "layered",  "reinstatement",
  "clash",              "footprint",  "event",  "layered",  "reinstatement",
  "aggregate_xl",       "declared",   "period", "layered",  "none",
  "stop_loss",          "declared",   "period", "layered",  "none",
  "ilw",                "none",       "event",  "index",    "none"
)

# ---- Loading -------------------------------------------------------------------------------------
# Read every csv of data_mega/ (or any generator --format=csv directory) into a named list.
load_mega <- function(dir = "data_mega") {
  tables <- c("treaty", "edges", "coverage", "event", "loss", "period", "lob", "segment",
              "division", "portfolio", "premium", "flow", "treaty_period")
  paths <- file.path(dir, paste0(tables, "_df.csv"))
  keep <- file.exists(paths)
  stats::setNames(
    lapply(paths[keep], readr::read_csv, show_col_types = FALSE, guess_max = Inf, progress = FALSE),
    paste0(tables[keep], "_df"))
}

# ---- Units and the DAG ---------------------------------------------------------------------------
radix_order <- function(...) order(..., method = "radix")

# One row per processing unit: a tower occupies one slot pinned to its layer-1 rank.
recovery_units <- function(data) {
  t <- data$treaty_df
  t <- t[radix_order(t$tower_id, t$layer_index, t$treaty_id), ]    # layers ascending within a unit
  t$unit_key <- dplyr::coalesce(t$tower_id, t$treaty_id)
  t |>
    dplyr::group_by(unit_key) |>
    dplyr::summarise(treaty_type = dplyr::first(treaty_type), anchor_treaty_id = dplyr::first(treaty_id),
                     chain_rank = dplyr::first(chain_rank), treaty_ids = list(treaty_id),
                     .groups = "drop") |>
    (\(u) u[match(unique(dplyr::coalesce(data$treaty_df$tower_id, data$treaty_df$treaty_id)), u$unit_key), ])()
}

# treaty_id -> unit_key lookup (a tower's layers share one unit).
unit_lookup <- function(units) stats::setNames(rep(units$unit_key, lengths(units$treaty_ids)), unlist(units$treaty_ids))

# The generator's own order: (chain_rank, treaty_id) of the unit's layer-1 treaty.
order_chain_rank <- function(units) units$unit_key[radix_order(units$chain_rank, units$anchor_treaty_id)]

# A treaty-level order becomes a unit order by placing each unit where its first layer appears
# (for Kahn with chain_rank priority that is layer 1, the generator's pinned position).
collapse_order <- function(treaty_order, units) unique(unname(unit_lookup(units)[treaty_order]))

# Kahn's algorithm on the treaty-level inuring DAG (edges_df as given: the edges run between tower
# LAYERS, so contracting towers first would create cycles; on mega there are three 2-cycles).
# Among treaties whose parents are all placed, the next is the lowest (chain_rank, treaty_id);
# isolated treaties are always available. Stops on a cycle.
order_kahn <- function(data, units = recovery_units(data)) {
  t <- data$treaty_df[radix_order(data$treaty_df$chain_rank, data$treaty_df$treaty_id), ]   # index == priority
  n <- nrow(t)
  from <- match(data$edges_df$from_treaty_id, t$treaty_id); to <- match(data$edges_df$to_treaty_id, t$treaty_id)
  step <- function(st, i) {
    avail <- which(st$indeg == 0L & !st$done)
    if (!length(avail)) stop("order_kahn(): the inuring graph has a cycle")
    pick <- avail[1]
    list(done = replace(st$done, pick, TRUE),
         indeg = st$indeg - tabulate(to[from == pick], n), order = c(st$order, pick))
  }
  fin <- Reduce(step, seq_len(n), list(done = rep(FALSE, n), indeg = tabulate(to, n), order = integer(0)))
  collapse_order(t$treaty_id[fin$order], units)
}

# Plain igraph topological sort (no tie-break rule): vertices in treaty_df order.
order_by_topo_sort <- function(data, units = recovery_units(data)) {
  g <- igraph::graph_from_data_frame(data$edges_df[c("from_treaty_id", "to_treaty_id")], directed = TRUE,
                                     vertices = data.frame(name = data$treaty_df$treaty_id))
  stopifnot(igraph::is_dag(g))
  collapse_order(names(igraph::topo_sort(g, mode = "out")), units)
}

# Edges of the layer-level DAG that a unit order breaks (parent's unit placed after child's unit).
# Atomic tower placement can break a few: an outside treaty may sit between two layers of a tower.
edge_violations <- function(data, order, units = recovery_units(data)) {
  ul <- unit_lookup(units)
  data$edges_df |> dplyr::select(from_treaty_id, to_treaty_id) |>
    dplyr::mutate(from_unit = unname(ul[from_treaty_id]), to_unit = unname(ul[to_treaty_id])) |>
    dplyr::filter(from_unit != to_unit, match(from_unit, order) > match(to_unit, order))
}

# ---- Plan: integer-indexed lookups built once ----------------------------------------------------
recovery_plan <- function(data) {
  periods <- data$period_df$period_id
  ev <- data$event_df
  ev <- ev[radix_order(match(ev$period_id, periods), ev$event_seq, ev$event_id), ]   # fold order
  lob_map <- data$lob_df |>
    dplyr::left_join(data$segment_df |> dplyr::select(segment_id, division_id), by = "segment_id") |>
    dplyr::left_join(data$division_df |> dplyr::select(division_id, portfolio_id), by = "division_id") |>
    dplyr::select(lob_id, segment_id, portfolio_id, lob_group)
  cells <- data$loss_df |>
    dplyr::mutate(event_idx = match(event_id, ev$event_id), period_idx = match(period_id, periods)) |>
    dplyr::left_join(lob_map |> dplyr::select(lob_id, lob_group), by = "lob_id")
  idx <- seq_len(nrow(cells))
  by_event <- split(idx[radix_order(cells$lob_id)], factor(cells$event_idx[radix_order(cells$lob_id)], seq_len(nrow(ev))))
  by_period <- split(idx[radix_order(cells$event_id, cells$lob_id)],
                     factor(cells$period_idx[radix_order(cells$event_id, cells$lob_id)], seq_along(periods)))
  list(periods = periods, events = ev |> dplyr::mutate(period_idx = match(period_id, periods)),
       cells = cells, lob_map = lob_map, event_cells = unname(by_event), period_cells = unname(by_period))
}

# ---- Unit specifications --------------------------------------------------------------------------
# Which loss cells a treaty can see: its type responds to the cell's loss class AND the cell's
# line of business is in its region.
cell_visible <- function(treaty_type, region, cells) {
  classes <- responds$loss_type[responds$responds & responds$treaty_type == treaty_type]
  cells$loss_type %in% classes & cells$lob_id %in% region
}

# Everything a step needs about one unit, as plain vectors: layer terms, the cap state kind, and
# cell_ok -- a logical per loss cell (loss class responds AND the cell is in the unit's region).
unit_spec <- function(unit, data, plan) {
  t <- data$treaty_df[match(unit$treaty_ids[[1]], data$treaty_df$treaty_id), ]
  rule <- treaty_rules[match(unit$treaty_type, treaty_rules$treaty_type), ]
  region <- switch(rule$region,
    footprint = data$coverage_df$lob_id[data$coverage_df$treaty_id == t$treaty_id[1]],
    declared = switch(t$scope_type[1], lob = t$scope_id[1],
                      segment = plan$lob_map$lob_id[plan$lob_map$segment_id == t$scope_id[1]],
                      group = plan$lob_map$lob_id[plan$lob_map$portfolio_id == t$scope_id[1]]),
    none = character(0))
  cap <- if (rule$cap == "premium_linked" && !isTRUE(t$premium_linked_limit[1])) "none" else rule$cap
  list(key = unit$unit_key, type = unit$treaty_type, kernel = rule$kernel, cadence = rule$cadence, cap = cap,
       tidx = match(t$treaty_id, data$treaty_df$treaty_id), attach = t$attachment, limit = t$limit,
       reinst = t$reinstatements, cession = t$cession_pct[1], trigger = t$industry_trigger[1],
       cell_ok = cell_visible(unit$treaty_type, region, plan$cells))
}

# ---- Kernels: (r, u, used) -> list(mat = n_cells x n_layers ceded, used = new cumulative per layer) ---
# layered: every layer sees the same aggregated residual R (towers do not erode); recovery
# min(max(R - attach, 0), limit), capped by the remaining (reinstatements + 1) x limit for the
# treaty-period (event cadence, cap "reinstatement") or uncapped (annual cadence); written back
# to cells pro rata to their residual.
kernel_layered <- function(r, u, used) {
  R <- sum(r)
  theo <- pmin(pmax(R - u$attach, 0), u$limit)
  rem <- if (u$cap == "reinstatement") pmax(0, (u$reinst + 1) * u$limit - used) else Inf
  act <- pmin(theo, rem)
  list(mat = outer(if (R > 0) r / R else rep(0, length(r)), act), used = used + act)
}

# flat: cession_pct x residual per cell; a premium-linked cap (quota share only) is a running
# total over cells and events within the period, checked cell by cell (so folded, not vectorised).
kernel_flat <- function(r, u, used) {
  theo <- u$cession * r
  if (u$cap != "premium_linked") return(list(mat = matrix(theo, ncol = 1), used = used + sum(theo)))
  fin <- Reduce(function(acc, th) {
    a <- min(th, max(0, u$limit - acc$used))
    list(used = acc$used + a, act = c(acc$act, a))
  }, theo, list(used = used, act = numeric(0)))
  list(mat = matrix(fin$act, ncol = 1), used = fin$used)
}

kernels <- list(layered = kernel_layered, flat = kernel_flat)

# ---- Steps: (state, ...) -> list(state, rows). State = list(ledger, cap); rows = list of chunks -----
# state$ledger: residual per loss cell; state$cap: cumulative ceded, treaty x period (matrix).
row_chunk <- function(cell, event, treaty, subject, ceded, reinst)
  list(cell = cell, event = event, treaty = treaty, subject = subject, ceded = ceded, reinst = reinst)

# One unit on the cells `ci` of one event (cadence event) or one period (cadence period).
apply_unit <- function(acc, u, ci, p, e, plan) {
  if (u$kernel == "index") {                      # ILW: off-ledger, every cat event, index trigger
    if (plan$events$event_type[e] != "cat") return(acc)
    il <- plan$events$industry_loss[e]
    ced <- if (!is.na(il) && il >= u$trigger) u$limit else 0
    acc$rows <- c(acc$rows, list(row_chunk(NA_integer_, e, u$tidx, NA_real_, ced, NA_real_)))
    return(acc)
  }
  ci <- ci[u$cell_ok[ci]]
  if (!length(ci)) return(acc)
  r <- acc$state$ledger[ci]
  k <- kernels[[u$kernel]](r, u, acc$state$cap[u$tidx, p])
  n <- length(ci); L <- length(u$tidx)
  ced <- as.vector(k$mat)
  lim <- rep(u$limit, each = n)
  acc$state$ledger[ci] <- r - rowSums(k$mat)
  acc$state$cap[u$tidx, p] <- k$used
  acc$rows <- c(acc$rows, list(row_chunk(rep(ci, times = L), plan$cells$event_idx[rep(ci, times = L)],
    rep(u$tidx, each = n), rep(r, times = L), ced,
    if (u$cap == "reinstatement") ifelse(lim > 0, ced / lim, 0) else rep(NA_real_, n * L))))
  acc
}

# Step for item i of a stage (an event index for event stages, a period index for period stages):
# apply the stage's units in DAG order.
step_item <- function(prev, i, stage, plan) {
  ev <- stage$cadence == "event"
  ci <- if (ev) plan$event_cells[[i]] else plan$period_cells[[i]]
  p <- if (ev) plan$events$period_idx[i] else i
  Reduce(function(acc, u) apply_unit(acc, u, ci, p, i, plan), stage$units,
         list(state = prev$state, rows = list()))
}

# Fold one stage over its items; rows from every step are collected, only the final state is kept.
run_stage <- function(state, stage, plan) {
  items <- if (stage$cadence == "event") seq_len(nrow(plan$events)) else seq_along(plan$periods)
  steps <- Reduce(function(prev, i) step_item(prev, i, stage, plan), items,
                  list(state = state, rows = list()), accumulate = TRUE)
  list(state = steps[[length(steps)]]$state,
       rows = unlist(lapply(steps[-1], `[[`, "rows"), recursive = FALSE))
}

# Consecutive event-cadence units form one stage (folded event by event); each period-cadence
# (annual) unit run is its own stage, because it acts on the whole-period residual at its rank.
build_stages <- function(specs) {
  cad <- ifelse(vapply(specs, `[[`, "", "cadence") == "period", "period", "event")
  run <- rep(seq_along(rle(cad)$lengths), rle(cad)$lengths)
  lapply(split(specs, run), function(s) list(cadence = s[[1]]$cadence, units = unname(s)))
}

# ---- The recovery -----------------------------------------------------------------------------------
# order: character vector of unit keys in application order (default: Kahn on the DAG).
recover_losses <- function(data, order = NULL) {
  units <- recovery_units(data)
  if (is.null(order)) order <- order_kahn(data, units)
  plan <- recovery_plan(data)
  specs <- lapply(order, \(k) unit_spec(units[units$unit_key == k, ], data, plan))
  state0 <- list(ledger = plan$cells$gross_loss,
                 cap = matrix(0, nrow(data$treaty_df), length(plan$periods)))
  fin <- Reduce(function(acc, stage) {
    s <- run_stage(acc$state, stage, plan)
    list(state = s$state, rows = c(acc$rows, s$rows))
  }, build_stages(specs), list(state = state0, rows = list()))
  col <- function(nm) unlist(lapply(fin$rows, `[[`, nm), use.names = FALSE)
  rows <- tibble::tibble(cell = col("cell"), event = col("event"), treaty = col("treaty"),
                         subject_loss_in = col("subject"), ceded = col("ceded"), reinstatement_consumed = col("reinst"))
  t <- data$treaty_df
  rank_of <- units$chain_rank[match(dplyr::coalesce(t$tower_id, t$treaty_id), units$unit_key)]  # tower-pinned
  ledger <- tibble::tibble(
    period_id = plan$events$period_id[rows$event], event_id = plan$events$event_id[rows$event],
    treaty_id = t$treaty_id[rows$treaty], treaty_type = t$treaty_type[rows$treaty],
    chain_rank = rank_of[rows$treaty], lob_id = plan$cells$lob_id[rows$cell],
    lob_group = plan$cells$lob_group[rows$cell],
    loss_type = ifelse(is.na(rows$cell), "cat", plan$cells$loss_type[rows$cell]),
    is_indemnity = t$treaty_type[rows$treaty] != "ilw", subject_loss_in = rows$subject_loss_in,
    ceded = rows$ceded, retained_after = rows$subject_loss_in - rows$ceded,
    reinstatement_consumed = rows$reinstatement_consumed)
  ledger <- ledger[radix_order(ledger$period_id, ledger$event_id, ledger$lob_id, ledger$loss_type, ledger$treaty_id), ]
  list(ledger = ledger, cells = dplyr::mutate(plan$cells, net_loss = fin$state$ledger),
       treaty_period = treaty_period_summary(data, fin$state$cap, plan), order = order)
}

# Per (treaty, period): reinstatements used, aggregate consumed (annual types and premium-linked QS),
# reinstatement premium = rol x net_subject_premium x max(0, used - 1). Same definitions as treaty_period_df.
treaty_period_summary <- function(data, cap, plan) {
  t <- data$treaty_df
  g <- tidyr::expand_grid(treaty_idx = seq_len(nrow(t)), period_idx = seq_along(plan$periods))
  tt <- t[g$treaty_idx, ]
  cap_kind <- treaty_rules$cap[match(tt$treaty_type, treaty_rules$treaty_type)]
  cad <- treaty_rules$cadence[match(tt$treaty_type, treaty_rules$treaty_type)]
  used <- cap[cbind(g$treaty_idx, g$period_idx)]
  nsp <- data$premium_df$net_subject_premium[match(tt$treaty_id, data$premium_df$treaty_id)]
  reinst <- ifelse(cap_kind == "reinstatement" & tt$limit > 0, used / tt$limit, NA_real_)
  tibble::tibble(treaty_id = tt$treaty_id, period_id = plan$periods[g$period_idx],
                 reinstatements_used = reinst,
                 reinstatement_premium = ifelse(is.na(reinst), NA_real_, tt$rol * nsp * pmax(0, reinst - 1)),
                 aggregate_consumed = ifelse(cad == "period" | (cap_kind == "premium_linked" & dplyr::coalesce(tt$premium_linked_limit, FALSE)),
                                             used, NA_real_)) |>
    (\(d) d[radix_order(d$treaty_id, d$period_id), ])()
}

# ---- Order checks -----------------------------------------------------------------------------------
# Does the Kahn order equal the (chain_rank, treaty_id) order, and how much would plain topo_sort change?
order_agrees <- function(data, tol = 1e-9) {
  units <- recovery_units(data)
  kahn <- order_kahn(data, units); crank <- order_chain_rank(units); topo <- order_by_topo_sort(data, units)
  base <- recover_losses(data, kahn); alt <- recover_losses(data, topo)
  m <- dplyr::inner_join(base$ledger, alt$ledger,
         by = c("event_id", "treaty_id", "lob_id", "loss_type"), suffix = c("", "_alt"))
  differs <- abs(m$ceded - m$ceded_alt) > tol * pmax(1, abs(m$ceded))
  ev_diff <- m |> dplyr::mutate(differs = differs) |> dplyr::group_by(event_id) |>
    dplyr::summarise(d = any(differs), .groups = "drop")
  list(kahn_equals_chain_rank = identical(kahn, crank), n_units = length(kahn),
       n_position_differences_kahn_vs_chain_rank = sum(kahn != crank),
       n_position_differences_topo_vs_kahn = sum(topo != kahn),
       n_edges = nrow(data$edges_df), edge_violations_kahn = nrow(edge_violations(data, kahn, units)),
       edge_violations_topo_sort = nrow(edge_violations(data, topo, units)),
       frac_rows_differ_under_topo_sort = mean(differs), frac_events_differ_under_topo_sort = mean(ev_diff$d),
       n_rows = nrow(m), n_events = nrow(ev_diff))
}

# ---- Summaries ---------------------------------------------------------------------------------------
# Group by any of: period_id, loss_type, lob_id, lob_group, event_id (gross, net); plus treaty_type,
# treaty_id (recoveries). ILW rows are non-indemnity and excluded from recoveries unless indemnity = FALSE.
gross_by <- function(res, ...) res$cells |> dplyr::group_by(...) |> dplyr::summarise(gross = sum(gross_loss), .groups = "drop")
net_by <- function(res, ...) res$cells |> dplyr::group_by(...) |> dplyr::summarise(net = sum(net_loss), .groups = "drop")
recoveries_by <- function(res, ..., indemnity = TRUE)
  res$ledger |> dplyr::filter(is_indemnity == indemnity) |> dplyr::group_by(...) |>
    dplyr::summarise(ceded = sum(ceded), .groups = "drop")

# gross, net, ceded side by side over shared grouping columns; ratio = net / gross.
recovery_summary <- function(res, ...) {
  keys <- vapply(rlang::enquos(...), rlang::as_name, "")
  g <- gross_by(res, ...); n <- net_by(res, ...); r <- recoveries_by(res, ...)
  out <- g |> dplyr::left_join(n, by = keys) |> dplyr::left_join(r, by = keys)
  dplyr::mutate(out, ceded = dplyr::coalesce(ceded, 0), net_gross = net / gross)
}

# Exhaustion: ceded is summed by (event, treaty) -- or (period, treaty) for annual and premium-linked
# covers -- BEFORE comparing with the layer limit, because one layer's recovery can be split across
# loss cells and a row-level test under-detects it. Relative tolerance 1e-9.
exhaustion <- function(res, data, tol = 1e-9) {
  t <- data$treaty_df
  lvl <- function(ledger, keys, level)
    ledger |> dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "treaty_id", "treaty_type")))) |>
      dplyr::summarise(ceded = sum(ceded), .groups = "drop") |>
      dplyr::mutate(level = level, limit = t$limit[match(treaty_id, t$treaty_id)],
                    exhausted = ceded >= limit * (1 - tol))
  ledg <- dplyr::filter(res$ledger, is_indemnity)
  rule <- treaty_rules[match(ledg$treaty_type, treaty_rules$treaty_type), ]
  pll <- rule$cap == "premium_linked" & dplyr::coalesce(t$premium_linked_limit[match(ledg$treaty_id, t$treaty_id)], FALSE)
  dplyr::bind_rows(
    lvl(ledg[rule$cadence == "event" & rule$cap == "reinstatement", ], "event_id", "event"),
    lvl(ledg[rule$cadence == "period" | pll, ], "period_id", "period"))
}

# ---- Reconciliation with the generator ---------------------------------------------------------------
# Relative difference of two numbers (NA only against NA counts as equal); scaled by max(|a|, |b|, 1).
rel_diff <- function(a, b) ifelse(is.na(a) & is.na(b), 0, abs(a - b) / pmax(abs(a), abs(b), 1, na.rm = TRUE))

# A run against the generator's flow_df, row by row on (event, treaty, LOB, loss type). Per matched row:
# relative differences in ceded, retained_after, subject_loss_in and reinstatement_consumed; `ceded_ok`
# is the ceded test alone, `ok` all four (and NA in the same places). Returns the row table and counts.
compare_flows <- function(res, data, tol = 1e-9) {
  key <- c("event_id", "treaty_id", "lob_id", "loss_type")
  j <- dplyr::full_join(res$ledger, data$flow_df, by = key, suffix = c("", ".gen"))
  rows <- j |> dplyr::filter(!is.na(period_id), !is.na(period_id.gen)) |>
    dplyr::mutate(d_ceded = rel_diff(ceded, ceded.gen), d_ret = rel_diff(retained_after, retained_after.gen),
                  d_subj = rel_diff(subject_loss_in, subject_loss_in.gen),
                  d_reinst = rel_diff(reinstatement_consumed, reinstatement_consumed.gen),
                  abs_ceded = abs(ceded - ceded.gen),
                  na_mismatch = xor(is.na(reinstatement_consumed), is.na(reinstatement_consumed.gen)),
                  ceded_ok = d_ceded <= tol,
                  ok = d_ceded <= tol & d_ret <= tol & d_subj <= tol & d_reinst <= tol & !na_mismatch)
  list(rows = rows, n_engine = nrow(res$ledger), n_generator = nrow(data$flow_df), n_matched_keys = nrow(rows),
       n_only_engine = sum(is.na(j$period_id.gen)), n_only_generator = sum(is.na(j$period_id)),
       n_ok = sum(rows$ok), n_ceded_ok = sum(rows$ceded_ok))
}

# ---- Loss anatomy ------------------------------------------------------------------------------------
# What a loss row is and who sees which event. A loss_df row is one (event, LOB, loss_type) cell with a
# gross_loss. loss_type == event_type; attritional rows are annual per-(period, LOB) aggregates, large
# events have one LOB, only cat events span several LOBs. Treaties see cells, not events: a treaty gets
# the cells of an event that are in its loss-class set (responds) and in its region (coverage / scope).
loss_anatomy <- function(data, res = recover_losses(data)) {
  ls <- data$loss_df; ev <- data$event_df; t <- data$treaty_df
  per_event <- ls |> dplyr::group_by(event_id) |>
    dplyr::summarise(loss_type = dplyr::first(loss_type), n_loss_types = dplyr::n_distinct(loss_type),
                     n_rows = dplyr::n(), gross = sum(gross_loss), .groups = "drop") |>
    dplyr::left_join(ev |> dplyr::select(event_id, period_id, event_type), by = "event_id")
  rows_per_event <- per_event |>
    dplyr::mutate(rows = dplyr::case_when(n_rows == 1 ~ "1", n_rows == 2 ~ "2", TRUE ~ "3+")) |>
    dplyr::count(loss_type, rows) |> tidyr::pivot_wider(names_from = rows, values_from = n, values_fill = 0L)
  lobs_per_cat_event <- per_event |> dplyr::filter(loss_type == "cat") |> dplyr::count(n_rows, name = "events")
  # fixed rule for the examples: largest attritional cell, largest large event, cat event with most LOB rows (ties: gross)
  pick <- c(attritional = per_event$event_id[per_event$loss_type == "attritional"][which.max(per_event$gross[per_event$loss_type == "attritional"])],
            large = per_event$event_id[per_event$loss_type == "large"][which.max(per_event$gross[per_event$loss_type == "large"])],
            cat = (per_event |> dplyr::filter(loss_type == "cat") |> dplyr::arrange(dplyr::desc(n_rows), dplyr::desc(gross)))$event_id[1])
  who <- function(id) {
    res$ledger |> dplyr::filter(event_id == id) |>
      dplyr::group_by(treaty_id, treaty_type, is_indemnity) |>
      dplyr::summarise(n_cells = sum(!is.na(lob_id)), subject_loss_in = sum(subject_loss_in), ceded = sum(ceded), .groups = "drop") |>
      dplyr::mutate(layer_index = t$layer_index[match(treaty_id, t$treaty_id)],
                    attachment = t$attachment[match(treaty_id, t$treaty_id)], limit = t$limit[match(treaty_id, t$treaty_id)],
                    ceded_share = ifelse(subject_loss_in > 0, ceded / subject_loss_in, NA_real_)) |>
      dplyr::arrange(dplyr::desc(ceded))
  }
  list(
    rows_per_event = rows_per_event, lobs_per_cat_event = lobs_per_cat_event,
    events_with_several_loss_types = sum(per_event$n_loss_types > 1),
    loss_type_equals_event_type = all(per_event$loss_type == per_event$event_type | is.na(per_event$event_type)),
    events_without_loss_rows = ev |> dplyr::filter(!event_id %in% ls$event_id) |> dplyr::count(event_type),
    columns = tibble::tibble(table = rep(c("loss_df", "event_df"), c(ncol(ls), ncol(ev))),
                             column = c(names(ls), names(ev)),
                             type = c(vapply(ls, \(x) class(x)[1], ""), vapply(ev, \(x) class(x)[1], ""))),
    example_events = pick,
    example_loss_rows = lapply(pick, \(id) dplyr::filter(ls, event_id == id)),
    example_event_rows = lapply(pick, \(id) dplyr::filter(ev, event_id == id)),
    who_sees_what = lapply(pick, who),
    n_treaties = nrow(t))
}

# recompute_functions.R -- the independent functional recompute of the generator's flow_df.
#
# Sourced by derive.R (and by the talk .qmd); defines functions only, no file I/O.
#
# Pure step functions: state (residual ledger, cap usage, emitted rows) is threaded through
# Reduce(), never mutated in place. Semantics replicate lib_flows.R (see README).
#
# Read-only lookup tables the step functions expect in scope (global environment), all built
# from the generator's raw csv frames by prepare_recompute_inputs():
#   treaty_df, coverage_df, lob_df, period_df   (raw frames, used as read)
#   loss_cells, event_meta, period_events, all_lobs, unit_order   (derived by prepare_recompute_inputs)
# Usage:  list2env(prepare_recompute_inputs(...), globalenv());  final <- recompute_flows()
# Needs dplyr (filter, mutate, arrange, select, tibble, bind_rows, coalesce) and base R.

cell_key <- function(event_id, lob_id, loss_type) paste(event_id, lob_id, loss_type, sep = "::")

# Derive the lookup tables the step functions read (returned, not assigned: the caller decides
# where they live).
prepare_recompute_inputs <- function(treaty_df, lob_df, loss_df, event_df, period_df) {
  loss_cells <- loss_df |>
    mutate(key = cell_key(event_id, lob_id, loss_type)) |>
    arrange(event_id, lob_id, method = "radix")
  event_meta <- event_df |> select(event_id, period_id, event_type, event_seq, industry_loss)
  period_events <- event_meta |> filter(event_id %in% loss_cells$event_id) |>
    mutate(pidx = match(period_id, period_df$period_id)) |>
    arrange(pidx, event_seq, event_id, method = "radix")
  # Processing units: towers occupy one slot at layer-1 rank; ordered by (chain_rank, treaty_id).
  unit_order <- treaty_df |>
    filter(is.na(layer_index) | layer_index == 1L) |>
    mutate(unit_key = coalesce(tower_id, treaty_id)) |>
    arrange(chain_rank, treaty_id, method = "radix")
  list(loss_cells = loss_cells, event_meta = event_meta, period_events = period_events,
       all_lobs = lob_df$lob_id, unit_order = unit_order)
}

get_cap <- function(cap, k) { v <- unname(cap[k]); ifelse(is.na(v), 0, v) }
set_cap <- function(cap, k, v) { cap[k] <- v; cap }          # copy-on-modify: local copy only

# One footprint event (xol_per_risk / xol_per_occurrence / clash tower): every layer sees the
# same pre-unit aggregated residual R; reinstatement cap enforced per layer & period.
step_footprint <- function(state, ev, u) {
  cells <- loss_cells |> filter(event_id == ev$event_id, lob_id %in% u$fp)
  if (nrow(cells) == 0) return(state)
  r <- unname(state$ledger[cells$key]); R <- sum(r)
  L <- u$layers
  ck <- paste0("r|", L$treaty_id, "|", ev$period_id)
  theo <- pmin(pmax(R - L$attachment, 0), L$limit)
  rem  <- pmax(0, (L$reinstatements + 1) * L$limit - get_cap(state$cap, ck))
  act  <- pmin(theo, rem)
  share <- if (R > 0) r / R else rep(0, length(r))
  mat <- outer(share, act)
  rows <- tibble(period_id = ev$period_id, event_id = ev$event_id,
                 treaty_id = rep(L$treaty_id, each = nrow(cells)), lob_id = rep(cells$lob_id, times = nrow(L)),
                 loss_type = rep(cells$loss_type, times = nrow(L)),
                 subject = rep(r, times = nrow(L)), ceded = as.vector(mat))
  state$ledger <- {l <- state$ledger; l[cells$key] <- r - rowSums(mat); l}
  state$cap <- set_cap(state$cap, ck, get_cap(state$cap, ck) + act)
  state$rows <- c(state$rows, list(rows)); state
}

# One period of an annual-aggregate unit: whole-period residual of the declared scope, once.
step_annual <- function(state, pid, u) {
  cells <- loss_cells |> filter(event_id %in% period_events$event_id[period_events$period_id == pid], lob_id %in% u$scope)
  if (nrow(cells) == 0) return(state)
  r <- unname(state$ledger[cells$key]); R <- sum(r); L <- u$layers
  ck <- paste0("a|", L$treaty_id, "|", pid)
  act <- pmin(pmax(R - L$attachment, 0), L$limit)
  share <- if (R > 0) r / R else rep(0, length(r))
  mat <- outer(share, act)
  rows <- tibble(period_id = pid, event_id = rep(cells$event_id, times = nrow(L)),
                 treaty_id = rep(L$treaty_id, each = nrow(cells)), lob_id = rep(cells$lob_id, times = nrow(L)),
                 loss_type = rep(cells$loss_type, times = nrow(L)),
                 subject = rep(r, times = nrow(L)), ceded = as.vector(mat))
  state$ledger <- {l <- state$ledger; l[cells$key] <- r - rowSums(mat); l}
  state$cap <- set_cap(state$cap, ck, act)
  state$rows <- c(state$rows, list(rows)); state
}

# One event for a proportional unit: per cell, flat cession; premium-linked cap (QS only) is a
# running cumulative across cells/events in the period (folded cell by cell).
step_prop <- function(state, ev, u) {
  cells <- loss_cells |> filter(event_id == ev$event_id, lob_id %in% u$scope)
  if (nrow(cells) == 0) return(state)
  ck <- paste0("a|", u$treaty_id, "|", ev$period_id)
  one <- function(acc, key) {
    R <- acc$state$ledger[[key]]
    theo <- u$cession * R
    act <- if (u$pll) min(theo, max(0, u$limit - get_cap(acc$state$cap, ck))) else theo
    acc$state$ledger <- {l <- acc$state$ledger; l[key] <- R - act; l}
    if (u$pll) acc$state$cap <- set_cap(acc$state$cap, ck, get_cap(acc$state$cap, ck) + act)
    acc$rows <- c(acc$rows, list(tibble(key = key, subject = R, ceded = act))); acc
  }
  res <- Reduce(one, cells$key, list(state = state, rows = list()))
  out <- bind_rows(res$rows) |> left_join(cells |> select(key, lob_id, loss_type), by = "key") |>
    transmute(period_id = ev$period_id, event_id = ev$event_id, treaty_id = u$treaty_id, lob_id, loss_type, subject, ceded)
  st <- res$state; st$rows <- c(state$rows, list(out)); st
}

step_ilw <- function(state, u) {
  # all cat events in event_df, including those with no loss_df rows (the generator does the same)
  cats <- event_meta |> filter(event_type == "cat") |>
    mutate(ceded = ifelse(!is.na(industry_loss) & industry_loss >= u$trigger, u$limit, 0))
  state$rows <- c(state$rows, list(tibble(period_id = cats$period_id, event_id = cats$event_id, treaty_id = u$treaty_id,
                                          lob_id = NA_character_, loss_type = "cat", subject = NA_real_, ceded = cats$ceded)))
  state
}

scope_lobs <- function(st, sid) switch(st, lob = sid, segment = lob_df$lob_id[lob_df$segment_id == sid], group = all_lobs)

apply_unit <- function(state, unit_key) {
  m <- treaty_df |> filter(coalesce(tower_id, treaty_id) == unit_key) |> arrange(layer_index, method = "radix")
  type <- m$treaty_type[1]
  if (type == "ilw") return(step_ilw(state, list(treaty_id = m$treaty_id[1], trigger = m$industry_trigger[1], limit = m$limit[1])))
  if (type %in% c("xol_per_risk", "xol_per_occurrence", "clash")) {
    u <- list(fp = coverage_df$lob_id[coverage_df$treaty_id == m$treaty_id[1]], layers = m)
    resp <- if (type == "xol_per_risk") "large" else "cat"
    evs <- period_events |> filter(event_type == resp)
    return(Reduce(function(s, i) step_footprint(s, evs[i, ], u), seq_len(nrow(evs)), state))
  }
  sc <- scope_lobs(m$scope_type[1], m$scope_id[1])
  if (type %in% c("aggregate_xl", "stop_loss"))
    return(Reduce(function(s, pid) step_annual(s, pid, list(scope = sc, layers = m)), period_df$period_id, state))
  u <- list(treaty_id = m$treaty_id[1], scope = sc, cession = m$cession_pct[1], limit = m$limit[1],
            pll = type %in% c("quota_share", "group_quota_share") && isTRUE(m$premium_linked_limit[1]))
  # same event order as the generator: period index, then event_seq, then event_id
  Reduce(function(s, i) step_prop(s, period_events[i, ], u), seq_len(nrow(period_events)), state)
}

# The whole recompute: fold apply_unit over the processing units in rank order.
recompute_flows <- function() {
  state0 <- list(ledger = setNames(loss_cells$gross_loss, loss_cells$key), cap = numeric(0), rows = list())
  Reduce(apply_unit, unit_order$unit_key, state0)
}

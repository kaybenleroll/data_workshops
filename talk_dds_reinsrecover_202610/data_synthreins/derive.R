# derive.R -- rebuild the talk-ready tables from raw synthreins generator output.
#
# Usage (from anywhere; paths are arguments, nothing is read from the cwd):
#   Rscript derive.R <data_synthreins_dir> <mega_example_csv_dir>
# e.g. inside rocker/tidyverse:4.5.1 (needs dplyr, readr, purrr, tidyr, arrow):
#   Rscript derive.R /work/talk_dds_reinsrecover_202610/data_synthreins /mega/csv
#
# Reads   <dir>/raw/phase2_mid_seed43/*.csv       (generator Phase 1 + 2 csv)
#         <mega_csv_dir>/{treaty_df,edges_df,lob_df,segment_df,portfolio_df}.csv
# Writes  <dir>/{treaties,inuring_edges,loss_events,loss_allocation_walkthrough}.csv
#         <dir>/{nodes_tbl,edges_tbl}.{csv,parquet}
#         <dir>/recompute_reconciliation.csv, <dir>/walkthrough_functional.csv (if needed)
#         <dir>/realistic_mega/{nodes_tbl,edges_tbl}.{csv,parquet}, stats.csv
#
# Deterministic: no RNG, all orderings explicit (radix sorts on ids).
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(purrr); library(tidyr); library(arrow)
})

args <- commandArgs(trailingOnly = TRUE)
OUT  <- args[1]
MEGA <- args[2]
RAW  <- file.path(OUT, "raw", "phase2_mid_seed43")
rd   <- function(d, n) read_csv(file.path(d, paste0(n, ".csv")), show_col_types = FALSE, progress = FALSE)

TOL_REL <- 1e-9   # relative tolerance for "match" in the reconciliation and for layer_exhausted

# ---------------------------------------------------------------------------
# 1. Labels, short ids, nodes, edges (shared by the talk run and the mega run)
# ---------------------------------------------------------------------------
PREFIX <- c(quota_share = "QS", group_quota_share = "GQS", surplus = "SUR",
            xol_per_risk = "XR", xol_per_occurrence = "XO", clash = "CLASH",
            aggregate_xl = "AGG", stop_loss = "SL", ilw = "ILW")
TYPE_SHORT <- c(quota_share = "QS", group_quota_share = "Group QS", surplus = "Surplus",
                xol_per_risk = "Per-risk XoL", xol_per_occurrence = "Occ XoL", clash = "Clash",
                aggregate_xl = "Agg XoL", stop_loss = "Stop Loss", ilw = "ILW")

fmt_m <- function(x) ifelse(is.na(x), NA_character_,
  ifelse(x >= 1e7, sprintf("%.0fm", x / 1e6), sprintf("%.1fm", x / 1e6)))

scope_names <- function(treaty_df, lob_df, segment_df, portfolio_df) {
  nm <- c(setNames(sub("\\s*\\(P[0-9]+ [0-9.]+\\)$", "", lob_df$name), lob_df$lob_id),
          setNames(segment_df$name, segment_df$segment_id),
          setNames(rep("Group", nrow(portfolio_df)), portfolio_df$portfolio_id))
  unname(nm[treaty_df$scope_id])
}

# Short, stable ids: <PREFIX><nn> numbered by (chain_rank, generator id) per type; towers
# share one number and each layer gets a _L<k> suffix.
make_treaties <- function(treaty_df, lob_df, segment_df, portfolio_df) {
  t <- treaty_df |>
    mutate(unit_key = coalesce(tower_id, treaty_id)) |>
    group_by(unit_key) |>
    mutate(unit_rank = min(chain_rank), unit_tid = min(treaty_id[chain_rank == min(chain_rank)])) |>
    ungroup()
  units <- t |> distinct(unit_key, treaty_type, unit_rank, unit_tid) |>
    arrange(treaty_type, unit_rank, unit_tid, method = "radix") |>
    group_by(treaty_type) |> mutate(num = row_number()) |> ungroup() |>
    mutate(unit_label = sprintf("%s%02d", PREFIX[treaty_type], num)) |>
    select(unit_key, unit_label)
  t <- t |> left_join(units, by = "unit_key") |>
    mutate(gen_tid = treaty_id,
           short_id = ifelse(is.na(tower_id), unit_label, paste0(unit_label, "_L", layer_index)),
           scope_name = scope_names(pick(everything()), lob_df, segment_df, portfolio_df),
           ts = TYPE_SHORT[treaty_type],
           band = ifelse(is.na(limit), NA_character_, paste0(fmt_m(limit), "XS", fmt_m(attachment))),
           treaty_label = case_when(
             treaty_type %in% c("quota_share", "group_quota_share", "surplus") ~
               sprintf("%s %s %.0f%%", scope_name, ts, 100 * cession_pct),
             treaty_type == "ilw" ~ sprintf("%s ILW %s", scope_name, fmt_m(limit)),
             !is.na(layer_index) ~ sprintf("%s %s %s L%d", scope_name, ts, band, layer_index),
             TRUE ~ sprintf("%s %s %s", scope_name, ts, band)),
           description = case_when(
             treaty_type %in% c("quota_share", "group_quota_share", "surplus") ~
               sprintf("%s on %s (%s scope), %.0f%% ceded", ts, scope_name, scope_type, 100 * cession_pct),
             treaty_type == "ilw" ~ sprintf("ILW on %s: %s limit, industry trigger %.3g", scope_name, fmt_m(limit), industry_trigger),
             !is.na(layer_index) ~ sprintf("Tower %s layer %d of %d: %s xs %s, %d reinstatement(s)", unit_label, layer_index, n_layers,
                                             fmt_m(limit), fmt_m(attachment), coalesce(reinstatements, 0L)),
             treaty_type %in% c("aggregate_xl", "stop_loss") ~ sprintf("%s on %s: %s xs %s (annual aggregate)", ts, scope_name, fmt_m(limit), fmt_m(attachment)),
             TRUE ~ sprintf("%s on %s: %s xs %s, %d reinstatement(s)", ts, scope_name, fmt_m(limit), fmt_m(attachment), coalesce(reinstatements, 0L))))
  t |> transmute(
    treaty_id = short_id, treaty_type, chain_rank = round(chain_rank, 6), need_id = NA_character_,
    scope = scope_id, attachment, limit, cession = cession_pct, layer = layer_index, rol, reinstatements,
    description, treaty_label,
    generator_treaty_id = gen_tid, scope_type, scope_name, tower_id, n_layers,
    premium_linked_limit, industry_trigger) |>
    arrange(chain_rank, generator_treaty_id, method = "radix")
}

make_edges <- function(edges_df, treaties) {
  sid <- setNames(treaties$treaty_id, treaties$generator_treaty_id)
  e <- edges_df |>
    transmute(from_treaty = unname(sid[from_treaty_id]), to_treaty = unname(sid[to_treaty_id]),
              edge_kind, rank_gap) |>
    group_by(to_treaty) |> mutate(n_parents = n()) |> ungroup() |>
    mutate(edge_type = ifelse(n_parents > 1, "converge", "chain"))
  e |> select(from_treaty, to_treaty, edge_type, edge_kind, rank_gap) |>
    arrange(from_treaty, to_treaty, method = "radix")
}

graph_stats <- function(treaties, edges) {
  ids <- treaties$treaty_id; rk <- setNames(treaties$chain_rank, ids)
  connected <- unique(c(edges$from_treaty, edges$to_treaty))
  lp <- setNames(rep(0L, length(ids)), ids)
  for (v in ids[order(rk)]) for (w in edges$to_treaty[edges$from_treaty == v]) lp[w] <- max(lp[w], lp[v] + 1L)
  comp <- setNames(seq_along(ids), ids)
  repeat {
    ch <- FALSE
    for (i in seq_len(nrow(edges))) {
      a <- comp[edges$from_treaty[i]]; b <- comp[edges$to_treaty[i]]
      if (a != b) { comp[comp == max(a, b)] <- min(a, b); ch <- TRUE }
    }
    if (!ch) break
  }
  comp_tab <- table(comp[connected])
  sc <- setNames(treaties$scope, ids)
  list(
    treaties = nrow(treaties), connected = length(connected), isolated = nrow(treaties) - length(connected),
    edges = nrow(edges), components = length(comp_tab), largest_component = as.integer(max(comp_tab)),
    longest_path_edges = max(lp), longest_path_nodes = max(lp) + 1L,
    max_parents = max(table(edges$to_treaty)), multi_parent_nodes = sum(table(edges$to_treaty) > 1),
    cross_scope_edges = sum(sc[edges$from_treaty] != sc[edges$to_treaty]),
    type_mix = paste(names(table(treaties$treaty_type)), table(treaties$treaty_type), sep = ":", collapse = ", "))
}

write_pair <- function(x, dir, name) {
  write_csv(x, file.path(dir, paste0(name, ".csv")), na = "NA")
  write_parquet(x, file.path(dir, paste0(name, ".parquet")))
}

# ---------------------------------------------------------------------------
# 2. Talk run: treaties, nodes, edges
# ---------------------------------------------------------------------------
treaty_df <- rd(RAW, "treaty_df"); edges_df <- rd(RAW, "edges_df"); lob_df <- rd(RAW, "lob_df")
segment_df <- rd(RAW, "segment_df"); portfolio_df <- rd(RAW, "portfolio_df")
division_df <- rd(RAW, "division_df"); coverage_df <- rd(RAW, "coverage_df")
event_df <- rd(RAW, "event_df"); loss_df <- rd(RAW, "loss_df"); period_df <- rd(RAW, "period_df")
flow_df <- rd(RAW, "flow_df"); tp_df <- rd(RAW, "treaty_period_df")
stopifnot(nrow(portfolio_df) == 1)

treaties <- make_treaties(treaty_df, lob_df, segment_df, portfolio_df)
inuring_edges <- make_edges(edges_df, treaties)
nodes_tbl <- treaties |> filter(treaty_id %in% c(inuring_edges$from_treaty, inuring_edges$to_treaty))
write_csv(treaties, file.path(OUT, "treaties.csv"), na = "NA")
write_csv(inuring_edges, file.path(OUT, "inuring_edges.csv"), na = "NA")
write_pair(nodes_tbl, OUT, "nodes_tbl")
write_pair(inuring_edges, OUT, "edges_tbl")
talk_stats <- graph_stats(treaties, inuring_edges)

# ---------------------------------------------------------------------------
# 3. Independent functional recompute of flow_df
#    Pure functions; state (residual ledger, cap usage, emitted rows) is threaded through
#    Reduce(), never mutated in place. Semantics replicate lib_flows.R (see README).
# ---------------------------------------------------------------------------
cell_key <- function(event_id, lob_id, loss_type) paste(event_id, lob_id, loss_type, sep = "::")

loss_cells <- loss_df |>
  mutate(key = cell_key(event_id, lob_id, loss_type)) |>
  arrange(event_id, lob_id, method = "radix")
event_meta <- event_df |> select(event_id, period_id, event_type, event_seq, industry_loss)
period_events <- event_meta |> filter(event_id %in% loss_cells$event_id) |>
  mutate(pidx = match(period_id, period_df$period_id)) |>
  arrange(pidx, event_seq, event_id, method = "radix")
all_lobs <- lob_df$lob_id

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

# Processing units: towers occupy one slot at layer-1 rank; ordered by (chain_rank, treaty_id).
unit_order <- treaty_df |>
  filter(is.na(layer_index) | layer_index == 1L) |>
  mutate(unit_key = coalesce(tower_id, treaty_id)) |>
  arrange(chain_rank, treaty_id, method = "radix")
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

state0 <- list(ledger = setNames(loss_cells$gross_loss, loss_cells$key), cap = numeric(0), rows = list())
final <- Reduce(apply_unit, unit_order$unit_key, state0)
recompute <- bind_rows(final$rows)

# ---------------------------------------------------------------------------
# 4. Reconcile the recompute against the generator's flow_df
# ---------------------------------------------------------------------------
key5 <- function(d) paste(d$period_id, d$event_id, d$treaty_id, coalesce(d$lob_id, "NA"), d$loss_type, sep = "|")
cmp <- full_join(
  flow_df |> transmute(k = key5(flow_df), g_subject = subject_loss_in, g_ceded = ceded, in_g = TRUE),
  recompute |> transmute(k = key5(recompute), f_subject = subject, f_ceded = ceded, in_f = TRUE), by = "k") |>
  mutate(in_g = coalesce(in_g, FALSE), in_f = coalesce(in_f, FALSE))
close <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= TOL_REL * pmax(1, abs(a), abs(b)))
cmp <- cmp |> mutate(ok = in_g & in_f & close(g_ceded, f_ceded) & close(g_subject, f_subject),
                     d_ceded = abs(g_ceded - f_ceded), d_subject = abs(g_subject - f_subject))
tp_cmp <- tp_df |> filter(!is.na(reinstatements_used)) |>
  mutate(f_used = get_cap(final$cap, paste0("r|", treaty_id, "|", period_id)) /
           treaty_df$limit[match(treaty_id, treaty_df$treaty_id)])
recon <- tibble(
  metric = c("flow_df rows", "recompute rows", "rows matched (ceded and subject within tol)", "match rate",
             "rows only in one side", "max abs diff ceded", "max abs diff subject_loss_in",
             "treaty_period rows compared (reinstatements_used)", "treaty_period max abs diff", "relative tolerance"),
  value = c(nrow(flow_df), nrow(recompute), sum(cmp$ok), mean(cmp$ok),
            sum(!cmp$in_g | !cmp$in_f),
            max(cmp$d_ceded, na.rm = TRUE), max(cmp$d_subject, na.rm = TRUE),
            nrow(tp_cmp), max(abs(tp_cmp$reinstatements_used - tp_cmp$f_used)), TOL_REL))
write_csv(recon, file.path(OUT, "recompute_reconciliation.csv"))

# ---------------------------------------------------------------------------
# 5. Event selection, loss_events, walkthrough
# ---------------------------------------------------------------------------
tshort <- setNames(treaties$treaty_id, treaties$generator_treaty_id)
tinfo <- treaty_df |> transmute(gid = treaty_id, treaty_type, chain_rank, limit,
                                prop = treaty_type %in% c("quota_share", "group_quota_share", "surplus"),
                                resp_reinst = treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash"),
                                rein = coalesce(reinstatements, 0L))
# per event x treaty (summing over LOB cells), from the generator's own flow_df
src <- function(use_functional) {
  if (use_functional) recompute |> transmute(period_id, event_id, treaty_id, lob_id, loss_type, subject_loss_in = subject, ceded)
  else flow_df |> filter(is_indemnity) |> select(period_id, event_id, treaty_id, lob_id, loss_type, subject_loss_in, ceded)
}
ev_tr <- function(fl) {
  fl |> filter(!is.na(subject_loss_in)) |>
    inner_join(tinfo, by = c("treaty_id" = "gid")) |>
    group_by(period_id, event_id, treaty_id, treaty_type, chain_rank, limit, prop) |>
    summarise(gross_in = sum(subject_loss_in), ceded = sum(ceded), .groups = "drop") |>
    mutate(exhausted = !prop & !is.na(limit) & limit > 0 & ceded >= limit * (1 - TOL_REL))
}
et_g <- ev_tr(src(FALSE))
ev_gross <- loss_cells |> group_by(event_id) |>
  summarise(gross = sum(gross_loss), n_lobs = n_distinct(lob_id), .groups = "drop") |>
  left_join(event_meta, by = "event_id")

# reinstatement-crossing event: first event (period, event_seq order) taking a treaty-period's cumulative cession beyond 1x limit
cross <- et_g |> filter(!prop, treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash")) |>
  left_join(event_meta |> select(event_id, event_seq), by = "event_id") |>
  mutate(pidx = match(period_id, period_df$period_id)) |>
  arrange(treaty_id, pidx, event_seq, event_id, method = "radix") |>
  group_by(treaty_id, period_id) |> mutate(cum = cumsum(ceded), prev = cum - ceded) |> ungroup() |>
  filter(prev <= limit * (1 + TOL_REL), cum > limit * (1 + TOL_REL))
# sandwich: a proportional treaty that pays, ranked strictly between two paying non-proportional treaties
# (XoL / proportional / XoL ordering in chain_rank)
sandwich <- et_g |> filter(ceded > 0) |> group_by(event_id) |>
  summarise(ok = any(map_lgl(chain_rank[prop], function(r) any(!prop & chain_rank < r) && any(!prop & chain_rank > r))), .groups = "drop") |> filter(ok)
conv_nodes <- inuring_edges |> group_by(to_treaty) |> filter(n() > 1) |> ungroup()
gid_of <- setNames(treaties$generator_treaty_id, treaties$treaty_id)
conv_hits <- et_g |>
  mutate(short = unname(tshort[treaty_id])) |>
  group_by(event_id) |>
  summarise(n_conv = sum(map_int(unique(conv_nodes$to_treaty), function(sh)
    as.integer(sh %in% short && sum(conv_nodes$from_treaty[conv_nodes$to_treaty == sh] %in% short) >= 2))), .groups = "drop")

pick <- function(ids) if (length(ids)) ids[1] else NA_character_
top <- function(d, col) d |> arrange(desc(.data[[col]]), event_id, method = "radix") |> pull(event_id)
cand <- list(
  attritional = ev_gross |> filter(event_type == "attritional", event_id %in% et_g$event_id[et_g$prop & et_g$ceded > 0]) |>
    arrange(gross, event_id, method = "radix") |> pull(event_id),
  exhaustion = et_g |> group_by(event_id) |> summarise(n = sum(exhausted), .groups = "drop") |> inner_join(ev_gross |> filter(event_type == "large"), by = "event_id") |> top("n"),
  reinstatement = cross |> arrange(desc(ceded), event_id, method = "radix") |> pull(event_id),
  cat_multi_lob = ev_gross |> filter(event_type == "cat", n_lobs > 1) |> top("gross"),
  sandwich = ev_gross |> filter(event_id %in% sandwich$event_id) |> top("gross"),
  convergence = conv_hits |> filter(n_conv > 0) |> inner_join(ev_gross |> filter(event_type == "large"), by = "event_id") |> top("n_conv"),
  clash = et_g |> filter(treaty_type == "clash", ceded > 0) |> arrange(desc(ceded), event_id, method = "radix") |> pull(event_id),
  aggregate = et_g |> filter(treaty_type %in% c("aggregate_xl", "stop_loss"), ceded > 0) |> arrange(desc(ceded), event_id, method = "radix") |> pull(event_id))
chosen <- character(0); tags <- list()
for (nm in names(cand)) {
  id <- pick(setdiff(cand[[nm]], chosen))
  if (!is.na(id)) { chosen <- c(chosen, id); tags[[id]] <- nm }
}
sel <- tibble(generator_event_id = chosen, tag = unlist(tags[chosen]), event_id = sprintf("EV%02d", seq_along(chosen)))

touched <- et_g |> filter(event_id %in% chosen) |>
  mutate(short = unname(tshort[treaty_id]))
entry <- touched |> arrange(chain_rank, treaty_id, method = "radix") |> group_by(event_id) |> slice(1) |> ungroup() |>
  select(event_id, entry_treaty = short)
lobname <- setNames(sub("\\s*\\(P[0-9]+ [0-9.]+\\)$", "", lob_df$name), lob_df$lob_id)
ev_lobs <- loss_cells |> filter(event_id %in% chosen) |> group_by(event_id) |>
  summarise(lobs = paste(unique(lobname[lob_id]), collapse = " + "), .groups = "drop")
desc_tbl <- sel |>
  left_join(ev_gross |> select(generator_event_id = event_id, gross, event_type, n_lobs), by = "generator_event_id") |>
  left_join(ev_lobs, by = c("generator_event_id" = "event_id")) |>
  mutate(description = sprintf("%s %s event on %s [%s]", tools::toTitleCase(event_type), ifelse(n_lobs > 1, "multi-LOB", "single-LOB"), lobs,
                               c(attritional = "simple proportional", exhaustion = "layer exhaustion", reinstatement = "reinstatement consumed",
                                 cat_multi_lob = "cat across LOBs", sandwich = "XoL / proportional / XoL sandwich", convergence = "convergence",
                                 clash = "clash hub", aggregate = "aggregate / stop loss")[tag]))

# decide the walkthrough source by reconciling the chosen events
chosen_ok <- cmp |> mutate(ev = sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", k)) |> filter(ev %in% chosen) |> summarise(ok = all(ok)) |> pull(ok)
use_functional <- isTRUE(chosen_ok)
et_use <- ev_tr(src(use_functional)) |> filter(event_id %in% chosen)
build_walk <- function(et) {
  net_total <- ev_gross |> filter(event_id %in% chosen) |> select(event_id, gross) |>
    left_join(et |> group_by(event_id) |> summarise(tot_ceded = sum(ceded), .groups = "drop"), by = "event_id") |>
    mutate(final_net_retained = gross - tot_ceded)
  et |> left_join(sel |> select(generator_event_id, event_id_short = event_id), by = c("event_id" = "generator_event_id")) |>
    left_join(desc_tbl |> select(generator_event_id, event_desc = description), by = c("event_id" = "generator_event_id")) |>
    left_join(net_total |> select(event_id, final_net_retained), by = "event_id") |>
    arrange(event_id_short, chain_rank, treaty_id, method = "radix") |>
    transmute(event_id = event_id_short, event_desc, treaty_id = unname(tshort[treaty_id]), treaty_type,
              gross_in, ceded, net_out = gross_in - ceded, layer_exhausted = exhausted, final_net_retained)
}
walk_use <- build_walk(et_use)
write_csv(walk_use, file.path(OUT, "loss_allocation_walkthrough.csv"), na = "NA")
if (!use_functional) {
  write_csv(build_walk(ev_tr(src(TRUE)) |> filter(event_id %in% chosen)), file.path(OUT, "walkthrough_functional.csv"), na = "NA")
}
loss_events <- desc_tbl |> left_join(entry, by = c("generator_event_id" = "event_id")) |>
  transmute(event_id, description, entry_treaty, gross_loss = gross, generator_event_id, scenario = tag)
write_csv(loss_events, file.path(OUT, "loss_events.csv"), na = "NA")

# ---------------------------------------------------------------------------
# 6. Checks
# ---------------------------------------------------------------------------
chk <- c(
  edge_endpoints_exist = all(c(inuring_edges$from_treaty, inuring_edges$to_treaty) %in% treaties$treaty_id),
  walkthrough_treaties_exist = all(walk_use$treaty_id %in% treaties$treaty_id),
  entry_treaties_exist = all(loss_events$entry_treaty %in% treaties$treaty_id),
  no_na_keys = !anyNA(treaties[c("treaty_id", "treaty_type", "chain_rank")]) && !anyNA(inuring_edges[c("from_treaty", "to_treaty", "edge_type")]) &&
    !anyNA(walk_use[c("event_id", "treaty_id", "gross_in", "ceded", "net_out")]) && !anyNA(loss_events[c("event_id", "entry_treaty", "gross_loss")]),
  ceded_plus_net_eq_gross = all(abs(walk_use$ceded + walk_use$net_out - walk_use$gross_in) <= TOL_REL * pmax(1, walk_use$gross_in)),
  treaty_ids_unique = !anyDuplicated(treaties$treaty_id))
cat("CHECKS\n"); print(chk)
cat("RECONCILIATION\n"); print(as.data.frame(recon))
cat("WALKTHROUGH SOURCE:", ifelse(use_functional, "functional recompute (reconciles)", "flow_df (mismatch on selected events)"), "\n")

# Phase 2 stats for the README
tp_ok <- tp_df |> left_join(treaty_df |> select(treaty_id, treaty_type, reinstatements, limit), by = "treaty_id") |>
  filter(treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash"))
et_all <- ev_tr(src(FALSE))
p2 <- list(
  events = n_distinct(loss_df$event_id), event_types = paste(names(table(event_df$event_type)), table(event_df$event_type), sep = ":", collapse = ", "),
  loss_rows = nrow(loss_df), flow_rows = nrow(flow_df),
  distinct_entry_treaties = et_all |> arrange(chain_rank, treaty_id, method = "radix") |> group_by(event_id) |> slice(1) |> ungroup() |> distinct(treaty_id) |> nrow(),
  layer_exhaustions_event_x_layer = sum(et_all$exhausted), distinct_layers_exhausted = n_distinct(et_all$treaty_id[et_all$exhausted]),
  treaty_periods_beyond_one_limit = sum(tp_ok$reinstatements_used > 1 + 1e-9, na.rm = TRUE),
  treaty_periods_at_reinstatement_cap = sum(tp_ok$reinstatements_used >= tp_ok$reinstatements + 1 - 1e-9, na.rm = TRUE),
  multi_lob_cat_events = sum(ev_gross$event_type == "cat" & ev_gross$n_lobs > 1))
cat("TALK GRAPH STATS\n"); str(talk_stats); cat("PHASE2 STATS\n"); str(p2)
cat("SELECTED EVENTS\n"); print(as.data.frame(loss_events |> mutate(gross_loss = round(gross_loss))))

# ---------------------------------------------------------------------------
# 7. Realistic mega exhibit (Phase 1 only)
# ---------------------------------------------------------------------------
MD <- file.path(OUT, "realistic_mega"); dir.create(MD, showWarnings = FALSE)
m_treaty <- rd(MEGA, "treaty_df"); m_edges <- rd(MEGA, "edges_df")
m_t <- make_treaties(m_treaty, rd(MEGA, "lob_df"), rd(MEGA, "segment_df"), rd(MEGA, "portfolio_df"))
m_e <- make_edges(m_edges, m_t)
write_pair(m_t, MD, "nodes_tbl"); write_pair(m_e, MD, "edges_tbl")
m_stats <- graph_stats(m_t, m_e)
write_csv(tibble(metric = names(m_stats), value = as.character(unlist(m_stats))), file.path(MD, "stats.csv"))
cat("MEGA STATS\n"); str(m_stats)
write_csv(tibble(metric = names(talk_stats), value = as.character(unlist(talk_stats))), file.path(OUT, "graph_stats.csv"))
write_csv(tibble(metric = names(p2), value = as.character(unlist(p2))), file.path(OUT, "phase2_stats.csv"))

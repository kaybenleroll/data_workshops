# reconcile_mega.R -- run the DAG-driven recovery on data_mega and reconcile it with the generator's flow_df.
# Run inside the talk container from this directory:  Rscript reconcile_mega.R [data_dir]
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("recover_functions.R")
args <- commandArgs(TRUE); dir <- if (length(args)) args[1] else "data_mega"
data <- load_mega(dir)

# ---- run time ---------------------------------------------------------------------------------------
tm <- system.time(res <- recover_losses(data))
cat(sprintf("== engine run time: %.2f s elapsed (%d ledger rows)\n", tm[["elapsed"]], nrow(res$ledger)))

# ---- reconciliation with flow_df -----------------------------------------------------------------------
key <- c("event_id", "treaty_id", "lob_id", "loss_type")
rel <- rel_diff
cf <- compare_flows(res, data)
cmp <- cf$rows
cat(sprintf("\n== rows: engine %d, generator %d, matched keys %d, only engine %d, only generator %d\n",
            cf$n_engine, cf$n_generator, cf$n_matched_keys, cf$n_only_engine, cf$n_only_generator))
cat(sprintf("rows matching on ceded, retained_after, subject_loss_in, reinstatement_consumed (rel 1e-9): %d / %d (%.4f%%)\n",
            sum(cmp$ok), nrow(cmp), 100 * mean(cmp$ok)))
cat(sprintf("max abs diff ceded %.3g | max rel diff ceded %.3g, retained_after %.3g, subject_loss_in %.3g, reinstatement_consumed %.3g\n",
            max(cmp$abs_ceded), max(cmp$d_ceded), max(cmp$d_ret), max(cmp$d_subj), max(cmp$d_reinst)))
cat("mismatches by treaty type:\n"); print(cmp |> filter(!ok) |> count(treaty_type))
tp <- inner_join(res$treaty_period, data$treaty_period_df, by = c("treaty_id", "period_id"), suffix = c("", ".gen"))
tp_ok <- rel(tp$reinstatements_used, tp$reinstatements_used.gen) <= 1e-9 & rel(tp$aggregate_consumed, tp$aggregate_consumed.gen) <= 1e-9 &
  rel(tp$reinstatement_premium, tp$reinstatement_premium.gen) <= 1e-9 & !xor(is.na(tp$reinstatements_used), is.na(tp$reinstatements_used.gen)) &
  !xor(is.na(tp$aggregate_consumed), is.na(tp$aggregate_consumed.gen))
cat(sprintf("treaty_period rows matching (reinstatements_used, aggregate_consumed, reinstatement_premium): %d / %d\n", sum(tp_ok), nrow(tp)))
cat(sprintf("treaty-periods with reinstatements used (> 0): %d (generator: %d)\n",
            sum(res$treaty_period$reinstatements_used > 0, na.rm = TRUE), sum(data$treaty_period_df$reinstatements_used > 0, na.rm = TRUE)))
cat(sprintf("conservation: sum(gross - net - indemnity ceded) = %.3g\n", sum(res$cells$gross_loss) - sum(res$cells$net_loss) - sum(res$ledger$ceded[res$ledger$is_indemnity])))

# ---- order ---------------------------------------------------------------------------------------------
oa <- order_agrees(data)
cat("\n== order\n"); print(unlist(oa))
units <- recovery_units(data); topo <- order_by_topo_sort(data, units)
alt <- recover_losses(data, topo)
jt <- compare_flows(alt, data)$rows
cat(sprintf("topo_sort order vs generator flow_df: ceded matches on %.2f%% of %d rows\n",
            100 * mean(jt$ceded_ok), nrow(jt)))

# ---- headline numbers ------------------------------------------------------------------------------------
cat("\n== headline\n")
tot <- res$cells |> summarise(gross = sum(gross_loss), net = sum(net_loss))
ced <- sum(res$ledger$ceded[res$ledger$is_indemnity])
cat(sprintf("gross %.6g | ceded %.6g | net %.6g | net/gross %.4f\n", tot$gross, ced, tot$net, tot$net / tot$gross))
print(recovery_summary(res, loss_type), width = 120)
print(recovery_summary(res, period_id), width = 120)
cat("ceded by treaty_type\n"); print(recoveries_by(res, treaty_type) |> arrange(desc(ceded)), width = 120)
cat("top 10 treaties by ceded\n"); print(recoveries_by(res, treaty_id, treaty_type) |> slice_max(ceded, n = 10), width = 120)
ex <- exhaustion(res, data)
cat("exhaustions (summed rule) by level and treaty type\n"); print(ex |> filter(exhausted) |> count(level, treaty_type))
cat("exhaustions the row-level test would find (event level):\n")
rowlvl <- res$ledger |> filter(is_indemnity, treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash")) |>
  mutate(limit = data$treaty_df$limit[match(treaty_id, data$treaty_df$treaty_id)]) |> filter(ceded >= limit * (1 - 1e-9)) |>
  distinct(event_id, treaty_id)
cat(sprintf("  row-level %d vs summed %d\n", nrow(rowlvl), sum(ex$exhausted & ex$level == "event")))
cat("treaty-periods with reinstatements used (> 0) by type\n")
print(res$treaty_period |> filter(reinstatements_used > 0) |> left_join(data$treaty_df |> select(treaty_id, treaty_type), by = "treaty_id") |> count(treaty_type))
cat("five largest events by gross\n")
big <- gross_by(res, event_id, loss_type, period_id) |> slice_max(gross, n = 5) |> left_join(net_by(res, event_id), by = "event_id") |>
  left_join(recoveries_by(res, event_id), by = "event_id")
print(big, width = 140)
for (id in big$event_id)
  print(res$ledger |> filter(event_id == id, is_indemnity, ceded > 0) |> group_by(treaty_id, treaty_type) |> summarise(ceded = sum(ceded), .groups = "drop") |> arrange(desc(ceded)) |> mutate(event_id = id), width = 140, n = 12)

# ---- loss anatomy -------------------------------------------------------------------------------------------
la <- loss_anatomy(data, res)
cat("\n== loss anatomy\n"); print(la$rows_per_event); print(la$lobs_per_cat_event)
cat("events with several loss types:", la$events_with_several_loss_types, "| loss_type == event_type:", la$loss_type_equals_event_type, "\n")
print(la$events_without_loss_rows); print(la$columns, n = 20)
for (nm in names(la$example_events)) { cat("\n--", nm, la$example_events[[nm]], "\n"); print(la$example_event_rows[[nm]], width = 140); print(la$example_loss_rows[[nm]], width = 140)
  cat("treaties seeing it:", nrow(la$who_sees_what[[nm]]), "of", la$n_treaties, "\n"); print(la$who_sees_what[[nm]], n = 25, width = 160) }

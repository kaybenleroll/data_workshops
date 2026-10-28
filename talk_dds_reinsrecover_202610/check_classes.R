# check_classes.R -- invariants and headline numbers for the class model + explicit-inuring engine.
# Run from the talk directory inside the container:  Rscript check_classes.R
# Validity is outcome-based: net >= 0 on every loss row is a HARD invariant (a violation is a data error).
# PRIMARY table = the generator's edges_df if it yields no row with net < 0, else the locally built complete table.
suppressMessages({library(dplyr); library(tidyr); library(readr)})
source("class_model.R"); source("class_engine.R")
dir.create(".scratch", showWarnings = FALSE)
fails <- 0L
check <- function(name, ok, detail = "") {
  ok <- isTRUE(ok); if (!ok) fails <<- fails + 1L
  cat(sprintf("%s  %s%s\n", if (ok) "PASS" else "FAIL", name, if (nzchar(detail)) paste0("  [", detail, "]") else ""))
}
tick <- function() proc.time()[["elapsed"]]
bn <- function(x) sprintf("%.3fbn", x / 1e9)
tol <- 1e-9

t0 <- tick(); w <- class_world(); t_world <- tick() - t0
t1 <- tick(); primary <- run_inuring_engine(w); t_engine <- tick() - t1
data <- w$data; t <- data$treaty_df; ct <- w$class_table
cat(sprintf("\nTIMING  world+simulation+risk split+primary decision %.2fs | one engine run %.2fs\n", t_world, t_engine))
gc_ <- w$inuring_choice
cat(sprintf("PRIMARY inuring table = %s (generator edges_df: %d of %d rows with net < 0, %s = %.2f%% of gross)\n\n", gc_$which,
            gc_$generator_outcome$n_rows_over, gc_$generator_outcome$n_rows, bn(gc_$generator_outcome$amount_over), 100 * gc_$generator_outcome$frac_of_gross))

# ---- (vi) bridge ---------------------------------------------------------------------------------------------
br <- bridge_report(w$bridge, t, ct)
check("(vi) every treaty has >= 1 class", br$n_treaties_without_class == 0)
tw <- w$filters |> filter(!is.na(match(treaty_id, t$treaty_id[!is.na(t$tower_id)]))) |>
  mutate(tower = t$tower_id[match(treaty_id, t$treaty_id)], key = vapply(seq_len(n()), function(i) paste(paste(lob_set[[i]], collapse = ","), paste(region_set[[i]], collapse = ",")), ""))
check("(vi) tower layers share one filter", all(tapply(tw$key, tw$tower, function(x) length(unique(x))) == 1))
bc <- w$bridge |> mutate(tower = t$tower_id[match(treaty_id, t$treaty_id)]) |> filter(!is.na(tower)) |> group_by(tower, treaty_id) |> summarise(k = paste(sort(class_id), collapse = "|"), .groups = "drop")
check("(vi) tower layers share bridge classes", all(tapply(bc$k, bc$tower, function(x) length(unique(x))) == 1))

# ---- (vii) DAG / order / validity --------------------------------------------------------------------------------
oa <- tryCatch({ old <- .old_engine$load_mega("data_mega"); .old_engine$order_agrees(old) }, error = function(e) NULL)
if (!is.null(oa)) {
  check("(vii) Kahn order == chain_rank order (generator edges)", oa$kahn_equals_chain_rank)
  check("(vii) edges broken by atomic tower placement (earlier finding: 14 of 133)", oa$edge_violations_kahn == 14L, sprintf("%d of %d", oa$edge_violations_kahn, oa$n_edges))
}
aud <- attr(w$inuring_generator, "audit")
cat("INFO generator edges -> unit table audit:", paste(names(aud), unlist(aud), sep = "=", collapse = ", "), "\n")
check("(vii) both inuring tables are DAGs", igraph::is_dag(igraph::graph_from_data_frame(w$inuring_generator[c("from_unit", "to_unit")])) &&
                                           igraph::is_dag(igraph::graph_from_data_frame(w$inuring_local[c("from_unit", "to_unit")])))
gen_res <- withCallingHandlers(run_inuring_engine(w, w$inuring_generator),
                               warning = function(c) { wmsg <<- conditionMessage(c); invokeRestart("muffleWarning") })
check("(vii) engine flags the generator table's over-recovery clearly (warning with counts)", exists("wmsg") && grepl("INVALID OUTCOME", wmsg), if (exists("wmsg")) substr(wmsg, 1, 120) else "no warning")
dg <- inuring_diagnostics(w, w$inuring_generator, gen_res); dl <- inuring_diagnostics(w, w$inuring_local, primary_loc <- run_inuring_engine(w, w$inuring_local))
cat(sprintf("INFO overlapping unit pairs %d | generator table: with edge %d (%.0f%%), without %d = %d cause over-recovery on >= 1 row + %d harmless | local table: with edge %d, without %d\n",
            dg$n_overlapping, dg$n_with_edge, 100 * dg$n_with_edge / dg$n_overlapping, dg$n_without_edge, dg$n_without_edge_harmful, dg$n_without_edge_harmless, dl$n_with_edge, dl$n_without_edge))

# ---- (i) accounting; (ii)/(iii) caps and signs --------------------------------------------------------------------------
chk_run <- function(r, lab) {
  ced_row <- gsum(r$ledger$ceded, r$ledger$row_id, nrow(r$rows))
  check(sprintf("(i) gross = sum(ceded) + net on every row [%s]", lab), all(abs(r$rows$gross_loss - ced_row - r$rows$net_loss) <= tol * pmax(1, r$rows$gross_loss)))
  check(sprintf("(i) HARD: net >= 0 on every row [%s]", lab), all(r$rows$net_loss >= -tol * pmax(1, r$rows$gross_loss)))
  check(sprintf("(i) ceded <= subject loss <= gross for every treaty and row [%s]", lab),
        all(r$ledger$ceded <= r$ledger$subject_loss_in * (1 + tol)) && all(r$ledger$subject_loss_in <= r$rows$gross_loss[r$ledger$row_id] * (1 + tol)))
  ex <- exhaustion(r)
  check(sprintf("(ii) recoveries within risk / event / period / reinstatement caps [%s]", lab), all(ex$ceded <= ex$bound * (1 + tol)),
        sprintf("%d treaty-key cells, max ratio %.9f", nrow(ex), max(ex$ceded / ex$bound)))
  check(sprintf("(iii) ceded >= 0 on every ledger row and per treaty [%s]", lab), all(r$ledger$ceded >= 0) && all(tapply(r$ledger$ceded, r$ledger$treaty_id, sum) >= 0))
}
chk_run(primary, "primary")

# ---- peer exceptions -----------------------------------------------------------------------------------------------------------
pe <- pick_peer_exceptions(w)
check("(pe) three deterministic peer exceptions chosen", nrow(pe) == 3 && identical(pe, pick_peer_exceptions(w)))
wx <- apply_peer_exceptions(w, pe)
check("(pe) each exception removes exactly one edge", nrow(wx$inuring) == nrow(w$inuring) - 3, sprintf("%d edges, was %d", nrow(wx$inuring), nrow(w$inuring)))
px <- run_inuring_engine(wx, on_invalid = "ignore")
ox <- outcome_violations(px)
chk_run_soft <- function(r, lab) {
  check(sprintf("(i) ceded <= subject loss <= gross for every treaty and row [%s]", lab),
        all(r$ledger$ceded <= r$ledger$subject_loss_in * (1 + tol)) && all(r$ledger$subject_loss_in <= r$rows$gross_loss[r$ledger$row_id] * (1 + tol)))
  check(sprintf("(ii) caps and reinstatement limits respected [%s]", lab), all(exhaustion(r)$ceded <= exhaustion(r)$bound * (1 + tol)))
}
chk_run_soft(px, "peer exceptions")
cat(sprintf("  peer exceptions outcome: %s; rows with net < 0: %d (%s over-recovery)\n", if (ox$valid) "VALID (net >= 0 everywhere)" else "INVALID", ox$summary$n_rows_over, bn(ox$summary$amount_over)))
for (i in seq_len(nrow(pe))) {
  tid <- pe$to_treaty_id[i]
  before <- sum(primary$ledger$ceded[primary$ledger$treaty_id == tid]); after <- sum(px$ledger$ceded[px$ledger$treaty_id == tid])
  cat(sprintf("  exception %d: '%s' not net of '%s' [%s]: treaty ceded %s -> %s\n", i, pe$to_name[i], pe$from_name[i], pe$from_type[i], bn(before), bn(after)))
}
cat(sprintf("  all exceptions: ceded %s -> %s, net %s -> %s\n", bn(sum(primary$ledger$ceded)), bn(sum(px$ledger$ceded)), bn(sum(primary$rows$net_loss)), bn(sum(px$rows$net_loss))))
if (!ox$valid) { print(head(ox$pairs, 5), width = 200); print(head(ox$events, 3)) }

# ---- (iv) determinism ----------------------------------------------------------------------------------------------------------
md5_of <- function(res) { f <- file.path(".scratch", "ledger_hash.bin")
  writeBin(serialize(res$ledger[c("row_id", "treaty_idx", "subject_loss_in", "ceded")], NULL, version = 3), f); unname(tools::md5sum(f)) }
w2 <- class_world()
check("(iv) determinism: two runs, same seeds, identical ledger md5", md5_of(run_inuring_engine(w2)) == md5_of(primary), md5_of(primary))
check("(iv) class table, bridge and inuring tables identical across runs", identical(w2$class_table, w$class_table) && identical(w2$bridge, w$bridge) &&
        identical(w2$inuring_generator, w$inuring_generator) && identical(w2$inuring_local, w$inuring_local))
check("(iv) peer exceptions deterministic", md5_of(run_inuring_engine(apply_peer_exceptions(w2), on_invalid = "ignore")) == md5_of(px))
w3 <- class_world(sim_seed = 1)
check("(iv) different sim seed gives different losses", !isTRUE(all.equal(w3$loss_rows$gross_loss[1:50], w$loss_rows$gross_loss[1:50])))

# ---- (v) order independence ------------------------------------------------------------------------------------------------------
sh <- w; sh$loss_rows <- w$loss_rows[with_seed(99, sample(nrow(w$loss_rows))), ]
chk <- compare_runs(primary, run_inuring_engine(sh))
check("(v) shuffling input row order gives the same ledger", chk$max_abs_row_diff <= 1e-6 && chk$frac_rows_differ == 0, sprintf("max abs diff %.2e", chk$max_abs_row_diff))
for (lab in c("primary", "generator table", "peer exceptions")) {
  ww <- if (lab == "primary") w else if (lab == "generator table") { z <- w; z$inuring <- w$inuring_generator; z } else wx
  os <- order_sensitivity(ww, ww$inuring, n_random = 20)
  check(sprintf("(v) 20 random topological orders (+ rank order) give the same ledger [%s]", lab),
        os$worst$max_abs_row_diff <= 1e-6 && os$worst$frac_rows_differ == 0,
        sprintf("%d orders; max abs row diff %.2e, treaty ceded %.2e, net by period %.2e", nrow(os$table), os$worst$max_abs_row_diff, os$worst$max_abs_treaty_ceded_diff, os$worst$max_abs_net_period_diff))
}
rr <- run_residual_engine(w)
cr <- compare_runs(primary_loc, rr)
check("(v) complete (repair) table == residual-ledger engine in chain_rank order", cr$max_abs_row_diff <= 1e-6 && cr$frac_rows_differ == 0, sprintf("max abs diff %.2e", cr$max_abs_row_diff))

# ---- (viii) sanity + headline --------------------------------------------------------------------------------------------------------
tot_g <- sum(primary$rows$gross_loss); tot_c <- sum(primary$ledger$ceded); tot_n <- sum(primary$rows$net_loss)
check("(viii) net share plausible (0.2 - 0.8 of gross)", tot_n / tot_g > 0.2 && tot_n / tot_g < 0.8, sprintf("net share %.3f", tot_n / tot_g))
check("(viii) total gross same order as generator's mega run (26.6bn; factor 2)", tot_g > 13e9 && tot_g < 53e9, bn(tot_g))
cat(sprintf("\nHEADLINE (primary) gross %s | ceded %s | net %s | net share %.3f\n", bn(tot_g), bn(tot_c), bn(tot_n), tot_n / tot_g))
print(recoveries_by(primary, treaty_type) |> mutate(ceded_bn = round(ceded / 1e9, 3), share = round(ceded / tot_c, 3)) |> select(-ceded) |> arrange(desc(ceded_bn)))
print(recoveries_by(primary, treaty_name, treaty_type) |> arrange(desc(ceded)) |> head(5) |> mutate(ceded_bn = round(ceded / 1e9, 3)) |> select(-ceded))
print(exhaustion_summary(exhaustion(primary)))
tp <- primary$treaty_period; cap_t <- tp$treaty_type %in% c("xol_per_risk", "xol_per_occurrence", "clash")
cat("reinstatement-using treaty-periods (utilisation > 1, capped kinds):", sum(cap_t & tp$utilisation > 1 + tol, na.rm = TRUE), "of", sum(cap_t), "capped treaty-periods with recoveries\n")
big <- primary$events |> arrange(desc(gross)) |> head(1); rc <- treaty_reach(primary) |> filter(event_id == big$event_id)
cat(sprintf("largest event %s (%s): gross %s -> net %s, %d classes, %d risks, %d treaties see it, %d paid\n", big$event_id, big$peril, bn(big$gross), bn(big$net), big$n_classes, big$n_risks, rc$n_treaties_seeing, rc$n_treaties_paying))

cat("\nSIMULATION  events:", nrow(w$events_sim), " loss rows:", nrow(w$loss_rows), " classes:", nrow(ct), "\n")
print(w$events_sim |> group_by(peril) |> summarise(events = n(), gross_bn = round(sum(gross) / 1e9, 2), largest_bn = round(max(gross) / 1e9, 3), max_classes = max(n_classes)))
print(w$events_sim |> arrange(desc(gross)) |> head(4) |> select(event_id, peril, n_classes, n_regions, gross))
cat("BRIDGE  classes/treaty:", paste(br$classes_per_treaty, collapse = "/"), " treaties/class:", paste(br$treaties_per_class, collapse = "/"), " uncovered classes:", br$n_uncovered_classes, "\n")

cat("\nINURING TABLES\n")
re <- data$edges_df; rin <- sort(table(re$to_treaty_id), decreasing = TRUE); rout <- sort(table(re$from_treaty_id), decreasing = TRUE)
cat(sprintf("generator edges_df raw (layer level): %d edges, in-degree max %d, out-degree max %d; top in-degree: %s\n", nrow(re), max(rin), max(rout),
            paste(sprintf("%s [%s] %d", t$name[match(names(rin)[1:3], t$treaty_id)], t$treaty_type[match(names(rin)[1:3], t$treaty_id)], as.integer(rin[1:3])), collapse = "; ")))
for (lab in c("generator", "local")) {
  tb <- if (lab == "generator") w$inuring_generator else w$inuring_local
  st <- inuring_stats(tb, data); ic <- interaction_coverage(w, tb)
  cat(sprintf("%s: %d edges over %d units | in-degree min/median/mean/max %s | out-degree %s | overlapping pairs %d, with edge %d (%.0f%%), without %d | edges between treaties that share no class %d\n",
              lab, st$n_edges, st$n_units, paste(st$in_degree, collapse = "/"), paste(st$out_degree, collapse = "/"), ic$n_interacting, ic$n_with_edge,
              100 * ic$n_with_edge / ic$n_interacting, ic$n_without_edge, ic$edges_without_overlap))
  if (lab == "generator") { print(st$top_in_degree); print(ic$top_missing_type_pairs) }
}
lobw <- w; lobw$bridge <- w$bridge |> left_join(ct[c("class_id", "lob_id")], by = "class_id") |> transmute(treaty_id, class_id = lob_id) |> distinct()
icl <- interaction_coverage(lobw, w$inuring_generator, lobw$bridge)
cat(sprintf("generator edges at LOB-footprint level (regions ignored): %d overlapping pairs, %d with edge (%.1f%%)\n", icl$n_interacting, icl$n_with_edge, 100 * icl$n_with_edge / icl$n_interacting))
print(compare_inuring(w$inuring_generator, w$inuring_local))

cat("\nCONTRAST: generator table vs complete (repair) table, same losses\n")
ct_ <- contrast_inuring(w, w$inuring_generator, w$inuring_local, c("generator", "local"))
print(ct_$over_recovery); print(ct_$ceded_by_type |> mutate(across(where(is.numeric), ~ round(.x / 1e9, 3)))); print(ct_$total |> mutate(across(where(is.numeric), ~ round(.x / 1e9, 3))))
ov <- outcome_violations(gen_res)
print(head(ov$events, 4)); print(head(ov$rows |> select(event_id, class_id, risk_id, gross_loss, ceded_total, over_amount), 3), width = 200); print(ov$pairs |> head(6), width = 200)
cat(sprintf("harmful unmatched pairs %d of %d overlapping pairs without an edge; top harmful: %s\n", dg$n_without_edge_harmful, dg$n_without_edge,
            paste(head(sprintf("%s + %s (%d rows)", dg$harmful_pairs$name_a, dg$harmful_pairs$name_b, dg$harmful_pairs$n_negative_rows), 2), collapse = "; ")))

an <- loss_anatomy2(primary)
cat("\nEXAMPLE EVENTS:\n"); for (k in names(an$examples)) { e <- an$examples[[k]]
  cat(sprintf("%-13s %s %s | %d classes, %d risks | gross %s net %s | %d treaties see, %d pay\n", k, e$event$event_id, e$event$peril, e$event$n_classes, e$event$n_risks, bn(e$event$gross), bn(e$event$net), e$n_treaties_seeing, e$n_treaties_paying)) }
cat(sprintf("\nTOTAL %d FAIL(s)\n", fails))
if (fails > 0 && !interactive()) quit(status = 1)

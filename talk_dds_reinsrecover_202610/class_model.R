# class_model.R -- region x line-of-business loss classes, treaty filters, event simulation, risk split.
#
# Sourceable: defines functions only (no I/O, no library() calls on source, no global mutation).
# Needs dplyr, tibble, tidyr, readr (namespaced below). Everything is deterministic given `seed`:
# each component draws from its own RNG stream (seed + offset) and the caller's .Random.seed is
# restored afterwards, so results do not depend on call order.
#
# Model (a local, self-contained version of generator issues #205-#211 on
# kaybenleroll/construct_synthetic_reinsurance_data):
#   LOSS CLASS = region x line of business. Peril is NOT part of the key; it is an event attribute
#     that decides the footprint (which regions and lines are hit).
#   EVENT      = losses in many classes at once.
#   TREATY     = a filter (set of lines x set of regions) -> (treaty, class) bridge; many-to-many.
#   RISK       = each (event, class) loss splits into individual risks (building/location level).
# See data_classes/README.md.

# ---- Small utilities ----------------------------------------------------------------------------
# Run `expr` under set.seed(seed) and restore the caller's RNG state afterwards.
with_seed <- function(seed, expr) {
  g <- globalenv()
  had <- exists(".Random.seed", envir = g, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = g, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = g)
    else if (exists(".Random.seed", envir = g, inherits = FALSE)) rm(".Random.seed", envir = g)
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", sample.kind = "Rejection")
  expr
}

# Stable string hash -> uniform in [0, 1). Same key, same salt, same number, on any machine.
hash_unif <- function(key, salt = "") {
  vapply(paste0(salt, "|", key), function(s) {
    h <- 5381
    for (ch in utf8ToInt(s)) h <- (h * 33 + ch) %% 2147483647
    for (k in 1:3) h <- (h * 48271) %% 2147483647
    h / 2147483647
  }, numeric(1), USE.NAMES = FALSE)
}

read_exposure <- function(path = "data_classes/exposure_df.csv")
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

# ---- Dimension tables -----------------------------------------------------------------------------
region_table <- function() {
  tibble::tibble(
    region = c("North America", "Europe", "UK & Ireland", "Asia Pacific", "Latin America", "Middle East & Africa"),
    region_code = c("NA", "EU", "UK", "APAC", "LATAM", "MEA"),
    weight = c(0.35, 0.22, 0.12, 0.16, 0.06, 0.09),
    adjacent = list(c("Latin America", "Europe"), c("UK & Ireland", "Middle East & Africa", "North America"),
                    c("Europe"), c("Middle East & Africa"), c("North America"),
                    c("Europe", "Asia Pacific")))
}

# Broad group from the generator's fine-grained lob_group.
broad_group <- function(lob_group)
  dplyr::case_when(grepl("^property", lob_group) ~ "property", grepl("^casualty", lob_group) ~ "casualty",
                   grepl("^specialty", lob_group) ~ "specialty", TRUE ~ "marine_energy")

# "Property Cat (P001 2.1)" -> "Property Cat 2.1"
lob_short_name <- function(name) sub("\\s*\\(P001 ([0-9.]+)\\)", " \\1", name)

# One row per class: each line operates in a deterministic subset of 2-5 regions (sparse table).
# Exposure of a line (premium, expected losses by component, from the generator's exposure_df) is
# split across its regions in proportion to the region weights.
class_table <- function(lob_df, seed = 2003, exposure_df = read_exposure()) {
  rg <- region_table()
  lobs <- lob_df[order(lob_df$lob_id, method = "radix"), ]
  lobs$group <- broad_group(lobs$lob_group)
  lobs$lob <- lob_short_name(lobs$name)
  k_prob <- list(property = c(0.0, 0.25, 0.45, 0.30), casualty = c(0.30, 0.40, 0.25, 0.05),
                 specialty = c(0.30, 0.40, 0.25, 0.05), marine_energy = c(0.25, 0.40, 0.25, 0.10))
  picks <- with_seed(seed + 11L, lapply(seq_len(nrow(lobs)), function(i) {
    k <- sample(2:5, 1, prob = k_prob[[lobs$group[i]]])
    sample(rg$region, k, prob = rg$weight)
  }))
  cls <- tibble::tibble(lob_id = rep(lobs$lob_id, lengths(picks)), region = unlist(picks)) |>
    dplyr::left_join(lobs |> dplyr::select(lob_id, segment_id, lob, lob_group, group), by = "lob_id") |>
    dplyr::left_join(rg |> dplyr::select(region, weight), by = "region") |>
    dplyr::group_by(lob_id) |> dplyr::mutate(exposure_share = weight / sum(weight)) |> dplyr::ungroup() |>
    dplyr::left_join(exposure_df, by = "lob_id") |>
    dplyr::mutate(class_id = paste(lob, region, sep = " | "),
                  premium = premium * exposure_share, expected_loss = expected_loss_total * exposure_share,
                  el_attritional = expected_loss_attritional * exposure_share,
                  el_large = expected_loss_large * exposure_share, el_cat = expected_loss_cat * exposure_share,
                  el_peril = el_large + el_cat) |>
    dplyr::select(class_id, region, lob_id, lob, lob_group, group, segment_id, exposure_share,
                  premium, expected_loss, el_attritional, el_large, el_cat, el_peril)
  cls[order(cls$group, cls$lob, cls$region, method = "radix"), ]
}

# ---- Treaty filters and the bridge ---------------------------------------------------------------
# Filter = (set of lines) x (set of regions), derived from the treaty's footprint (coverage_df for
# xol_per_risk / xol_per_occurrence / clash) or declared scope (lob, segment, group). About 35% of
# treaties other than quota_share / group_quota_share / ilw get a regional restriction of 1-3 regions
# (chosen among the regions where their lines operate), decided by hashing the tower id (or treaty id),
# so all layers of a tower always share one filter.
treaty_filters <- function(treaty_df, coverage_df, class_table, seed = 2003, restrict_frac = 0.35) {
  unit_key <- dplyr::coalesce(treaty_df$tower_id, treaty_df$treaty_id)
  fp_types <- c("xol_per_risk", "xol_per_occurrence", "clash")
  all_lobs <- unique(class_table$lob_id)
  lobs_of <- function(i) {
    t <- treaty_df[i, ]
    if (t$treaty_type %in% fp_types) return(sort(unique(coverage_df$lob_id[coverage_df$treaty_id == t$treaty_id])))
    switch(t$scope_type, lob = t$scope_id,
           segment = sort(unique(class_table$lob_id[class_table$segment_id == t$scope_id])), group = sort(all_lobs))
  }
  lob_set <- lapply(seq_len(nrow(treaty_df)), lobs_of)
  # lines of a tower come from its first layer so that every layer has the same filter
  first_of <- match(unit_key, unit_key)
  lob_set <- lob_set[first_of]
  restrictable <- !treaty_df$treaty_type %in% c("quota_share", "group_quota_share", "ilw")
  region_set <- lapply(seq_len(nrow(treaty_df)), function(i) {
    cand <- sort(unique(class_table$region[class_table$lob_id %in% lob_set[[i]]]))
    if (!restrictable[i] || length(cand) < 2 || hash_unif(unit_key[i], paste0(seed, "restrict")) >= restrict_frac)
      return(cand)
    n <- min(1L + as.integer(floor(3 * hash_unif(unit_key[i], paste0(seed, "nreg")))), length(cand) - 1L)
    cand[order(hash_unif(paste(unit_key[i], cand), paste0(seed, "pick")))][seq_len(n)] |> sort()
  })
  region_set <- region_set[first_of]
  tibble::tibble(treaty_id = treaty_df$treaty_id, unit_key = unit_key, treaty_type = treaty_df$treaty_type,
                 lob_set = lob_set, region_set = region_set,
                 n_lobs = lengths(lob_set), n_regions = lengths(region_set),
                 restricted = vapply(seq_along(region_set), function(i)
                   length(region_set[[i]]) < length(unique(class_table$region[class_table$lob_id %in% lob_set[[i]]])), TRUE))
}

# Expand filters against the class table: one row per (treaty, class). Treaties <-> classes is many-to-many.
treaty_class_bridge <- function(filters, class_table) {
  ft <- filters |> dplyr::select(treaty_id, lob_set) |> tidyr::unnest(lob_set) |> dplyr::rename(lob_id = lob_set)
  fr <- filters |> dplyr::select(treaty_id, region_set) |> tidyr::unnest(region_set) |> dplyr::rename(region = region_set)
  b <- ft |> dplyr::inner_join(fr, by = "treaty_id", relationship = "many-to-many") |>
    dplyr::inner_join(class_table |> dplyr::select(class_id, lob_id, region), by = c("lob_id", "region"))
  b <- b[order(b$treaty_id, b$class_id, method = "radix"), c("treaty_id", "class_id")]
  tibble::as_tibble(b)
}

# Distribution report: classes per treaty, treaties per class, uncovered classes.
bridge_report <- function(bridge, treaty_df, class_table) {
  ct <- dplyr::count(bridge, treaty_id, name = "n_classes")
  ct <- ct[match(treaty_df$treaty_id, ct$treaty_id), ]
  tc <- dplyr::count(bridge, class_id, name = "n_treaties")
  tc <- dplyr::left_join(class_table["class_id"], tc, by = "class_id") |> dplyr::mutate(n_treaties = dplyr::coalesce(n_treaties, 0L))
  q <- function(x) c(min = min(x), median = stats::median(x), max = max(x))
  list(classes_per_treaty = q(ct$n_classes), treaties_per_class = q(tc$n_treaties),
       n_classes = nrow(class_table), n_uncovered_classes = sum(tc$n_treaties == 0),
       n_treaties_without_class = sum(is.na(ct$n_classes)), uncovered = tc$class_id[tc$n_treaties == 0])
}

# ---- Perils ---------------------------------------------------------------------------------------
# freq: events per period (Poisson). int_mean: relative mean size of one event of the peril (only the
# ratios between perils matter: class losses are normalised so that every class has the same expected
# annual peril loss, see simulate_events()). int_sigma: lognormal sigma (heavy tail); int_cap: tail
# truncation in multiples of int_mean. max_regions: footprint size; adjacent: natural-cat footprints
# grow into neighbouring regions.
peril_table <- function() {
  tibble::tribble(
    ~peril,       ~freq, ~int_mean, ~int_sigma, ~int_cap, ~max_regions, ~adjacent,
    "Windstorm",    1.5,   1.00,      1.8,       60,      3L,          TRUE,
    "Earthquake",   0.6,   1.20,      2.0,       60,      2L,          TRUE,
    "Flood",        1.2,   0.80,      1.7,       40,      2L,          TRUE,
    "Fire",        14.0,   0.25,      1.3,       15,      1L,          FALSE,
    "Marine",       5.0,   0.30,      1.4,       15,      1L,          FALSE,
    "Liability",    6.0,   0.40,      1.4,       15,      2L,          FALSE,
    "Cyber",        2.0,   0.60,      1.6,       20,      2L,          FALSE)
}

# Which lines a peril hits: hit = probability the line is touched (given its region is in the
# footprint), sev = relative severity on that line.
peril_vulnerability <- function() {
  tibble::tribble(
    ~peril,       ~lob_group,                          ~hit, ~sev,
    "Windstorm",  "property_cat",                      1.0,  1.0,
    "Windstorm",  "property_noncat",                   1.0,  0.7,
    "Windstorm",  "energy_onshore",                    0.5,  0.3,
    "Windstorm",  "marine_cargo",                      0.3,  0.2,
    "Windstorm",  "aviation",                          0.2,  0.1,
    "Earthquake", "property_cat",                      1.0,  1.2,
    "Earthquake", "property_noncat",                   1.0,  0.9,
    "Earthquake", "energy_onshore",                    0.7,  0.6,
    "Earthquake", "energy_offshore",                   0.2,  0.3,
    "Earthquake", "marine_cargo",                      0.5,  0.3,
    "Earthquake", "casualty_workers_comp",             0.6,  0.5,
    "Earthquake", "specialty_credit",                  0.3,  0.1,
    "Flood",      "property_cat",                      1.0,  0.8,
    "Flood",      "property_noncat",                   1.0,  0.8,
    "Flood",      "casualty_auto",                     0.6,  0.3,
    "Flood",      "marine_cargo",                      0.5,  0.3,
    "Flood",      "energy_onshore",                    0.4,  0.3,
    "Fire",       "property_noncat",                   1.0,  1.0,
    "Fire",       "property_cat",                      0.3,  0.4,
    "Fire",       "energy_onshore",                    0.5,  0.9,
    "Marine",     "marine_cargo",                      1.0,  1.0,
    "Marine",     "energy_offshore",                   0.5,  0.9,
    "Marine",     "aviation",                          0.3,  0.8,
    "Liability",  "casualty_liability",                1.0,  1.0,
    "Liability",  "casualty_auto",                     0.5,  0.6,
    "Liability",  "casualty_workers_comp",             0.5,  0.5,
    "Liability",  "specialty_professional_indemnity",  0.6,  0.6,
    "Liability",  "specialty_directors_officers",      0.5,  0.8,
    "Cyber",      "specialty_cyber",                   1.0,  1.2,
    "Cyber",      "specialty_professional_indemnity",  0.6,  0.5,
    "Cyber",      "specialty_directors_officers",      0.5,  0.5,
    "Cyber",      "specialty_credit",                  0.4,  0.4,
    "Cyber",      "casualty_liability",                0.3,  0.3)
}

# ---- Events ---------------------------------------------------------------------------------------
# Regions hit by one peril event: an anchor region (weighted by region weight), grown into adjacent
# regions for natural-cat perils, else extra regions drawn by weight.
pick_footprint_regions <- function(rg, p) {
  anchor <- sample(rg$region, 1, prob = rg$weight)
  n <- if (p$max_regions == 1L) 1L else sample(seq_len(p$max_regions), 1, prob = rep(1, p$max_regions) * rev(seq_len(p$max_regions)))
  if (n == 1L) return(anchor)
  if (p$adjacent) {
    regs <- anchor
    while (length(regs) < n) {
      cand <- setdiff(unlist(rg$adjacent[match(regs, rg$region)]), regs)
      if (!length(cand)) break
      regs <- c(regs, sample(cand, 1))
    }
    regs
  } else {
    r2 <- setdiff(rg$region, anchor)
    c(anchor, sample(r2, n - 1L, prob = rg$weight[match(r2, rg$region)]))
  }
}

# P(region in footprint) per peril, by Monte Carlo over the footprint rule (stream seed + 24).
footprint_region_prob <- function(rg, perils, seed, n_mc = 1500) {
  with_seed(seed + 24L, do.call(rbind, lapply(seq_len(nrow(perils)), function(k) {
    d <- replicate(n_mc, rg$region %in% pick_footprint_regions(rg, perils[k, ]))
    stats::setNames(rowMeans(d), rg$region)
  })))
}

# Calibration constant per class: D_c = sum over perils of freq x int_mean x P(class hit) x sev. Dividing
# a class's losses by D_c makes its expected annual peril loss equal peril_ratio x (expected large + cat
# loss of the generator's exposure_df) whatever the footprint rules are.
class_norm <- function(ct, perils, vuln, seed) {
  rg <- region_table()
  rp <- footprint_region_prob(rg, perils, seed)
  contrib <- vapply(seq_len(nrow(perils)), function(k) {
    v <- vuln[vuln$peril == perils$peril[k], ]
    vi <- match(ct$lob_group, v$lob_group)
    ifelse(is.na(vi), 0, v$hit[vi] * v$sev[vi]) * rp[k, ct$region] * perils$freq[k] * perils$int_mean[k]
  }, numeric(nrow(ct)))
  rowSums(matrix(contrib, nrow = nrow(ct)))
}

# Simulate n_periods of events. Returns list(events, class_losses):
#   events: one row per event (event_id, period_id, event_seq, peril, kind, n_classes, n_regions, gross)
#   class_losses: one row per (event, class) with class_gross.
# Streams: seed+21 background events, seed+22 peril events, seed+23 event timing.
simulate_events <- function(class_table, n_periods = 10, seed = 2003, perils = peril_table(),
                            vuln = peril_vulnerability(), bg_mean = 2.5e6, bg_sigma = 1.1, noise_sigma = 0.35,
                            peril_ratio = 1.3, loss_scale = 1) {
  rg <- region_table()
  per_ids <- sprintf("P%03d", seq_len(n_periods))
  ct <- class_table
  norm_c <- class_norm(ct, perils, vuln, seed)
  # --- background stream: single-class small losses ---
  bg <- with_seed(seed + 21L, {
    rate <- pmax(2, ct$el_attritional / bg_mean)
    g <- tidyr::expand_grid(ci = seq_len(nrow(ct)), pi = seq_len(n_periods))
    g$n <- stats::rpois(nrow(g), rate[g$ci])
    g <- g[rep(seq_len(nrow(g)), g$n), ]
    mu <- log(ct$el_attritional[g$ci] / rate[g$ci]) - bg_sigma^2 / 2
    tibble::tibble(period_id = per_ids[g$pi], peril = "Attritional", kind = "background", ci = g$ci,
                   class_gross = loss_scale * stats::rlnorm(nrow(g), mu, bg_sigma))
  })
  # --- peril events ---
  pe <- with_seed(seed + 22L, {
    ev <- do.call(rbind, lapply(seq_len(nrow(perils)), function(k) {
      n <- stats::rpois(n_periods, perils$freq[k])
      data.frame(peril = perils$peril[k], pi = rep(seq_len(n_periods), n))
    }))
    ev$eid <- seq_len(nrow(ev))
    rows <- lapply(seq_len(nrow(ev)), function(j) {
      p <- perils[perils$peril == ev$peril[j], ]
      v <- vuln[vuln$peril == p$peril, ]
      regs <- pick_footprint_regions(rg, p)
      cand <- which(ct$region %in% regs & ct$lob_group %in% v$lob_group)
      if (!length(cand)) return(NULL)
      vi <- match(ct$lob_group[cand], v$lob_group)
      hit <- stats::runif(length(cand)) < v$hit[vi]
      if (!any(hit)) hit[which.max(v$sev[vi] * ct$el_peril[cand])] <- TRUE
      cand <- cand[hit]; sev <- v$sev[vi][hit]
      inten <- min(stats::rlnorm(1, log(p$int_mean) - p$int_sigma^2 / 2, p$int_sigma), p$int_cap * p$int_mean)
      noise <- stats::rlnorm(length(cand), -noise_sigma^2 / 2, noise_sigma)
      data.frame(eid = ev$eid[j], pi = ev$pi[j], peril = p$peril, ci = cand,
                 class_gross = loss_scale * peril_ratio * inten * sev * ct$el_peril[cand] / norm_c[cand] * noise)
    })
    out <- do.call(rbind, rows)
    tibble::tibble(period_id = per_ids[out$pi], peril = out$peril, kind = "peril", ci = out$ci,
                   class_gross = out$class_gross, tmp = out$eid)
  })
  bg$tmp <- NA_integer_
  bg$tmp <- 1e6L + seq_len(nrow(bg))      # each background loss is its own event
  all <- dplyr::bind_rows(pe, bg)
  # --- timing: each event gets a uniform time within the period; event_seq = rank by (time, tmp) ---
  ev0 <- all |> dplyr::distinct(tmp, period_id, peril, kind)
  ev0 <- ev0[order(ev0$tmp, method = "radix"), ]
  ev0$u <- with_seed(seed + 23L, stats::runif(nrow(ev0)))
  ev0 <- ev0[order(ev0$period_id, ev0$u, ev0$tmp, method = "radix"), ]
  ev0$event_seq <- stats::ave(seq_len(nrow(ev0)), ev0$period_id, FUN = seq_along)
  ev0$event_id <- sprintf("%s-E%04d", ev0$period_id, ev0$event_seq)
  cl <- all |> dplyr::left_join(ev0 |> dplyr::select(tmp, event_id, event_seq), by = "tmp") |>
    dplyr::mutate(class_id = ct$class_id[ci], region = ct$region[ci], lob_id = ct$lob_id[ci],
                  lob_group = ct$lob_group[ci], group = ct$group[ci], class_premium = ct$premium[ci]) |>
    dplyr::select(event_id, period_id, event_seq, peril, kind, class_id, region, lob_id, lob_group, group,
                  class_premium, class_gross)
  cl <- cl[order(cl$period_id, cl$event_seq, cl$class_id, method = "radix"), ]
  events <- cl |> dplyr::group_by(event_id, period_id, event_seq, peril, kind) |>
    dplyr::summarise(n_classes = dplyr::n(), n_regions = dplyr::n_distinct(region), gross = sum(class_gross), .groups = "drop")
  events <- events[order(events$period_id, events$event_seq, method = "radix"), ]
  list(events = events, class_losses = cl)
}

# ---- Risks ----------------------------------------------------------------------------------------
# Each (event, class) loss splits into n risks with heavy-tailed (lognormal) weights. n grows with
# the loss and with the class premium, capped at max_risks; background losses get 1-3 risks.
# Returns loss rows: event_id, period_id, event_seq, peril, kind, class_id, region, lob_id, lob_group,
# group, risk_id, gross_loss.
split_into_risks <- function(class_losses, seed = 2003, max_risks = 40L, risk_sigma = 1.4, loss_per_risk = 1.5e6) {
  cl <- class_losses[order(class_losses$period_id, class_losses$event_seq, class_losses$class_id, method = "radix"), ]
  med_prem <- stats::median(cl$class_premium)
  out <- with_seed(seed + 31L, {
    n <- ifelse(cl$kind == "background", 1L + stats::rbinom(nrow(cl), 2, 0.35),
                as.integer(pmin(max_risks, pmax(2, ceiling(sqrt(cl$class_gross / loss_per_risk) * (cl$class_premium / med_prem)^0.3)))))
    rep_i <- rep(seq_len(nrow(cl)), n)
    w <- stats::rlnorm(length(rep_i), 0, risk_sigma)
    list(rep_i = rep_i, w = w, k = sequence(n))
  })
  tot_w <- as.vector(rowsum(out$w, out$rep_i))
  gross <- cl$class_gross[out$rep_i] * out$w / tot_w[out$rep_i]
  cls_no <- as.integer(factor(paste(cl$event_id, cl$class_id), levels = unique(paste(cl$event_id, cl$class_id))))
  r <- cl[out$rep_i, c("event_id", "period_id", "event_seq", "peril", "kind", "class_id", "region", "lob_id", "lob_group", "group")]
  r$risk_id <- sprintf("%s-C%02d-R%02d", r$event_id, stats::ave(cls_no[out$rep_i], r$event_id, FUN = function(x) match(x, unique(x))), out$k)
  r$gross_loss <- gross
  tibble::as_tibble(r)
}

# Convenience: class table -> events -> loss rows in one call.
simulate_loss_rows <- function(class_table, n_periods = 10, seed = 2003, ...) {
  sim <- simulate_events(class_table, n_periods, seed, ...)
  list(events = sim$events, class_losses = sim$class_losses, loss_rows = split_into_risks(sim$class_losses, seed))
}

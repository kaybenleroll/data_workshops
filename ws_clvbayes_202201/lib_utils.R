resolve_conflicts <- function(pkg_priority) {
  get_index <- function(pkg_name) {
    idx <- str_which(pkg_priority, pkg_name)

    if(length(idx) == 0) {
      idx <- 0L
    }

    return(idx)
  }

  conflict_lst <- conflict_scout()

  for(func_name in names(conflict_lst)) {
    pkg_index <- map_int(conflict_lst[[func_name]], get_index)

    pkg_index <- pkg_index[pkg_index > 0]

    if(length(pkg_index) == 0) {
      pkg_use <- conflict_lst[[func_name]][1]
    } else {
      pkg_use <- pkg_index %>%
        min() %>%
        pkg_priority[.]

    }

    conflict_prefer(func_name, pkg_use)
  }

  return(conflict_lst)
}


calculate_distribution_qvals <- function(data_tbl, distrib_vals, ref_val, ...) {
  qval_data_tbl <- data_tbl %>%
    group_by(..., {{ ref_val }}) %>%
    summarise(
      .groups = "drop",

      dval_lst = list({{ distrib_vals }})
    ) %>%
    mutate(
      q_val = map2_dbl(dval_lst, {{ref_val}}, ~ ecdf(.x)(.y))
    )

  return(qval_data_tbl)
}


calculate_medical_summary_stats <- function(population_tbl) {

  n_truepos <- population_tbl %>%
    filter(infected == TRUE, test_result == TRUE) %>%
    nrow()

  n_trueneg <- population_tbl %>%
    filter(infected == FALSE, test_result == FALSE) %>%
    nrow()

  n_falsepos <- population_tbl %>%
    filter(infected == FALSE, test_result == TRUE) %>%
    nrow()

  n_falseneg <- population_tbl %>%
    filter(infected == TRUE, test_result == FALSE) %>%
    nrow()


  prop_falsepos <- n_falsepos / (n_falsepos + n_truepos)
  prop_falseneg <- n_falseneg / (n_falseneg + n_trueneg)

  summstat_lst <- list(
    prop_falsepos = prop_falsepos,
    prop_falseneg = prop_falseneg
    )

  return(summstat_lst)
}


calculate_transaction_summary_stats <- function(data_tbl) {
  tnx_stats_tbl <- data_tbl %>%
  group_by(customer_id) %>%
    summarise(
      .groups = "drop",

      tnx_count    = n(),
      first_tnx_ts = min(tnx_timestamp),
      last_tnx_ts  = max(tnx_timestamp),
      btyd_count   = tnx_count - 1,
      all_weeks    = 52,
      tnx_weeks    = difftime(last_tnx_ts, first_tnx_ts, units = "weeks") %>% as.numeric(),
      tnx_weeks    = if_else(btyd_count > 0, tnx_weeks, 52),
      obs_freq     = btyd_count / tnx_weeks,
      emp_freq     = btyd_count / all_weeks
    )

  return(tnx_stats_tbl)
}


gamma_mucv2shaperate <- function(mu, cv) {
  shape <- 1 / (cv^2)
  rate  <- 1 / (cv^2 * mu)

  return(c(shape = shape, rate = rate))
}


gamma_shaperate2mucv <- function(shape, rate) {
  mu <- shape / rate
  cv <- 1 / sqrt(shape)

  return(c(mu = mu, cv = cv))
}


rgamma_mucv <- function(n, mu, cv, ...) {
  params <- gamma_mucv2shaperate(mu, cv)

  rgamma(n = n, shape = params[1], rate = params[2], ...)
}


create_posterior_validation_data <- function(stanfit, data_tbl, simparams_tbl, bincount = 50) {

  validation_tbl <- stanfit %>%
    recover_types(data_tbl) %>%
    spread_draws(lambda[customer_id], mu[customer_id]) %>%
    ungroup() %>%
    inner_join(simparams_tbl, by = "customer_id") %>%
    select(
      customer_id, draw_id = .draw, post_lambda = lambda, customer_lambda,
      post_mu = mu, customer_mu
      )

  validation_tbl %>% glimpse()


  tmp_tbl <- validation_tbl %>%
    calculate_distribution_qvals(post_lambda, customer_lambda, customer_id)

  qvalues_tbl <- validation_tbl %>%
    calculate_distribution_qvals(post_mu, customer_mu, customer_id) %>%
    rename(qval_mu = q_val) %>%
    inner_join(tmp_tbl, by = "customer_id") %>%
    select(
      customer_id, customer_lambda, qval_lambda = q_val, customer_mu, qval_mu
      )


  unif_count <- qvalues_tbl %>%
    nrow() %>%
    divide_by(bincount)

  lambda_qval_plot <- ggplot(qvalues_tbl) +
    geom_histogram(aes(x = qval_lambda), bins = bincount) +
    geom_hline(aes(yintercept = unif_count), colour = "red") +
    labs(
      x = "q-Values",
      y = "Frequency",

      title = "Histogram of the q-Values for Lambda"
      )

  mu_qval_plot <- ggplot(qvalues_tbl) +
    geom_histogram(aes(x = qval_mu), bins = bincount) +
    geom_hline(aes(yintercept = unif_count), colour = "red") +
    labs(
      x = "q-Values",
      y = "Frequency",

      title = "Histogram of the q-Values for Mu"
      )


  valid_lst <- list(
    validation_tbl   = validation_tbl,
    qvalues_tbl      = qvalues_tbl,

    mu_qval_plot     = mu_qval_plot,
    lambda_qval_plot = lambda_qval_plot
    )

  return(valid_lst)
}


calculate_transaction_cbs_data <- function(tnx_data_tbl, last_date) {
  cbs_data_tbl <- tnx_data_tbl %>%
    filter(tnx_timestamp <= last_date) %>%
    group_by(customer_id) %>%
    summarise(
      .groups = "drop",

      first_tnx_date = min(tnx_timestamp),
      last_tnx_date  = max(tnx_timestamp),

      x     = n() - 1,
      t_x   = difftime(last_tnx_date, first_tnx_date, units = "weeks") %>% as.numeric(),
      T_cal = difftime(last_date,     first_tnx_date, units = "weeks") %>% as.numeric()
    )

  return(cbs_data_tbl)
}


validate_frequency_model <- function(freqmodel_stanfit, input_data_tbl) {

  freqmodel_valid_tbl <- freqmodel_stanfit %>%
    recover_types(input_data_tbl) %>%
    spread_draws(lambda[customer_id]) %>%
    ungroup() %>%
    inner_join(input_data_tbl, by = "customer_id") %>%
    transmute(
      customer_id     = customer_id,
      draw_id         = .draw,
      customer_lambda = customer_lambda,
      post_lambda     = lambda,
      pred_count      = rpois(n(), lambda = lambda * tnx_window),
      obs_count       = obs_count
      ) %>%
    replace_na(list(obs_count = 0))


  return(freqmodel_valid_tbl)
}


create_pnbd_posterior_validation_data <- function(stanfit, data_tbl, simparams_tbl, bincount = 50) {

  validation_tbl <- stanfit %>%
    recover_types(data_tbl) %>%
    spread_draws(lambda[customer_id], mu[customer_id], p_alive[customer_id]) %>%
    ungroup() %>%
    inner_join(simparams_tbl, by = "customer_id") %>%
    select(
      customer_id, draw_id = .draw, post_lambda = lambda, customer_lambda,
      post_mu = mu, customer_mu, p_alive
    )

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


generate_pnbd_validation_transactions <- function(p_alive, lambda, mu, tnx_mu, tnx_cv, start_dttm, end_dttm) {

  customer_active <- rbernoulli(n = 1, p = p_alive)

  if(customer_active) {
    tau <- rexp(n = 1, rate = mu)

    tnx_intervals <- calculate_event_times(
      rate       = lambda,
      total_time = tau,
      block_size = 1000
      )

    event_dates <- start_dttm + (cumsum(tnx_intervals) * (7 * 24 * 60 * 60))

    tnx_amounts <- rgamma_mucv(length(event_dates), mu = tnx_mu, cv = tnx_cv)

    tnxdata_tbl <- tibble(
        tnx_timestamp = event_dates,
        tnx_amount    = tnx_amounts %>% round(2)
        ) %>%
      filter(
        tnx_timestamp <= end_dttm
        )

  } else {
    tnxdata_tbl <- tibble(
        tnx_timestamp = start_dttm,
        tnx_amount    = 0
        ) %>%
      slice(0)
  }


  return(tnxdata_tbl)
}

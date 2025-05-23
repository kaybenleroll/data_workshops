calculate_event_times <- function(rate, total_time, block_size = 100) {
  sample_vec <- c()

  sample_complete <- FALSE

  while(!sample_complete) {
    block_sample <- rexp(block_size, rate = rate)

    sample_vec <- c(sample_vec, block_sample)

    cuml_value <- cumsum(sample_vec)

    sample_complete <- any(cuml_value > total_time)
  }

  event_times <- sample_vec[cumsum(sample_vec) < total_time]

  return(event_times)
}


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


create_pnbd_simulation_validation_data <- function(validsims_tbl, obsdata_tbl) {
  validation_tbl <- validsims_tbl %>%
    group_by(customer_id) %>%
    summarise(
      .groups = "drop",

      sim_tnx_counts = list(sim_tnx_count),
      sim_last_tnx   = list(sim_tnx_last)
      ) %>%
    left_join(obs_2019_id1000_stats_tbl, by = "customer_id") %>%
    replace_na(list(tnx_count = 0))


  return(validation_tbl)
}


generate_pnbd_validation_transactions <- function(p_alive, lambda, mu, tnx_mu, tnx_cv, start_dttm, end_dttm) {

  customer_active <- rbernoulli(n = 1, p = p_alive)

  max_obs <- difftime(end_dttm, start_dttm, units = "weeks")


  if(customer_active) {
    tau <- rexp(n = 1, rate = mu)

    obs_time <- min(tau, max_obs)

    tnx_intervals <- calculate_event_times(
      rate       = lambda,
      total_time = obs_time,
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


generate_bgnbd_validation_transactions <- function(lambda, p, tnx_mu, tnx_cv, start_dttm, end_dttm) {

  customer_active <- rbernoulli(n = 1, p = 1 - p)

  max_obs <- difftime(end_dttm, start_dttm, units = "weeks")


  if(customer_active) {
    tnx_intervals <- calculate_event_times(
      rate       = lambda,
      total_time = max_obs,
      block_size = 1000
    )

    goes_inactive <- rbernoulli(n = length(tnx_intervals), p = p)

    if(any(goes_inactive)) {
      inactive_idx <- which(goes_inactive)[1]

      tnx_intervals <- tnx_intervals[1:inactive_idx]
    }

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



run_pnbd_simulations_chunk <- function(
    sim_file, param_tbl, start_dttm = as.POSIXct("2019-01-01"),
    end_dttm   = as.POSIXct("2020-01-01")) {

  calc_file <- !file_exists(sim_file)

  if(calc_file) {
    simdata_tbl <- param_tbl %>%
      mutate(
        sim_data = pmap(
          list(
            p_alive  = p_alive,
            lambda   = post_lambda,
            mu       = post_mu
            ),
          generate_pnbd_validation_transactions,

          tnx_mu     = 1,
          tnx_cv     = 1,
          start_dttm = start_dttm,
          end_dttm   = end_dttm
          ),
        sim_tnx_count = map_int(sim_data, nrow),
        max_data = map(
          sim_data,
          ~ .x %>%
            slice_max(n = 1, order_by = tnx_timestamp, with_ties = FALSE) %>%
            select(sim_tnx_last = tnx_timestamp)
          )
        ) %>%
      unnest(max_data, keep_empty = TRUE)

    simdata_tbl %>% write_rds(sim_file)
  }


  return(calc_file)
}


run_bgnbd_simulations_chunk <- function(
    sim_file, param_tbl,
    start_dttm = as.POSIXct("2019-01-01"),
    end_dttm   = as.POSIXct("2020-01-01")) {

  calc_file <- !file_exists(sim_file)

  if(calc_file) {
    simdata_tbl <- param_tbl %>%
      mutate(
        sim_data = pmap(
          list(
            lambda   = post_lambda,
            p        = post_p
          ),
          generate_bgnbd_validation_transactions,

          tnx_mu     = 1,
          tnx_cv     = 1,
          start_dttm = start_dttm,
          end_dttm   = end_dttm
        ),
        sim_tnx_count = map_int(sim_data, nrow),
        max_data = map(
          sim_data,
          ~ .x %>%
            slice_max(n = 1, order_by = tnx_timestamp, with_ties = FALSE) %>%
            select(sim_tnx_last = tnx_timestamp)
        )
      ) %>%
      unnest(max_data, keep_empty = TRUE)

    simdata_tbl %>% write_rds(sim_file)
  }


  return(calc_file)
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


construct_model_validation_data <- function(
    btyd_stanfit, btyd_fitdata_tbl, btyd_obs_stats_tbl,
    precompute_dir, precompute_key, run_chunk_func) {


  model_type <- btyd_stanfit$output_files() %>%
    extract2(1) %>%
    basename() %>%
    str_replace("^fit_(.+?)_.*", "\\1")

  ## First we build the simulation input values
  if(model_type == "pnbd") {
    btyd_validation_tbl <- btyd_stanfit %>%
      recover_types(btyd_fitdata_tbl) %>%
      spread_draws(lambda[customer_id], mu[customer_id], p_alive[customer_id]) %>%
      ungroup() %>%
      select(
        customer_id, draw_id = .draw, post_lambda = lambda, post_mu = mu, p_alive
      )
  } else {
    btyd_validation_tbl <- btyd_stanfit %>%
      recover_types(btyd_fitdata_tbl) %>%
      spread_draws(lambda[customer_id], p[customer_id]) %>%
      ungroup() %>%
      select(
        customer_id, draw_id = .draw, post_lambda = lambda, post_p = p
        )
  }


  ## Our simulations are precomputed, so we want to exclude any files that
  ## already exist on the file system

  precomputed_tbl <- dir_ls(precompute_dir) %>%
    enframe(name = NULL, value = "sim_file") %>%
    mutate(sim_file = sim_file %>% as.character())


  btyd_validsims_lookup_tbl <- btyd_validation_tbl %>%
    group_nest(customer_id, .key = "cust_params") %>%
    mutate(
      sim_file = glue(
        "{precompute_dir}/{precompute_key}_{customer_id}.rds"
        )
      )


  exec_tbl <-  btyd_validsims_lookup_tbl %>%
    anti_join(precomputed_tbl, by = "sim_file")


  ## Assuming there are simulations missing, run those simulations.
  if(exec_tbl %>% nrow() > 0) {
    exec_tbl %>%
      mutate(
        calc_file = future_map2_lgl(
          sim_file, cust_params,
          run_chunk_func,

          .options = furrr_options(
            globals  = c(
              "calculate_event_times", "rgamma_mucv", "gamma_mucv2shaperate",
              "run_pnbd_simulations_chunk", "run_bgnbd_simulations_chunk",
              "generate_pnbd_validation_transactions",
              "generate_bgnbd_validation_transactions"
              ),
            packages   = c("tidyverse", "fs"),
            scheduling = FALSE,
            seed       = 4202
          ),
          .progress = TRUE
        )
      )
  }


  ## All files should now be generated, so read the output data into a table
  btyd_validsims_tbl <- btyd_validsims_lookup_tbl %>%
    mutate(
      data = map(sim_file, ~ .x %>% read_rds() %>% select(draw_id, sim_tnx_count, sim_tnx_last))
      ) %>%
    select(customer_id, sim_file, data) %>%
    unnest(data)


  ## Filtering on those customers that existed in the fitted data, calculate
  ## observed values for comparison purposes
  tnx_data_tbl <- btyd_obs_stats_tbl %>%
    semi_join(btyd_validsims_tbl, by = "customer_id")

  obs_customer_count  <- tnx_data_tbl %>% nrow()
  obs_total_tnx_count <- tnx_data_tbl %>% pull(tnx_count) %>% sum()

  btyd_tnx_simsumm_tbl <- btyd_validsims_tbl %>%
    group_by(draw_id) %>%
    summarise(
      .groups = "drop",

      sim_customer_count  = length(sim_tnx_count[sim_tnx_count > 0]),
      sim_total_tnx_count = sum(sim_tnx_count)
      )


  valid_custcount_plot <- ggplot(btyd_tnx_simsumm_tbl) +
    geom_histogram(aes(x = sim_customer_count), binwidth = 10) +
    geom_vline(aes(xintercept = obs_customer_count), colour = "red") +
    labs(
      x = "Simulated Customers With Transactions",
      y = "Frequency",
      title = "Histogram of Count of Customers Transacted",
      subtitle = "Observed Count in Red"
      )

  valid_tnxcount_plot <- ggplot(btyd_tnx_simsumm_tbl) +
    geom_histogram(aes(x = sim_total_tnx_count), binwidth = 50) +
    geom_vline(aes(xintercept = obs_total_tnx_count), colour = "red") +
    labs(
      x = "Simulated Transaction Count",
      y = "Frequency",
      title = "Histogram of Count of Total Transaction Count",
      subtitle = "Observed Count in Red"
      )


  valid_lst <- list(
    btyd_validation_tbl  = btyd_validation_tbl,
    btyd_validsims_tbl   = btyd_validsims_tbl,
    valid_custcount_plot = valid_custcount_plot,
    valid_tnxcount_plot  = valid_tnxcount_plot
    )

  return(valid_lst)
}




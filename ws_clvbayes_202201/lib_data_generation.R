

generate_customer_cohort_data <- function(n_customers, first_date, last_date) {
  tnx_dates <- seq(first_date, last_date, by = "day")

  customer_cohort_tbl <- tnx_dates %>%
    enframe(name = NULL, value = "first_tnx_date") %>%
    slice_sample(n = n_customers, replace = TRUE) %>%
    arrange(first_tnx_date) %>%
    group_by(format(first_tnx_date, "%Y%m")) %>%
    mutate(
      customer_id = sprintf("CUST%s-%04d", format(first_tnx_date, "%Y%m"), 1:n()),
      cohort_qtr  = first_tnx_date %>% as.yearqtr() %>% as.character(),
      cohort_ym   = first_tnx_date %>% format("%Y %m")
      ) %>%
    ungroup() %>%
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date)

  return(customer_cohort_tbl)
}




generate_pnbd_individual_transactions <- function(lifetime, tnx_rate, mx_nu, mx_p,
                                                  first_date, final_date = final_date) {

  obs_weeks <- difftime(final_date, first_date, units = "weeks") %>% as.numeric()

  tnx_window <- min(obs_weeks, lifetime)

  sim_count <- round(10 * (tnx_window * tnx_rate), 0) %>% max(10)

  event_times <- rexp(sim_count, rate = tnx_rate)

  tnx_intervals     <- cumsum(event_times)
  use_tnx_intervals <- tnx_intervals[tnx_intervals < tnx_window]


  first_tnx_dttm <- as.POSIXct(first_date) + runif(1, min = 0, max = 24 * 60 * 60 - 1)

  event_dates <- first_tnx_dttm + (use_tnx_intervals * (7 * 24 * 60 * 60))

  tnx_amounts <- rgamma(1 + length(event_dates), shape = mx_p, rate = mx_nu)

  sim_tnx_tbl <- tibble(
    tnx_timestamp = c(first_tnx_dttm, event_dates),
    tnx_amount    = tnx_amounts %>% round(2)
    )

  return(sim_tnx_tbl)
}


generate_pnbd_customer_simulation_params <- function(customer_cohort_data_tbl, params_lst) {
  mu_shape     <- params_lst$mu_shape
  mu_rate      <- params_lst$mu_rate
  lambda_shape <- params_lst$lambda_shape
  lambda_rate  <- params_lst$lambda_rate
  mx_q         <- params_lst$mx_q
  mx_g         <- params_lst$mx_g

  customer_basic_parameters_tbl <- customer_cohort_data_tbl %>%
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date) %>%
    arrange(first_tnx_date, customer_id) %>%
    mutate(
      customer_mu     = rgamma(n(), shape = mu_shape,     rate = mu_rate),
      customer_tau    = rexp(n(), rate = customer_mu),
      customer_lambda = rgamma(n(), shape = lambda_shape, rate = lambda_rate),
      customer_nu     = rgamma(n(), shape = mx_q,         rate = mx_g),
      customer_p      = params_lst$mx_p
    )

  return(customer_basic_parameters_tbl)
}


generate_pnbd_customer_transaction_data <- function(sim_params_tbl) {
  customer_transactions_tbl <- sim_params_tbl %>%
    mutate(
      sim_data = pmap(
        list(
          lifetime   = customer_tau,
          tnx_rate   = customer_lambda,
          mx_nu      = customer_nu,
          mx_p       = customer_p,
          first_date = first_tnx_date
        ),
        generate_pnbd_individual_transactions,
        final_date = final_date_observed
        )
      ) %>%
    select(
      customer_id, cohort_qtr, cohort_ym, sim_data
      ) %>%
    unnest(sim_data)

  return(customer_transactions_tbl)
}





generate_customer_cohort_data <- function(input_data_tbl, first_date, last_date) {

}




generate_customer_transactions <- function(lifetime, tnx_rate, mx_nu, mx_g,
                                           first_date, final_date = final_date) {

  obs_weeks <- difftime(final_date, first_date, units = "weeks") %>% as.numeric()

  tnx_window <- min(obs_weeks, lifetime)

  sim_count <- round(10 * (tnx_window * tnx_rate), 0) %>% max(10)

  event_times <- rexp(sim_count, rate = tnx_rate)

  use_event_weeks <- cumsum(event_times)
  use_event_weeks <- use_event_weeks[use_event_weeks < tnx_window]

  event_dates <- first_date + (use_event_weeks * 7)

  tnx_amounts <- rgamma(1 + length(event_dates), shape = mx_p, rate = mx_nu)

  sim_tnx_tbl <- tibble(
    tnx_date   = c(first_date, event_dates),
    tnx_amount = tnx_amounts %>% round(2)
  )

  return(sim_tnx_tbl)
}


generate_pnbd_customer_simulation_params <- function(customer_cohort_data_tbl) {
  customer_basic_parameters_tbl <- customer_cohort_data_tbl %>%
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date) %>%
    arrange(first_tnx_date, customer_id) %>%
    mutate(
      customer_mu     = rgamma(n(), shape = mu_shape,     rate = mu_rate),
      customer_tau    = rexp(n(), rate = customer_mu),
      customer_lambda = rgamma(n(), shape = lambda_shape, rate = lambda_rate),
      customer_nu     = rgamma(n(), shape = mx_q,         rate = mx_g)
    )

  return(customer_basic_parameters_tbl)
}


generate_customer_transaction_data <- function(sim_params_tbl) {
  customer_transaction_tbl <- sim_params_tbl %>%
    mutate(
      sim_data = pmap(
        list(
          lifetime   = customer_tau,
          tnx_rate   = customer_lambda,
          mx_nu      = customer_nu,
          mx_g       = mx_g,
          first_date = first_tnx_date
        ),
        generate_customer_transactions,
        final_date = final_date_observed
      )
    ) %>%
    select(
      customer_id, cohort_qtr, cohort_ym, sim_data
    ) %>%
    unnest(sim_data) %>%
    arrange(customer_id, tnx_date)

  return(customer_transactions_tbl)
}





generate_customer_cohort_data <- function(n_customers, first_date, last_date) {
  tnx_dates <- seq(first_date, last_date, by = "day")

  customer_cohort_tbl <- tnx_dates %>%
    enframe(name = NULL, value = "first_tnx_date") %>%
    slice_sample(n = n_customers, replace = TRUE) %>%
    arrange(first_tnx_date) %>%
    group_by(format(first_tnx_date, "%Y%m")) %>%
    mutate(
      customer_id = sprintf("C%s_%04d", format(first_tnx_date, "%Y%m"), 1:n()),
      cohort_qtr  = first_tnx_date %>% as.yearqtr() %>% as.character(),
      cohort_ym   = first_tnx_date %>% format("%Y %m")
      ) %>%
    ungroup() %>%
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date)

  return(customer_cohort_tbl)
}


generate_pnbd_customer_simulation_params <- function(customer_cohort_data_tbl,
                                                     params_lst) {
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


generate_pnbd_customer_transaction_data <- function(sim_params_tbl, final_tnx_date) {
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
        final_date = final_tnx_date
        )
      ) %>%
    select(
      customer_id, cohort_qtr, cohort_ym, sim_data
      ) %>%
    unnest(sim_data)

  return(customer_transactions_tbl)
}


generate_pnbd_individual_transactions <- function(lifetime, tnx_rate, mx_nu, mx_p,
                                                  first_date, final_date = final_date) {

  obs_weeks <- difftime(final_date, first_date, units = "weeks") %>% as.numeric()

  tnx_window <- min(obs_weeks, lifetime)

  if(tnx_window < 0) warning("Invalid tnx_window value: should not be negative")

  first_tnx_dttm <- as.POSIXct(first_date) + runif(1, min = 0, max = 24 * 60 * 60 - 1)

  tnx_intervals <- calculate_event_times(
    rate       = tnx_rate,
    total_time = tnx_window,
    block_size = 100
  )

  event_dates <- first_tnx_dttm + (tnx_intervals * (7 * 24 * 60 * 60))

  tnx_amounts <- rgamma(1 + length(event_dates), shape = mx_p, rate = mx_nu)

  sim_tnx_tbl <- tibble(
    tnx_timestamp = c(first_tnx_dttm, event_dates),
    tnx_amount    = tnx_amounts %>% round(2)
    )

  return(sim_tnx_tbl)
}


generate_transaction_metadata <- function(data_tbl) {
  transactions_tbl <- data_tbl %>%
    arrange(tnx_timestamp) %>%
    group_by(tnx_date = as.Date(tnx_timestamp)) %>%
    mutate(
      invoice_id = sprintf("T%s-%04d", format(tnx_date, "%Y%m%d"), 1:n())
    ) %>%
    ungroup() %>%
    select(customer_id, tnx_timestamp, invoice_id, tnx_amount)

  return(transactions_tbl)
}



generate_medical_test_data <- function(n_patient, prevalence = 0.01,
                                       falsepos = 0.05, falseneg = 0.05) {

  test_prop_tbl <- tribble(
    ~infected,           ~prop_pos,
         TRUE,      (1 - falseneg),
        FALSE,            falsepos
    )

  calculate_test_result <- function(data_tbl, test_prop) {
    test_tbl <- data_tbl %>%
      mutate(
        test_result = rbernoulli(n = n(), p = test_prop)
      )

    return(test_tbl)
  }

  population_tbl <- tibble(patient_id = 1:n_sim) %>%
    mutate(
      infected = rbernoulli(n(), p = prevalence)
      ) %>%
    group_nest(infected, .key = "patient_data") %>%
    inner_join(test_prop_tbl, by = "infected") %>%
    mutate(
      test_data = map2(patient_data, prop_pos, calculate_test_result)
      ) %>%
    unnest(test_data) %>%
    select(patient_id, infected, test_result)

  return(population_tbl)
}


calculate_medical_test_probability <- function(prevalence = 0.01, false_pos = 0.05, false_neg = 0.05) {
  t1 <- (1 - false_pos) * prevalence
  t2 <- false_neg * (1 - prevalence)

  cond_prob <- t1 / (t1 + t2)

  return(cond_prob)
}


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

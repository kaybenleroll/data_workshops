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

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

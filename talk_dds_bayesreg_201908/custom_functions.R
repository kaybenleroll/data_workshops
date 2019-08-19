construct_loglik_function <- function(data_tbl) {
  carat_vals <- data_tbl %>% pull(carat)
  price_vals <- data_tbl %>% pull(price)

  param_loglik_func <- function(intercept, carat_param, fit_sd, show_prob = 0.01) {
    if(runif(1) < show_prob) {
      message(paste0("Int:", intercept, " Carat:", carat_param))
    }

    mean_vals <- intercept + carat_param * carat_vals

    data_loglik <- map2_dbl(price_vals, mean_vals
                           ,~dnorm(.x, mean = .y, sd = fit_sd, log = TRUE))

    return(sum(data_loglik))
  }

  return(param_loglik_func)
}

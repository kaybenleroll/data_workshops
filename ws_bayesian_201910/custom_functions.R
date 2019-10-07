create_medtest_data <- function(n_sim     = 1e6,
                                base_rate = 0.001,
                                true_rate = 0.99,
                                fa_rate   = 0.05) {

  data_tbl <- tibble(id = 1:n_sim) %>%
    mutate(sick_person = sample(c(TRUE, FALSE),
                                n_sim,
                                prob = c(base_rate, 1 - base_rate),
                                replace = TRUE))

  sick_tbl    <- data_tbl %>% filter(sick_person == TRUE)
  notsick_tbl <- data_tbl %>% filter(sick_person == FALSE)

  sick_tbl <- sick_tbl %>%
    mutate(test_result = sample(c(TRUE, FALSE),
                                n(),
                                prob = c(true_rate, 1 - true_rate),
                                replace = TRUE))

  notsick_tbl <- notsick_tbl %>%
    mutate(test_result = sample(c(TRUE, FALSE),
                                n(),
                                prob = c(fa_rate, 1 - fa_rate),
                                replace = TRUE))


  data_tbl <- list(sick_tbl, notsick_tbl) %>%
    bind_rows() %>%
    arrange(id)

  return(data_tbl)
}


create_two_medtest_data <- function(n_sim = 1e6, base_rate = 0.001,
                                    true_rate = 0.99, fa_rate = 0.05) {
  data_tbl <- tibble(id = 1:n_sim) %>%
    mutate(sick_person = sample(c(TRUE, FALSE)
                                ,n_sim
                                ,prob = c(base_rate, 1 - base_rate)
                                ,replace = TRUE))

  sick_tbl    <- data_tbl %>% filter(sick_person == TRUE)
  notsick_tbl <- data_tbl %>% filter(sick_person == FALSE)

  sick_tbl <- sick_tbl %>%
    mutate(test_result_1 = sample(c(TRUE, FALSE)
                                  ,n()
                                  ,prob = c(true_rate, 1 - true_rate)
                                  ,replace = TRUE),
           test_result_2 = sample(c(TRUE, FALSE)
                                  ,n()
                                  ,prob = c(true_rate, 1 - true_rate)
                                  ,replace = TRUE)
    )

  notsick_tbl <- notsick_tbl %>%
    mutate(test_result_1 = sample(c(TRUE, FALSE),
                                  n(),
                                  prob = c(fa_rate, 1 - fa_rate),
                                  replace = TRUE),
           test_result_2 = sample(c(TRUE, FALSE),
                                  n(),
                                  prob = c(fa_rate, 1 - fa_rate),
                                  replace = TRUE)
    )

  data_tbl <- list(sick_tbl, notsick_tbl) %>%
    bind_rows() %>%
    arrange(id)

  return(data_tbl)
}


calc_bern_likelihood <- function(y, theta, log = TRUE) {

  if(log) {
    lik <- y * log(theta) + (1-y) * log(1 - theta)
  } else {
    lik <- theta^y * (1 - theta)^(1 - y)
  }

  return(lik)
}


calc_data_loglik <- function(data, theta) {
  data_loglik_tbl <- tibble(theta = theta) %>%
    mutate(loglik = map_dbl(theta, ~ calc_bern_likelihood(data, .x) %>% sum()))

  return(data_loglik_tbl)
}


calc_posterior <- function(prior_tbl, data_loglik_tbl) {
  bayes_tbl <- prior_tbl %>%
    inner_join(data_loglik_tbl, by = 'theta') %>%
    mutate(unnorm_loglik = dens_log + loglik,
           width         = c(0, diff(theta)),
           tmp           = (exp(unnorm_loglik) * width) %>% sum(na.rm = TRUE) %>% log(),
           post_loglik   = unnorm_loglik - tmp,
           post_lik      = exp(post_loglik)
    )

  return(bayes_tbl)
}

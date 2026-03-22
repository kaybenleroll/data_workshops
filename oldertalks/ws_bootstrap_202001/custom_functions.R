construct_bootstrap_size_plots <- function(x, func) {
  btstrp_tbl <- tibble(x = x) %>%
    bootstraps(times = 10000) %>%
    mutate(btstrp_val = map_dbl(splits, ~ .x %>% analysis() %>% pull(x) %>% func())) %>%
    select(id, btstrp_val)

  btstrp_size_data_tbl <- c(100, 500, 1000, 10000) %>%
    enframe(name = NULL, value = 'btstrp_size') %>%
    mutate(boot_data = map(btstrp_size, ~ btstrp_tbl %>% head(.x))) %>%
    unnest(boot_data)

  return(btstrp_size_data_tbl)
}

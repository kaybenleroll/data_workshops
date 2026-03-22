library(conflicted)
library(tidyverse)
library(tidyquant)
library(scales)
library(cowplot)

source("custom_functions.R")

resolve_conflicts(c("magrittr", "rlang", "dplyr", "readr", "purrr", "ggplot2"))

spy_pricedata_tbl <- tq_get("SPY", from = as.Date("1990-01-01"))

spy_returns_tbl <- spy_pricedata_tbl %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = 'monthly',
    type       = 'log',
    col_rename = 'monthly_return'
    ) %>%
  arrange(symbol, date) %>%
  filter(date >= as.Date('2000-01-01'),
         date <= as.Date('2020-09-30')
         ) %>%
  ungroup()

spy_pricedata_tbl %>% write_rds("data/spy_pricedata_tbl.rds")
spy_returns_tbl   %>% write_rds("data/spy_returns_tbl.rds")

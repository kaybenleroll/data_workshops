library(conflicted)
library(tidyverse)
library(scales)
library(magrittr)
library(rlang)
library(fs)
library(purrr)
library(glue)
library(tidyquant)
library(snakecase)


source("lib_utils.R")


conflict_lst <- resolve_conflicts(
  c("magrittr", "rlang", "dplyr", "readr", "purrr", "ggplot2", "MASS",
    "fitdistrplus")
  )


options(
  width = 80L,
  warn  = 1,
  mc.cores = parallelly::availableCores()
  )


set.seed(42)
stanfit_seed <- 4000

theme_set(theme_cowplot())

plan(multisession)



### Retrieve Wikipedia Page

message(glue("Retrieving the S&P 500 Component ticker symbols as well as some other information"))

wikipedia_url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

sp500_wikipedia_tbl <- wikipedia_url |>
  read_html() |>
  html_nodes(xpath = '//*[@id="constituents"]') |>
  extract2(1) |>
  html_table() %>%
  set_colnames(names(.) |> to_snake_case()) |>
  mutate(
    symbol = symbol |> str_replace("\\.", "\\-")
    ) |>
  arrange(symbol)

sp500_wikipedia_tbl |> glimpse()

sp500_wikipedia_tbl |>
  write_rds("retrieved_data/sp500_wikipedia_tbl.rds")



### Retrieve tidyquant index data

message(glue("Retrieving tidyquant data on the S&P 500"))

sp500_tidyquant_tbl <- tq_index("SP500")

sp500_tidyquant_tbl |>
  write_rds("retrieved_data/sp500_tidyquant_tbl.rds")



### Retrieve S&P500 index data

message(glue("Retrieving price data on the S&P 500 from Jan 1 2021 to 31 May 2024"))

sp500_pricedata_tbl <- sp500_wikipedia_tbl |>
  mutate(
    price_data = map(
      symbol,
      \(x) tq_get(x, from = "2021-01-01", to = "2024-05-31"),

      .progress = "get_prices"
      )
    )

sp500_pricedata_tbl |>
  write_rds("retrieved_data/sp500_pricedata_tbl.rds")


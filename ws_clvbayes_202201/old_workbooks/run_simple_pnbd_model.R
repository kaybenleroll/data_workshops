library(conflicted)
library(tidyverse)
library(scales)
library(cowplot)
library(directlabels)
library(magrittr)
library(rlang)
library(fs)
library(purrr)
library(furrr)
library(glue)
library(CLVTools)
library(rsample)
library(MASS)
library(fitdistrplus)
library(rstan)
library(brms)
library(posterior)
library(bayesplot)
library(tidybayes)


source("lib_utils.R")

resolve_conflicts(
  c("magrittr", "rlang", "dplyr", "readr", "purrr", "ggplot2", "MASS",
    "fitdistrplus")
)

options(
  width = 80L,
  warn  = 1,
  mc.cores = parallel::detectCores()
)

theme_set(theme_cowplot())

set.seed(42)

plan(multisession)

training_data_date <- as.Date("2011-03-31")

customer_firstdate_tbl   <- read_rds("data/customer_firstdate_tbl.rds")
daily_spend_customer_tbl <- read_rds("data/daily_spend_customer_tbl.rds")

customer_clvdata_tbl <- daily_spend_customer_tbl %>%
  left_join(customer_firstdate_tbl, by = "customer_id") %>%
  filter(cohort_qtr == "2009 Q4") %>%
  clvdata(
    date.format = "%Y-%m-%d",
    time.unit = "weeks",
    estimation.split = training_data_date,
    name.id          = "customer_id",
    name.date        = "invoice_date",
    name.price       = "total_spend"
  )

customer_pnbd_cbs_tbl <- customer_clvdata_tbl %>%
  (CLVTools:::pnbd_cbs)()


pnbd_data_lst <- list(
  N         = customer_pnbd_cbs_tbl %>% nrow(),
  n         = customer_pnbd_cbs_tbl %>% nrow(),

  T         = customer_pnbd_cbs_tbl %>% pull(T.cal),
  T         = customer_pnbd_cbs_tbl %>% pull(T.cal),

  recency   = customer_pnbd_cbs_tbl %>% pull(t.x),
  t         = customer_pnbd_cbs_tbl %>% pull(t.x),

  frequency = customer_pnbd_cbs_tbl %>% pull(x),
  k         = customer_pnbd_cbs_tbl %>% pull(x)
)

stan_data_lst <- compose_data(
  pnbd_data_lst,

  lambda_rate = 10,
  etau_rate   = 1
)



pnbd_priors_stanmodel <- stan_model(
  "stan_files/clv_pnbd_priormean.stan",
  model_name = "pnbd_priors_model",
  verbose    = TRUE
)


pnbd_priors_stanfit <- sampling(
  pnbd_priors_stanmodel,
  data   = stan_data_lst,
  chains =    4,
  iter   = 1000,
  seed   =   42
)

pnbd_priors_stanfit %>% check_hmc_diagnostics()


pnbd_priors_postparams_tbl <- pnbd_priors_stanfit %>%
  tidy_draws() %>%
  pivot_longer(
    !c(.chain, .iteration, .draw),
    names_to  = "parameter",
    values_to = "value"
  )

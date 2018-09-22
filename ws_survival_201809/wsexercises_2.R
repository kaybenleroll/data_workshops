library(tidyverse)
library(scales)
library(cowplot)
library(survival)
library(survminer)
library(muhaz)
library(broom)

source('data_setup.R')


# Exercise 1.1.1.1


### Question 1
tc_km <- survfit(Surv(accountdur, churned) ~ 1, data = telco_churn_tbl)

ggsurvplot(tc_km)

ggsurvplot(tc_km, censor = FALSE)




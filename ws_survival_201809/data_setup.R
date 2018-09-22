### Data Setup Script
library(tidyverse)
library(asaur)

# Re-using some code from Sec 1 of the workshop to avoid repeating myself
# in later worksheets.


# Setup the Telco churn data
telco_churn_tbl <- read_csv('data/telcochurn.csv'
                           ,col_types = cols()
                            )

# Setup the pharmacoSmoking data
pharmaco_smoker_tbl <- asaur::pharmacoSmoking %>% as_data_frame()

# Setup the prostate survival data
prostate_surv_tbl <- asaur::prostateSurvival %>% as_data_frame()


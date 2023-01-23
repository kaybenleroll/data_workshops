library(tidyverse)
library(tidytext)
library(textdata)


sentiments_afinn_tbl    <- get_sentiments("afinn")
sentiments_afinn_tbl    |> write_rds("input_data/sentiments_afinn_tbl.rds")

sentiments_bing_tbl     <- get_sentiments("bing")
sentiments_bing_tbl     |> write_rds("input_data/sentiments_bing_tbl.rds")

sentiments_loughran_tbl <- get_sentiments("loughran")
sentiments_loughran_tbl |> write_rds("input_data/sentiments_loughran_tbl.rds")

sentiments_nrc_tbl      <- get_sentiments("nrc")
sentiments_nrc_tbl      |> write_rds("input_data/sentiments_nrc_tbl.rds")


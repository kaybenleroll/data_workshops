library(tidyverse)
library(jsonlite)

parser_lst <- read_json("input_data/script_parser_data.json")

sorted_lst <- parser_lst[sort(names(parser_lst))]

write_json(
  sorted_lst,
  path       = "input_data/script_parser_data_SORTED.json",
  pretty     = TRUE,
  auto_unbox = TRUE
  )



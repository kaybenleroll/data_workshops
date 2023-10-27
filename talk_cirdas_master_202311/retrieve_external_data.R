library(conflicted)
library(tidyverse)
library(rlang)
library(cowplot)
library(fs)
library(curl)


source("lib_utils.R")

conflict_lst <- resolve_conflicts(
  c("xml2", "magrittr", "rlang", "dplyr", "readr", "purrr", "ggplot2")
)


options(
  width = 80L,
  warn  = 1,
  mc.cores = parallelly::availableCores()
)

set.seed(42)


geospatial_data_file <- "geospatial_data/FRA_adm_shp.zip"

if(!file_exists(geospatial_data_file)) {
  curl_download(
    "https://biogeo.ucdavis.edu/data/gadm2.8/shp/FRA_adm_shp.zip",
    geospatial_data_file
    )
}

unzip(geospatial_data_file, exdir = "geospatial_data")

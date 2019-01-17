retrieve_cran_download_data <- function(cran_pkgs, start_date, end_date) {

    cran_data_tbl <- cran_downloads(cran_pkgs
                                   ,from = start_date
                                   ,to   = end_date) %>%
        as_tibble()


    cran_all_tbl  <- cran_downloads(NULL
                                   ,from = start_date
                                   ,to   = end_date) %>%
        as_tibble() %>%
        mutate(package = 'TOTAL')


    cran_data_tbl <- bind_rows(cran_data_tbl, cran_all_tbl)


    return(cran_data_tbl)
}

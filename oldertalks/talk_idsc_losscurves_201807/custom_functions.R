get_character_index <- function(x) match(x, sort(unique(x)))

read_claim_datafile <- function(path, col_type = cols(), progress = FALSE) {
    lob <- path %>% basename
    lob <- gsub("_pos\\.csv", "", lob)

    data_tbl <- read_csv(path, col_type = col_type, progress = progress)

    col_names <- data_tbl %>% names %>% tolower
    col_names <- gsub("_..?", "", col_names)

    names(data_tbl) <- col_names

    data_tbl <- data_tbl %>% mutate(lob = lob)

    return(data_tbl)
}


create_curve_likelihood_function <- function(curve_tbl) {
    calculate_curve_data <- function(lambda, ulr, sd = 0.03) {
        data_tbl <- curve_tbl %>%
            mutate(predicted = ulr * (1 - exp(-lambda * dev_lag))
                  ,loglik    = map2_dbl(loss_ratio, predicted, dnorm, sd = sd, log = TRUE)
            )

        loglik <- data_tbl$loglik %>% sum

        return(list(curve_tbl = data_tbl %>% dplyr::select(dev_lag, loss_ratio, predicted, loglik)
                   ,loglik    = loglik))
    }

    return(calculate_curve_data)
}


create_dev_factor_diagnostics <- function(curve_tbl, l_seq, u_seq
                                         ,plot_title = 'Grid Approximation of Curve Fit') {

    max_development <- curve_tbl %>% pull(dev_lag) %>% max

    curve_logl <- create_curve_likelihood_function(curve_tbl)

    curve_mle <- optim(c(0.5, 0.5)
                      ,function(x) curve_logl(x[1],x[2])$loglik
                      ,control = list(fnscale = -1)
    )


    curve_grid_tbl <- crossing(lambda = l_seq, ulr = u_seq) %>%
        mutate(max_dev  = max_development
              ,output   = map2(lambda, ulr, curve_logl)
              ,loglik   = map_dbl(output, 'loglik')
              ,imp_prob = exp(loglik)
              ,prob     = imp_prob / sum(imp_prob)
        )

    curve_gridapprox_plot <- ggplot(curve_grid_tbl) +
        geom_tile   (aes(x = lambda, y = ulr, fill = prob)) +
#        geom_contour(aes(x = lambda, y = ulr, z = prob), binwidth = 1, colour = 'black', bins = 10) +
        geom_point  (aes(x = curve_mle$par[1], y = curve_mle$par[2]), colour = 'black', size = 0.2) +
        scale_fill_gradient(low = 'blue', high = 'red') +
        xlab(expression(lambda)) +
        ylab("ULR") +
        ggtitle(plot_title
               ,subtitle = paste0("Data Count: ", max_development, " Years")
               ) +
        theme(legend.position = 'none')



    diagnostic_lst <- list(
        logl            = curve_logl
       ,mle             = curve_mle
       ,grid_tbl        = curve_grid_tbl
       ,gridapprox_plot = curve_gridapprox_plot
    )

    return(diagnostic_lst)
}


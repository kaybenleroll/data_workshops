
clean_names <- function(colnames) {
    colnames <- gsub(" ",        "_", colnames)
    colnames <- gsub("/",        "_", colnames)
    colnames <- gsub("\\.",      "_", colnames)
    colnames <- gsub("\\-",      "_", colnames)
    colnames <- gsub("'",         "", colnames)
    colnames <- gsub("`",         "", colnames)
    colnames <- gsub("\\(",       "", colnames)
    colnames <- gsub("\\)",       "", colnames)
    colnames <- gsub("\\?",       "", colnames)
    colnames <- gsub("\\%",       "", colnames)
    colnames <- gsub("\u2019",    "", colnames)
    colnames <- gsub("\u20AC", "EUR", colnames)

    colnames <- colnames %>%
        gsub('(.)([A-Z][a-z]+)',  '\\1_\\2', .) %>%
        gsub('([a-z0-9])([A-Z])', '\\1_\\2', .) %>%
        tolower

    colnames <- gsub("__+", "_", colnames)

    return(colnames)
}


fill_na_values <- function(x) ifelse(is.na(x), 0, x)


powerlaw_claimsize_count <- function(claimsize, claimdata_tbl) {
    claimdata_tbl %>%
        filter(claim_amount >= 10^claimsize) %>%
        nrow
}


create_crossval_assessment <- function(train, valid) {
    valid_tbl <- valid %>% as_data_frame

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_nb)) %>%
        pull(total_claims)

    model_glm <- glm(claim_nb ~ gas + cat_driver_age
                     ,offset = log(exposure)
                     ,data   = train %>% as_data_frame
                     ,family = poisson
    )

    claim_rates <- predict(model_glm
                           ,newdata = valid %>% as_data_frame
                           ,type = 'response')

    predicted_claim_count <- rpois(1000, claim_rates %>% sum)

    cuml_prob <- ecdf(predicted_claim_count)(observed_valid_count)

    assess_lst <- list(
        observed_count  = observed_valid_count
        ,predicted_count = predicted_claim_count
        ,cuml_prob       = cuml_prob
    )

    return(assess_lst)
}


create_claimrate_assessment <- function(train, valid) {
    model_glm <- glm(claim_count ~ gas + cat_driver_age + car_age + density +
                         cat_driver_age:gas
                     ,offset = log(exposure)
                     ,data   = train %>% as_data_frame
                     ,family = poisson
    )

    valid_tbl <- valid %>% as_data_frame

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_count)) %>%
        pull(total_claims)

    claim_rates <- predict(model_glm
                           ,newdata = valid_tbl
                           ,type = 'response')

    predicted_claim_count <- rpois(1000, claim_rates %>% sum)

    cuml_prob <- ecdf(predicted_claim_count)(observed_valid_count)

    assess_lst <- list(
        observed_count  = observed_valid_count
        ,predicted_count = predicted_claim_count
        ,cuml_prob       = cuml_prob
    )

    return(assess_lst)
}


create_pricing_function <- function(claimrate_model_glm
                                    ,claimsize_model_glm
                                    ,largeloss_charge
                                    ,quote_ratio) {

    price_function <- function(policy_tbl) {
        claim_rates <- predict(claimrate_model_glm
                               ,newdata = policy_tbl
                               ,type = 'response')

        claim_sizes <- predict(claimsize_model_glm
                               ,newdata = policy_tbl
                               ,type = 'response')

        expect_price <- claim_rates * claim_sizes
        risk_premium <- expect_price + largeloss_charge

        price_tbl <- data_frame(
            expect_price     = expect_price
            ,largeloss_charge = largeloss_charge
            ,risk_premium     = risk_premium
            ,quote_price      = risk_premium * (1 + quote_ratio)
        )

        return(price_tbl)
    }

    return(price_function)
}


###
### Quick note on what this function does:
###
### For computational efficiency, I calculate the total number of claims
### required and then use a combination of cumulative sums and indexes to
### properly allocate out the claim amounts across the simulations.
###
calculate_claim_sizes <- function(claim_list, shape, rate) {
    claim_idx <- claim_list %>% cumsum

    total_claim_count <- claim_list %>% sum
    claim_amounts     <- rgamma(total_claim_count, shape = shape, rate = rate)

    claim_amount_cumsum <- c(0, claim_amounts %>% cumsum)
    claim_cumsum        <- claim_amount_cumsum[claim_idx + 1]

    claim_sizes <- c(0, claim_cumsum) %>% diff

    return(claim_sizes)
}


create_claim_simulator <- function(claimfreq_glm, claimsev_glm, n_sim = 1000) {

    generate_claim_simulations <- function(data_tbl, variable_claim_size = TRUE) {
        simulation_tbl <- data_tbl %>%
            mutate(claim_rate    = predict(claimfreq_glm, newdata = data_tbl, type = 'response')
                  ,claim_size_mu = predict(claimsev_glm,  newdata = data_tbl, type = 'response')
                  ,claim_counts  = map(claim_rate, function(l) rpois(n_sim, l))
                    )

        if(variable_claim_size) {
            simulation_tbl <- simulation_tbl %>%
                mutate(claim_size_shape = MASS::gamma.shape(claimsev_glm)$alpha
                      ,claim_size_rate  = claim_size_shape / claim_size_mu
                      ,claim_costs      = map(claim_counts
                                             ,calculate_claim_sizes
                                             ,shape = claim_size_shape
                                             ,rate  = claim_size_rate)
                )
        } else {
            simulation_tbl <- simulation_tbl %>%
                mutate(claim_costs = map2(claim_counts, claim_size_mu, `*`))
        }
    }

    return(generate_claim_simulations)
}




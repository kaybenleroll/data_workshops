
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


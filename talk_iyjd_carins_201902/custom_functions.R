
powerlaw_claimsize_count <- function(claimsize, claimdata_tbl) {
    claimdata_tbl %>%
        filter(claim_amount >= 10^claimsize) %>%
        nrow()
}


create_crossval_assessment <- function(train, valid, n_sim = 1000) {
    valid_tbl <- valid %>% as_tibble()

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_nb)) %>%
        pull(total_claims)

    model_glm <- glm(claim_nb ~ gas + cat_driver_age
                    ,offset = log(exposure)
                    ,data   = train %>% as_tibble()
                    ,family = poisson
    )

    claim_rates <- predict(model_glm
                          ,newdata = valid %>% as_tibble()
                          ,type = 'response')

    predicted_claim_count <- rpois(n_sim, claim_rates %>% sum())

    cuml_prob <- ecdf(predicted_claim_count)(observed_valid_count)

    assess_lst <- list(
        observed_count  = observed_valid_count
       ,predicted_count = predicted_claim_count
       ,cuml_prob       = cuml_prob
    )

    return(assess_lst)
}


create_claimrate_assessment <- function(train, valid, n_sim = 1000) {
    model_glm <- glm(claim_count ~ gas + cat_driver_age + car_age + density +
                                   cat_driver_age:gas
                    ,offset = log(exposure)
                    ,data   = train %>% as_tibble()
                    ,family = poisson
    )

    valid_tbl <- valid %>% as_tibble

    observed_valid_count <- valid_tbl %>%
        summarise(total_claims = sum(claim_count)) %>%
        pull(total_claims)

    claim_rates <- predict(model_glm
                          ,newdata = valid_tbl
                          ,type = 'response')

    predicted_claim_count <- rpois(n_sim, claim_rates %>% sum())

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
        policy_id <- policy_tbl$policy_id

        claim_rates <- predict(claimrate_model_glm
                              ,newdata = policy_tbl
                              ,type = 'response')

        claim_sizes <- predict(claimsize_model_glm
                              ,newdata = policy_tbl
                              ,type = 'response')

        expect_price <- claim_rates * claim_sizes
        risk_premium <- expect_price + largeloss_charge

        price_tbl <- tibble(
            policy_id        = policy_id
           ,expect_price     = expect_price
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


calculate_largeloss_claims <- function(claim_list, scaling) {
    claim_idx <- claim_list %>% cumsum

    total_claim_count <- claim_list %>% sum
    claim_amounts     <- rplcon(total_claim_count, xmin = 25000, alpha = scaling)

    claim_amount_cumsum <- c(0, claim_amounts %>% cumsum)
    claim_cumsum        <- claim_amount_cumsum[claim_idx + 1]

    claim_sizes <- c(0, claim_cumsum) %>% diff

    return(claim_sizes)
}





###
### This function creates a claim simulation function based on the various
### models provided to it (assuming a glm)
###
create_claim_simulator <- function(claimfreq_glm
                                  ,claimsev_glm
                                  ,largeloss_freq    = NULL
                                  ,largeloss_scaling = NULL) {

    generate_claim_simulations <- function(data_tbl
                                          ,n_sim = 1000
                                          ,variable_claim_size = TRUE
                                          ,model_large_losses  = TRUE) {

        ### We first predict the mean for the claim rate and claim size
        ### Then we simulate claim counts for each policy
        simulation_tbl <- data_tbl %>%
            mutate(claim_rate    = predict(claimfreq_glm, newdata = data_tbl, type = 'response')
                  ,claim_size_mu = predict(claimsev_glm,  newdata = data_tbl, type = 'response')
                  ,claim_counts  = map(claim_rate, function(l) rpois(n_sim, l))
                    )

        ### If we want variable claim size, we simulate from a gamma distribution
        ### using the parameters as given from the GLM
        if(variable_claim_size) {
            simulation_tbl <- simulation_tbl %>%
                mutate(claim_size_shape = gamma.shape(claimsev_glm)$alpha
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


        ### If we include large losses, we proceed in a similar way for the
        ### the attritional piece - simplified by the fact that all policies
        ### are treated the same.
        if(model_large_losses &
           !is.null(largeloss_freq) &
           !is.null(largeloss_scaling)) {
            simulation_tbl <- simulation_tbl %>%
                mutate(largeloss_freq       = largeloss_freq
                      ,largeloss_claimcount = map(largeloss_freq, function(l) rpois(n_sim, l))
                      ,largeloss_claimsize  = map(largeloss_claimcount
                                                 ,calculate_largeloss_claims
                                                 ,largeloss_scaling
                                                  )
                )
        }

        return(simulation_tbl)
    }

    return(generate_claim_simulations)
}


construct_assessment_plot <- function(sim_vals, obs_val, title, subtitle) {
    assessment_plot <- ggplot() +
        geom_histogram(aes(x = sim_vals), bins = 50) +
        geom_vline(aes(xintercept = obs_val), colour = 'red') +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        xlab("Simulation Value") +
        ylab("Count") +
        ggtitle(title, subtitle = subtitle)

    return(assessment_plot)
}


construct_model_assessment <- function(data_tbl, title_prefix) {
    sim_claim_count <- data_tbl$claim_counts %>% reduce(`+`)
    obs_claim_count <- data_tbl$claim_count %>% sum()

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Claim Counts vs Observed")
    plot_subtitle <- paste0((data_tbl %>% nrow %>% comma), " Policies Used")

    claimcount_plot <- construct_assessment_plot(sim_claim_count
                                                ,obs_claim_count
                                                ,plot_title
                                                ,plot_subtitle)



    sim_loss_amount <- data_tbl$claim_costs %>% reduce(`+`)
    obs_loss_amount <- data_tbl$claim_total %>% reduce(`+`)

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Loss Cost vs Observed")
    plot_subtitle <- paste0((data_tbl %>% nrow %>% comma), " Policies Used")

    losscost_plot <- construct_assessment_plot(sim_loss_amount
                                              ,obs_loss_amount
                                              ,plot_title
                                              ,plot_subtitle)

    losscost_plot <- losscost_plot +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


    assess_lst <- list(
        data_tbl        = data_tbl
       ,sim_claim_count = sim_claim_count
       ,obs_claim_count = obs_claim_count
       ,claimcount_plot = claimcount_plot
       ,sim_loss_amount = sim_loss_amount
       ,obs_loss_amount = obs_loss_amount
       ,losscost_plot   = losscost_plot
    )

    return(assess_lst)
}


construct_pricing_assessment <- function(data_tbl, title_prefix) {
    sim_loss_amount <- data_tbl$claim_costs  %>% reduce(`+`)
    premium_charged <- data_tbl$expect_price %>% sum()

    plot_title    <- paste0(title_prefix, " Comparison Plot for Simulated Loss Cost vs Premium Charged")
    plot_subtitle <- paste0((data_tbl %>% nrow() %>% comma()), " Policies Used")

    assess_plot <- construct_assessment_plot(sim_loss_amount
                                            ,premium_charged
                                            ,plot_title
                                            ,plot_subtitle
                                             )


    assess_lst <- list(
        data_tbl        = data_tbl
       ,sim_loss_amount = sim_loss_amount
       ,premium_charged = premium_charged
       ,assess_plot     = assess_plot
    )

    return(assess_lst)
}



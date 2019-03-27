calc_call_price <- function(S, T) {
    calc <- AmericanOption(type          = 'call'
                           ,underlying    = S
                           ,strike        = 100
                           ,dividendYield = 0
                           ,riskFreeRate  = r
                           ,maturity      = T / 252
                           ,volatility    = 0.2)

    return(calc$value)
}


calc_option_price_greeks <- function(...) {
    option_price <- EuropeanOption(...)

    option_price$delta <- option_price$delta * 100
    option_price$gamma <- option_price$gamma * 100
    option_price$vega  <- option_price$vega  * 0.01
    option_price$theta <- option_price$theta/252

    return(unlist(option_price[c('value','delta','gamma','vega','theta')]))
}


leverage_pricer <- function(S, t) {
    option_price <- AmericanOption(
        type = 'call'
        ,underlying    = S
        ,strike        = 100
        ,dividendYield = 0
        ,riskFreeRate  = 0.01
        ,maturity      = t
        ,volatility    = 0.2
    )

    return(option_price$value)
}


construct_margin_calc_details <- function(x) {
    comp_id = 1:length(x)

    spread <- map_chr(x, 'spread')
    margin <- map_dbl(x, 'margin')

    asset_dt  <- map(x, 'asset.dt')

    calc_tbl <- tibble(
        comp_id  = comp_id
       ,spread   = spread
       ,margin   = margin
       ,asset_dt = asset_dt
    )

    return(calc_tbl)
}


construct_margin_data <- function(x) {
    N <- length(x)

    margin_tbl <- tibble(sim_id = 1:N) %>%
        mutate(comp_count = map_int(x, length)
              ,data       = map(x, construct_margin_calc_details)
               ) %>%
        unnest()

    return(margin_tbl)
}



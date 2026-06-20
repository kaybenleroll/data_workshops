source("custom_functions.R")


shinyServer(function(input, output) {

    output$display_portfolio_dt <- DT::renderDataTable({
        DT::datatable(bondportfolio_dt
                     ,rownames = FALSE
                     ,list(pageLength = 50))
    })

    output$display_totalcashflow_dt <- DT::renderDataTable({
        DT::datatable(cashflow_dt[, lapply(.SD, sum), by = 'currency', .SDcols = ym_values]
                     ,rownames = FALSE
                     ,list(pageLength = 50))
    })

    output$display_bondcashflow_dt <- DT::renderDataTable({
        DT::datatable(cashflow_dt
                     ,rownames = FALSE
                     ,list(pageLength = 50))
    })

    output$display_hedges_dt <- DT::renderDataTable({
        DT::datatable(bondportfolio_hedge_dt
                     ,rownames = FALSE
                     ,list(pageLength = 50))
    })



})

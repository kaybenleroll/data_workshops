
sb_panel <- sidebarPanel(helpText("Portfolio files here"), width = 2)


shinyUI(navbarPage(
    "Bond Cashflow Application"
   ,tabPanel("Bond Portfolio"
            ,sidebarLayout(
                 sb_panel
                ,mainPanel(
                     div(DT::dataTableOutput('display_portfolio_dt')
                        ,style = 'font-size: 80%')
                 )))

   ,tabPanel("Bond Cashflows"
            ,sidebarLayout(
                 sb_panel
                ,mainPanel(
                     div(DT::dataTableOutput('display_bondcashflow_dt')
                        ,style = 'font-size: 80%')
                 )))

   ,tabPanel("Total Cashflows"
            ,sidebarLayout(
                 sb_panel
                ,mainPanel(
                     div(DT::dataTableOutput('display_totalcashflow_dt')
                        ,style = 'font-size: 80%')

                 )))

   ,tabPanel("Currency Hedging"
            ,sidebarLayout(
                 sb_panel
                ,mainPanel(
                     div(DT::dataTableOutput('display_hedges_dt')
                        ,style = 'font-size: 80%')
                 )))
))

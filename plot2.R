tickerSelected=input$txt
Ra <- tickerSelected %>%
        tq_get(get  = "stock.prices",
               from = "2010-01-01",
               to   = "2015-12-31") %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly", 
                     col_rename = "Ra")
Ra
stock_returns_monthly=Ra
wts_map <- tibble(
        symbols = c(tickerSelected[1:2]),
        weights = c(0.5, 0.5)
)
wts_map



portfolio_returns_monthly <- stock_returns_monthly %>%
        tq_portfolio(assets_col  = symbol, 
                     returns_col = Ra, 
                     weights     = wts_map, 
                     col_rename  = "Ra")            


portfolio_growth_monthly <- stock_returns_monthly %>%
        tq_portfolio(assets_col   = symbol, 
                     returns_col  = Ra, 
                     weights      = wts_map, 
                     col_rename   = "investment.growth",
                     wealth.index = TRUE) %>%
        mutate(investment.growth = investment.growth * 10000)




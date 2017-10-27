tickerSelected=input$txt
FROM=input$dates
Ra <- tickerSelected %>%
        tq_get(get  = "stock.prices",
               from = FROM,
               to   = Sys.Date()-1) %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly", 
                     col_rename = "Ra")
#
Ra
stock_returns_monthly=Ra
w=c()
for (i in tickerSelected){
        w[i]=1/length(tickerSelected)
}


wts_map <- tibble(
        symbols = c(tickerSelected[1:length(tickerSelected)]),
        weights = w
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
#s



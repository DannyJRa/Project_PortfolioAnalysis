#if (require(devtools)) install.packages("devtools")#if not already installed
#devtools::install_github("AnalytixWare/ShinySky")
#library(shinysky)
#shinysky::run.shinysky.example()

rm(list = ls())
library(shiny)
library(xts)
library(DT)
library(quantmod)
library(tidyquant)
library(shinysky)
library(ggplot2)
library(PerformanceAnalytics)
library(methods)
library(shinyBS)
library(plotly)
x <- list()
Data <- data.frame()
clo <- data.frame()

# Set ticker universe
ticker<- c("BIO.DE","ZIL2.DE","SIE.DE","IFX.DE","CBK.DE","2PP.F","BAYN.DE","SDF.DE","KBC.DE")


ui <- fluidPage(
        titlePanel("Portfolio Analysis"),
        tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
        sidebarLayout(
                sidebarPanel(
                        helpText("Define Portfolio"),
                        select2Input("txt","stock",choices=ticker,selected=ticker[1:2]),
                        dateInput("dates", "Date:", value = "2016-01-01"),
                        actionButton("go","submit") 
                        
                       
                        
                        
                ),
                
# Main Panel               
                mainPanel(
                        tabsetPanel(type="tab",
                                    tabPanel("Plot",plotOutput("plot")),
                                    tabPanel("summary",dataTableOutput("table")),
                                    tabPanel("close",dataTableOutput("table1")),
                                    tabPanel("CAPM",dataTableOutput("tableCAPM")),
                                    tabPanel("Wealth",plotlyOutput("plot2")))
                    
                )
        
                
        )
)

server <- function(input, output) {
        
      
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        stockData <- new.env()
        dataInput <- reactive({ 
                if(input$go==0){return()} #confirming button click
                isolate({
                        input$go
                        getSymbols(input$txt, src = "yahoo", env=stockData,from =as.Date(input$dates) )    
                        Data <- data.frame()
                        
                        validate(need(input$txt != "", label = "stock"))
                        for (i in 1:length(input$txt)) {
                                x[[i]] <- get(input$txt[i], pos=stockData)  # get data from stockData environment  
                                Data <- cbind(Data,diff(log(Cl(x[[i]]))))
                        }
                        Data
                })      
        })                 
        dataCAPM <- reactive({ 
                                if(input$go==0){return()} #confirming button click
                                isolate({
                                        input$go
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
                                        
                                        
                                        # Set name for BRK-A to BRK.A
                                        setSymbolLookup(GDAXI = list(name = "^GDAXI"))
                                        
                                        
                                        # Load BRK.A and ATT data
                                        getSymbols(c("GDAXI"))
                                        
                                        Rb <- "GDAXI" %>%
                                                tq_get(get  = "stock.prices",
                                                       from = "2010-01-01",
                                                       to   = "2015-12-31") %>%
                                                tq_transmute(select     = adjusted, 
                                                             mutate_fun = periodReturn, 
                                                             period     = "monthly", 
                                                             col_rename = "Rb")
                                        Rb
                                        
                                        RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
                                        RaRb
                                        
                                        RaRb_capm <- RaRb %>%
                                                tq_performance(Ra = Ra, 
                                                               Rb = Rb, 
                                                               performance_fun = table.CAPM)
                                        RaRb_capm
                                        })
                                                               
        })
        
                       
                        
           dataWealth <- reactive({ 
                                        if(input$go==0){return()} #confirming button click
                                        isolate({
                                                input$go                                
                                        
                                
                                        wts_map <- tibble(
                                        symbols = c(ticker[1:2]),
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
                                portfolio_growth_monthly
                                        })
           })                      
       
        
        Last_Close <- reactive({
                if(input$go==0){return()} #confirming button click
                isolate({
                        input$go
                        validate(need(input$txt != "", label = "stock"))
                        for (i in 1:length(input$txt)) {
                                x[[i]] <- get(input$txt[i], pos=stockData)  # get data from stockData environment  
                                clo <- cbind(clo,Cl(x[[i]]))
                        }
                        clo
                })
        })
        
        output$plot <- renderPlot(chart.Correlation(dataInput()))
        output$table <- DT::renderDataTable(datatable(as.data.frame(Cl(dataInput()))))
        output$table1 <- DT::renderDataTable(datatable(as.data.frame(Cl(Last_Close()))))
        output$tableCAPM <- DT::renderDataTable(datatable(as.data.frame(dataCAPM())))
        output$plot2 <- renderPlotly({
               
                # Objects in this file are defined each time this function is called
                source("plot2.R", local=TRUE)
                p=portfolio_growth_monthly%>%
                        ggplot(aes(x = date, y = investment.growth)) +
                        geom_line(size = 2, color = palette_light()[[1]]) +
                        labs(title = "Portfolio Growth",
                             subtitle = "of the defined Portfolio",
                             caption = "Now we can really visualize performance!",
                             x = "", y = "Portfolio Value") +
                        geom_smooth(method = "loess") +
                        theme_tq() +
                        scale_color_tq() +
                        scale_y_continuous(labels = scales::dollar) 
                ggplotly(p)
        })
        
}

shinyApp(ui, server)
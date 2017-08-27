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
ticker<- c("AMZN", "GOOG")


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
                                    tabPanel("Wealth",plotlyOutput("plot2"))
                                 
                                  )
                    
                )
        
                
        )
)

server <- function(input, output) {
        
      
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
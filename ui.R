library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(fpp3)

stocks <- read_csv("nyse_stocks.csv.zip")

ui <- fluidPage(
  navbarPage("Stocks",
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Input  date range to find the best performing stock in that time period", 
                                     start = min(stocks$date),
                                     end=max(stocks$date)),
                      dataTableOutput("max")))
)

shinyApp(ui,server)




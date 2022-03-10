library(shiny)
library(shinythemes)
library(readr)

stocks <- read_csv("nyse_stocks.csv.zip")

ui <- fluidPage(
  navbarPage("Stocks",
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Input  date range to find the best performing stock in that time period", 
                                     start = min(stock$Date),
                                     end=max(stock$Date)),
                      dataTableOutput("max")))
)






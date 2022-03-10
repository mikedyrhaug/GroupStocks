library(shiny)
library(shinythemes)
library(readr)

stocks <- read_csv("nyse_stocks.csv.zip")

ui <- fluidPage(
  navbarPage("Stocks",
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Date range", 
                                     start = min(stock$Date),
                                     end=max(stock$Date)),
                      dataTableOutput("max")))
)






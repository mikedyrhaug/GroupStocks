library(shiny)
library(shinythemes)
library(readr)

stocks <- read_csv("nyse_stocks.csv.zip")

ui <- fluidPage(
  navbarPage("Stocks",
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Date range", 
                                     start = min(gafa_stock$Date),
                                     end=max(gafa_stock$Date)),
                      dataTableOutput("max")))
)
getwd()





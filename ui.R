library(shiny)
library(shinythemes)
library(readr)
library(fpp3)
library(shinyWidgets)

stocks <- read_csv("nyse_stocks.csv.zip")

ui <- fluidPage(
    navbarPage("Stocks",
               tabPanel("Best Performing Stock",
                        dateRangeInput("dates", label = "Input  date range to find the best performing stock in that time period", 
                                       start = min(stocks$date),
                                       end=max(stocks$date)),
                        submitButton(text = "Submit"),
                        dataTableOutput("max")),
),
    setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  
)
           
) 


shinyApp(ui,server)






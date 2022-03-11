library(shiny)
library(shinythemes)
library(readr)
library(fpp3)

library(DT)

library(shinyWidgets)


stocks <- read_csv("nyse_stocks.csv.zip")
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

ui <- fluidPage(
  navbarPage("Stocks",
             #Feature 1
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Input  date range to find the best performing stock in that time period", 
                                     start = min(stocks$date),
                                     end=max(stocks$date)),
                      dataTableOutput("max")), 
             #Feature 2
             tabPanel("Sector Performance", 
                      radioButtons("sec", label = "select a sector",
                                   choices = unique(stocks$gics_sector) 
                      ),
                      dateRangeInput("d", label= "insert a date", 
                                     start=min(stocks$date), 
                                     end=max(stocks$date)),
                      dataTableOutput("tab2")),
             #Feature 3
             tabPanel("Compare Multiple Sectors",
                      checkboxGroupInput("CheckGroup", label = h3("Checkbox group"), 
                                         choices = unique(stocks$gics_sector)),
                      plotOutput("tab3")),
             #Feature 4
             tabPanel("Calculated Profit/Loss",
                      numericInput("num", label = "How much money would I have if I bought this many shares", value = 1),
                      dateRangeInput("date", label= "Buying and selling on these dates", 
                                     start=min(stocks$date), 
                                     end=max(stocks$date)),
                      textInput("text", 
                                label = "Of this company", 
                                value = "Netflix Inc."),
                      textOutput("tab4"))))

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









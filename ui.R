library(shiny)
library(shinythemes)
library(readr)
library(fpp3)
library(DT)

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

                     radioButtons("featureTwoSec", label = "select a sector",
                                   choices = unique(stocks$gics_sector) 
                      ),
                      dateRangeInput("featureTwod", label= "insert a date", 

                      radioButtons("sec", label = "select a sector",
                                   choices = unique(stocks$gics_sector) 
                      ),
                      dateRangeInput("d", label= "insert a date", 
                                     
                                     start=min(stocks$date), 
                                     end=max(stocks$date)),
                      dataTableOutput("tab2")),
             #Feature 3
             tabPanel("Compare Multiple Sectors",

                      radioButtons("featureThreesec", label = "select a sector",
                                   choices = unique(stocks$gics_sector) 
                      ),
                      dateRangeInput("featureThreed", label= "insert a date", 
                                     start=min(stocks$date), 
                                     end=max(stocks$date)),
                      dataTableOutput("tab2")),
             #Feature 4
             tabPanel("Calculated Profit/Loss",
                      numericInput("num", label = "How much money would I have if I bought this many shares", value = 1),
                      dateRangeInput("featureFourdate", label= "Buying and selling on these dates", 

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

                      textOutput("tab4")))

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





server <- function(input, output){
  output$max <- renderDataTable({
    first_date <- input$dates[1]
    last_date <- input$dates[2]
    first_day <- stocks %>%
      filter(date == first_date)
    last_day <- stocks %>%
      filter(date == last_date)
    first_last_day <- first_day %>%
      inner_join(
        last_day,
        by = "symbol",
        suffix = c("First", "Last"))
    f <- first_last_day %>%
      mutate(PercentChange = (openLast - openFirst) / openFirst) %>%
      mutate(PercentChangeStr = paste0(round(PercentChange * 100, 1), "%")) %>%
      select(symbol, openFirst, openLast, PercentChange, PercentChangeStr)
    
    f<-as.data.frame(f[which.max(f$PercentChange),])
    f
    
  })
  
  #Feature 2
  output$tab2 <- renderDataTable({
    p <- filter(stocks, stocks$gics_sector==input$featureTwoSec)
    first_date <- input$featureTwod[1]
    last_date <- input$featureTwod[2]
    first_day <- p %>%
      filter(date == first_date)
    last_day <- p %>%
      filter(date == last_date)
    first_last_day <- first_day %>%
      inner_join(
        last_day,
        by = "symbol",
        suffix = c("First", "Last"))
    featureTwo <- first_last_day %>%
      mutate(PercentChange = (openLast - openFirst) / openFirst) %>%
      mutate(PercentChangeStr = paste0(round(PercentChange * 100, 1), "%")) %>%
      select(symbol, openFirst, openLast, PercentChange, PercentChangeStr)
    featureTwo<-as.data.frame(featureTwo[which.max(featureTwo$PercentChange),])
    featureTwo
  })
  
  #Feature 3
  output$tab3 <- renderPlot({
    ag <- aggregate(close~gics_sector+date, stocks, FUN = mean)
    ag <- tsibble(ag, index=date, key = gics_sector)
    ag <- filter(ag, ag$gics_sector%in%input$CheckGroup)
    autoplot(ag)
  })
  
  #Feature 4
  output$tab4 <- renderPrint({
    number <- stocks[stocks$security==input$text,]
    first_day <- number[number$date==input$featureFourdate[1],]
    last_day <- number[number$date==input$featureFourdate[2],]
    dolla <- (last_day$open*input$num)-(first_day$open*input$num)
    paste("$", round(dolla,2))
  })
}

shinyApp(ui,server)





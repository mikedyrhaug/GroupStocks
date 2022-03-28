library(fpp3)
library(readr)
library(seasonal)
# Read zipped data
stocks <- read_csv("nyse_stocks.csv.zip")

# Convert to `tsibble()`
stocks$date <- as.Date(stocks$date)
stocks <- tsibble(stocks, index = date, key = symbol)

# 1 stock
selected_stock <- "AAPL"

stocks %>%
  filter(symbol == selected_stock) %>%
  autoplot(open) +
  labs(title = selected_stock)

# Multiple stocks
selected_stocks <- c("GOOG", "AAPL")

stocks %>%
  filter(symbol %in% selected_stocks) %>%
  autoplot(open)




#feature 2

ui <- fluidPage(
  radioButtons("sec", label = "select a sector",
               choices = unique(stocks$gics_sector) 
               ),
  dateRangeInput("d", label= "insert a date", 
                 start=min(stocks$date), 
                 end=max(stocks$date)),
  dataTableOutput("tab2")
)
  
  
  
  
server<-function(input,output){
  output$tab2 <- renderDataTable({
    p <- filter(stocks, stocks$gics_sector==input$sec)
    first_date <- input$d[1]
    last_date <- input$d[2]
    first_day <- p %>%
      filter(date == first_date)
    last_day <- p %>%
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
}

shinyApp(ui,server)
#feature3
ui <- fluidPage(
  checkboxGroupInput("CheckGroup", label = h3("Checkbox group"), 
                     choices = unique(stocks$gics_sector)),
  plotOutput("tab3")
)


server <- function(input,output){
  output$tab3 <- renderPlot({
    ag <- aggregate(close~gics_sector+date, stocks, FUN = mean)
    ag <- tsibble(ag, index=date, key = gics_sector)
    ag <- filter(ag, ag$gics_sector%in%input$CheckGroup)
    autoplot(ag)
  })
}
shinyApp(ui,server)
#feature 4

ui <- fluidPage(
  numericInput("num", label = "How much money would I have if I bought this many shares", value = 1),
  dateRangeInput("date", label= "Buying and selling on these dates", 
                 start=min(stocks$date), 
                 end=max(stocks$date)),
  textInput("text", 
            label = "Of this company", 
            value = "Netflix Inc."),
  textOutput("tab4")
)

server <- function(input, output){
  output$tab4 <- renderPrint({
    number <- stocks[stocks$security==input$text,]
    first_day <- number[number$date==input$date[1],]
    last_day <- number[number$date==input$date[2],]
    dolla <- (last_day$open*input$num)-(first_day$open*input$num)
    paste("$", round(dolla,2))
 })
}
shinyApp(ui,server)

ui <- fluidPage(
  navbarPage("Stocks",
             #Feature 1
             tabPanel("Best Performing Stock",
                      dateRangeInput("dates", label = "Input  date range to find the best performing stock in that time period", 
                                     start = min(stocks$date),
                                     end=max(stocks$date)),
                      dataTableOutput("max"))))

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
}
shinyApp(ui,server)


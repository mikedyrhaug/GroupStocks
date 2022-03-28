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
    first_day <- number[number$date==input$date[1],]
    last_day <- number[number$date==input$date[2],]
    dolla <- (last_day$open*input$num)-(first_day$open*input$num)
    paste("$", round(dolla,2))

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
    first_day <- number[number$date==input$date[1],]
    last_day <- number[number$date==input$date[2],]
    dolla <- (last_day$open*input$num)-(first_day$open*input$num)
    paste("$", round(dolla,2))
  })
}





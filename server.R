

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
        suffix = c("First", "Last")
      )
    f <- first_last_day %>%
      mutate(PercentChange = (openLast - openFirst) / openFirst) %>%
      mutate(PercentChangeStr = paste0(round(PercentChange * 100, 1), "%")) %>%
      select(symbol, security, openFirst, openLast, PercentChange, PercentChangeStr)
    
    f<-as.data.frame(f[which.max(f$PercentChange),])
    f
  })
}  


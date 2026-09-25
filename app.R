# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase
# Portfolio Visualization Dashboard using R Shiny

required_packages <- c("shiny", "ggplot2", "quantmod", "dplyr", "lubridate", "scales")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)

library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)
library(lubridate)
library(scales)

fetch_stock_data <- function(symbol, start_date, end_date) {
  tryCatch({
    x <- getSymbols(toupper(trimws(symbol)), src="yahoo",
                    from=as.Date(start_date), to=as.Date(end_date)+1,
                    auto.assign=FALSE)
    if (NROW(x) == 0) stop("No historical data returned.")
    x
  }, error=function(e) {
    stop(paste("Unable to retrieve stock data:", e$message))
  })
}

to_frame <- function(x) {
  data.frame(Date=as.Date(index(x)), Open=as.numeric(Op(x)),
             High=as.numeric(Hi(x)), Low=as.numeric(Lo(x)),
             Close=as.numeric(Cl(x)), Volume=as.numeric(Vo(x)))
}

aggregate_data <- function(df, timeframe) {
  if (timeframe == "Daily") return(df)
  z <- df %>% mutate(Period=if (timeframe=="Weekly")
    floor_date(Date, "week", week_start=1) else floor_date(Date, "month"))
  z %>% group_by(Period) %>% summarise(
    Date=first(Period), Open=first(Open), High=max(High, na.rm=TRUE),
    Low=min(Low, na.rm=TRUE), Close=last(Close),
    Volume=sum(Volume, na.rm=TRUE), .groups="drop")
}

add_indicators <- function(df, short_ma, long_ma, rsi_period,
                           macd_short, macd_long, macd_signal) {
  df$ShortMA <- as.numeric(SMA(df$Close, n=short_ma))
  df$LongMA <- as.numeric(SMA(df$Close, n=long_ma))
  df$RSI <- as.numeric(RSI(df$Close, n=rsi_period))
  m <- MACD(df$Close, nFast=macd_short, nSlow=macd_long,
            nSig=macd_signal, maType="EMA")
  df$MACD <- as.numeric(m[,1])
  df$MACDSignal <- as.numeric(m[,2])
  df$MACDHistogram <- df$MACD - df$MACDSignal
  df
}

generate_signals <- function(df) {
  s <- rep("Hold", nrow(df))
  if (nrow(df) >= 2) for (i in 2:nrow(df)) {
    if (!any(is.na(c(df$ShortMA[i], df$LongMA[i],
                     df$ShortMA[i-1], df$LongMA[i-1])))) {
      if (df$ShortMA[i] > df$LongMA[i] &&
          df$ShortMA[i-1] <= df$LongMA[i-1]) s[i] <- "Buy"
      if (df$ShortMA[i] < df$LongMA[i] &&
          df$ShortMA[i-1] >= df$LongMA[i-1]) s[i] <- "Sell"
    }
  }
  df$Signal <- s
  df
}

ui <- fluidPage(
  titlePanel("BDA400 Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Stock Symbol:", "AAPL"),
      dateRangeInput("date_range", "Select Date Range:",
                     start="2023-01-01", end="2023-07-01"),
      selectInput("time_frame", "Select Time Frame:",
                  c("Daily","Weekly","Monthly")),
      selectInput("chart_type", "Chart Type:",
                  c("Line","Area","Candlestick")),
      hr(),
      h4("Technical Indicators"),
      checkboxInput("show_ma", "Moving Averages", TRUE),
      numericInput("short_ma", "Short MA Period:", 20, min=2),
      numericInput("long_ma", "Long MA Period:", 50, min=2),
      checkboxInput("show_rsi", "RSI", FALSE),
      numericInput("rsi_period", "RSI Period:", 14, min=2),
      checkboxInput("show_macd", "MACD", FALSE),
      numericInput("macd_short", "MACD Fast Period:", 12, min=2),
      numericInput("macd_long", "MACD Slow Period:", 26, min=3),
      numericInput("macd_signal", "MACD Signal Period:", 9, min=2),
      hr(),
      checkboxInput("show_signals", "Show Buy/Sell Signals", TRUE),
      actionButton("refresh", "Refresh Stock Data")
    ),
    mainPanel(
      h4(textOutput("summary_title")),
      plotOutput("stock_chart", height="650px"),
      conditionalPanel("input.show_rsi == true",
                       plotOutput("rsi_chart", height="250px")),
      conditionalPanel("input.show_macd == true",
                       plotOutput("macd_chart", height="300px")),
      h4("Latest Trading Signal"),
      verbatimTextOutput("latest_signal"),
      h4("Recent Data"),
      tableOutput("data_table")
    )
  )
)

server <- function(input, output, session) {

  stock_data <- eventReactive(input$refresh, {
    req(input$symbol, input$date_range)
    validate(
      need(input$short_ma < input$long_ma,
           "Short MA must be smaller than Long MA."),
      need(input$macd_short < input$macd_long,
           "MACD Fast Period must be smaller than Slow Period.")
    )
    fetch_stock_data(input$symbol, input$date_range[1], input$date_range[2])
  }, ignoreNULL=FALSE)

  processed_data <- reactive({
    df <- to_frame(stock_data())
    df <- df %>% filter(Date >= as.Date(input$date_range[1]),
                         Date <= as.Date(input$date_range[2]))
    df <- aggregate_data(df, input$time_frame)
    validate(need(nrow(df) >= input$long_ma,
                  "Not enough observations. Choose a longer range or smaller Long MA."))
    df <- add_indicators(df, input$short_ma, input$long_ma, input$rsi_period,
                         input$macd_short, input$macd_long, input$macd_signal)
    generate_signals(df)
  })

  output$summary_title <- renderText({
    paste(toupper(trimws(input$symbol)), "|", input$time_frame, "|",
          input$chart_type, "Chart")
  })

  output$stock_chart <- renderPlot({
    df <- processed_data()
    p <- ggplot(df, aes(x=Date))

    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y=Close), linewidth=.8)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y=Close), alpha=.25) +
        geom_line(aes(y=Close), linewidth=.6)
    } else {
      p <- p +
        geom_segment(aes(x=Date, xend=Date, y=Low, yend=High),
                     linewidth=.5) +
        geom_rect(aes(xmin=Date-.3, xmax=Date+.3,
                      ymin=pmin(Open,Close), ymax=pmax(Open,Close)),
                  alpha=.75)
    }

    if (input$show_ma)
      p <- p + geom_line(aes(y=ShortMA), linewidth=.8, na.rm=TRUE) +
        geom_line(aes(y=LongMA), linewidth=.8, na.rm=TRUE)

    if (input$show_signals) {
      s <- df %>% filter(Signal != "Hold")
      if (nrow(s) > 0)
        p <- p + geom_text(data=s,
          aes(y=ifelse(Signal=="Buy",Low,High), label=Signal),
          fontface="bold", na.rm=TRUE)
    }

    p + labs(title=paste(toupper(trimws(input$symbol)),
                         "Stock Price and Technical Indicators"),
             x="Date", y="Price") +
      theme_minimal() +
      scale_x_date(date_labels="%Y-%m-%d") +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })

  output$rsi_chart <- renderPlot({
    req(input$show_rsi)
    df <- processed_data()
    ggplot(df, aes(Date, RSI)) + geom_line(linewidth=.8) +
      geom_hline(yintercept=70, linetype="dashed") +
      geom_hline(yintercept=30, linetype="dashed") +
      coord_cartesian(ylim=c(0,100)) +
      labs(title=paste0("RSI (",input$rsi_period,"-period)"),
           x="Date", y="RSI") + theme_minimal()
  })

  output$macd_chart <- renderPlot({
    req(input$show_macd)
    df <- processed_data()
    ggplot(df, aes(Date)) +
      geom_col(aes(y=MACDHistogram), alpha=.5) +
      geom_line(aes(y=MACD), linewidth=.8) +
      geom_line(aes(y=MACDSignal), linewidth=.8) +
      labs(title="MACD", x="Date", y="Value") + theme_minimal()
  })

  output$latest_signal <- renderText({
    df <- processed_data()
    s <- df %>% filter(Signal != "Hold")
    if (nrow(s)==0) return("No Buy/Sell crossover signal was generated.")
    z <- slice_tail(s, n=1)
    paste0("Signal: ",z$Signal,"\nDate: ",z$Date,
           "\nClose: ",round(z$Close,2),
           "\nShort MA: ",round(z$ShortMA,2),
           "\nLong MA: ",round(z$LongMA,2))
  })

  output$data_table <- renderTable({
    processed_data() %>%
      select(Date,Open,High,Low,Close,Volume,ShortMA,LongMA,
             RSI,MACD,MACDSignal,Signal) %>%
      slice_tail(n=10) %>%
      mutate(across(where(is.numeric), ~round(.x,2)))
  })
}

shinyApp(ui, server)

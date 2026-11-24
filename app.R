library(shiny)
library(tidyverse)
library(fpp3)
library(here)
library(urca)

aus_wine <- read_csv(
  here("data", "AustralianWines.csv"),
  na = "*",
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(
    Month = mdy(str_replace(Month, "-", "-01-")) |>
      yearmonth())

wine_ts <- aus_wine |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(
    key = Varietal,
    index = Month)

wine_ts

ui <- fluidPage(
  
  titlePanel("Australian Wine Sales Forecasting"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "varietal",
        "Select wine varietal:",
        choices = unique(wine_ts$Varietal),
        selected = "Fortified",
        multiple = TRUE
      ),
      
      dateRangeInput(
        "date_range",
        "Select date range:",
        start = as.Date(min(wine_ts$Month)),
        end = as.Date(max(wine_ts$Month))
      ),
      
      dateInput(
        "train_end",
        "Training period ends:",
        value = as.Date("1993-12-01"),
        min = as.Date("1982-01-01"),
        max = as.Date("1994-12-01")
      ),
      
      numericInput(
        "h",
        "Forecast horizon (months):",
        value = 12,
        min = 1,
        max = 24,
        step = 1),
    ),
    
    mainPanel(
      textOutput("validation_warning"),
      plotOutput("overview_plot"),
      plotOutput("forecast_plot"),
      tableOutput("accuracy_table"),
      tableOutput("model_specs"),
      tableOutput("best_model"))))

server <- function(input, output) {
  
  filtered_wine <- reactive({
    
    wine_ts |>
      filter(
        Varietal %in% input$varietal,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2]))})
  
  training_data <- reactive({
    
    filtered_wine() |>
      filter(Month <= yearmonth(input$train_end))})
  
  validation_data <- reactive({
    
    filtered_wine() |>
      filter(Month > yearmonth(input$train_end)) |>
      group_by_key() |>
      slice_head(n = input$h) |>
      ungroup()})
  
  output$validation_warning <- renderText({
    
    available_months <- nrow(
      filtered_wine() |>
        filter(Month > yearmonth(input$train_end)) |>
        distinct(Month))
    
    if (available_months < input$h) {
      paste(
        "Note: Only",
        available_months,
        "validation month(s) are available after the selected training cutoff.",
        "Choose an earlier training end date to evaluate the full",
        input$h,
        "month forecast horizon."
      )
    } else {
      ""}})
  
  model_fit <- reactive({
    
    training_data() |>
      model(
        TSLM = TSLM(Sales ~ trend() + season()),
        ETS = ETS(Sales),
        ARIMA = ARIMA(Sales))})
  
  output$model_specs <- renderTable({
    
    model_fit() |>
      as_tibble() |>
      transmute(
        Varietal,
        TSLM = format(TSLM),
        ETS = format(ETS),
        ARIMA = format(ARIMA))})
  
  validation_fc <- reactive({
    
    model_fit() |>
      forecast(new_data = validation_data())})
  
  output$forecast_plot <- renderPlot({
    
    validation_fc() |>
      autoplot(
        training_data(),
        level = c(80, 95)
      ) +
      facet_grid(
        Varietal ~ .model,
        scales = "free_y"
      ) +
      labs(
        title = "Forecast Comparison",
        x = "Month",
        y = "Sales")})
  
  output$accuracy_table <- renderTable({
    
    training_accuracy <- model_fit() |>
      accuracy() |>
      select(
        Varietal,
        .model,
        RMSE,
        MAE,
        MAPE
      ) |>
      mutate(Window = "Training")
    
    validation_accuracy <- validation_fc() |>
      accuracy(validation_data()) |>
      select(
        Varietal,
        .model,
        RMSE,
        MAE,
        MAPE
      ) |>
      mutate(Window = "Validation")
    
    bind_rows(
      training_accuracy,
      validation_accuracy
    ) |>
      select(
        Varietal,
        Window,
        .model,
        RMSE,
        MAE,
        MAPE
      ) |>
      arrange(
        Varietal,
        Window,
        RMSE
      ) |>
      mutate(
        across(
          c(RMSE, MAE, MAPE),
          ~ round(.x, 1)))})
  
  output$best_model <- renderTable({
    
    validation_fc() |>
      accuracy(validation_data()) |>
      select(
        Varietal,
        .model,
        RMSE,
        MAE,
        MAPE
      ) |>
      group_by(Varietal) |>
      slice_min(RMSE, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(
        RMSE = round(RMSE, 1),
        MAE = round(MAE, 1),
        MAPE = round(MAPE, 1)
      ) |>
      rename(
        `Best Model` = .model)})
  
  output$overview_plot <- renderPlot({
    
    filtered_wine() |>
      autoplot(Sales) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(
        title = "Australian Wine Sales",
        x = "Month",
        y = "Sales")})}

shinyApp(ui = ui, server = server)

library(shiny)
library(dplyr)
library(readr)
library(plotly)

# Load your demo menu file
#path <- "/Users/saeed/Library/CloudStorage/OneDrive-DalhousieUniversity/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working/demo_menu_with_calories.csv"
path <- "demo_menu_with_calories.csv"

# Load menu data
menu_df <- read_csv(path)



# Daily calorie target
DAILY_NEED <- 2500

ui <- fluidPage(
  titlePanel("Daily Meal Footprint Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Meal Selection"),
      
      h4("Breakfast"),
      uiOutput("breakfast_ui"),
      
      br(),
      
      h4("Lunch"),
      uiOutput("lunch_ui"),
      
      br(),
      
      h4("Dinner"),
      uiOutput("dinner_ui"),
      
      br(), br(),
      actionButton("reset", "Reset All", width = "100%"),
      width = 4
    ),
    
    mainPanel(
      h3("Your Daily Summary"),
      
      br(),
      
      h4(htmlOutput("calorie_summary")),
      plotlyOutput("calorie_bar", height = "140px"),
      
      br(), br(),
      
      h4(textOutput("co2_summary")),
      div(
        plotlyOutput("co2_bar", height = "320px"),
        style = "max-width:420px;"
      ),
      
      br(), br(),
      
      h4("Meal Shares"),
      textOutput("share_text"),
      br(),
      fluidRow(
        column(6, tableOutput("share_table")),
        column(6, plotlyOutput("share_pie", height = "320px"))
      ),
      
      br(), br(),
      
      div(
        htmlOutput("beef_swap_text"),
        style="margin-bottom:60px;"
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  
  # Helper to generate UI blocks for each meal
  mealUI <- function(meal_name) {
    foods <- menu_df %>% filter(meal == meal_name)
    n <- nrow(foods)
    
    left_idx  <- 1:ceiling(n / 2)
    right_idx <- (ceiling(n / 2) + 1):n
    
    makeCol <- function(idxs) {
      idxs <- idxs[idxs <= n]
      lapply(idxs, function(i) {
        div(style = "margin-bottom:4px; padding:0px;",
            div(style="display:flex; justify-content:space-between; align-items:center;",
                tags$span(style="font-size:12px;", foods$food_short[i]),
                numericInput(
                  inputId = paste0(meal_name, "_qty_", i),
                  label = NULL,
                  value = 0,
                  min = 0,
                  max = 10,
                  step = 1,
                  width = "60px"
                )
            )
        )
      })
    }
    
    div(
      style="margin:0; padding:0;",
      fluidRow(
        column(6, makeCol(left_idx), style="padding-right:4px;"),
        column(6, makeCol(right_idx), style="padding-left:4px;")
      )
    )
  }
  
  
  output$breakfast_ui <- renderUI(mealUI("Breakfast"))
  output$lunch_ui     <- renderUI(mealUI("Lunch"))
  output$dinner_ui    <- renderUI(mealUI("Dinner"))
  
  # Calculate totals for a single meal
  mealTotals <- function(meal_name) {
    foods <- menu_df %>% filter(meal == meal_name)
    
    total_cal <- 0
    total_co2 <- 0
    beef_qty <- 0
    
    for (i in 1:nrow(foods)) {
      qty <- input[[paste0(meal_name, "_qty_", i)]]
      
      if (!is.null(qty) && qty > 0) {
        total_cal <- total_cal + qty * foods$calories[i]
        total_co2 <- total_co2 + qty * foods$CO2e[i]
        
        if (foods$food_short[i] == "Beef") {
          beef_qty <- beef_qty + qty
        }
      }
    }
    
    list(cal = total_cal, co2 = total_co2, beef = beef_qty)
  }
  
  # Full day summary
  dailyTotals <- reactive({
    b <- mealTotals("Breakfast")
    l <- mealTotals("Lunch")
    d <- mealTotals("Dinner")
    
    list(
      cal = b$cal + l$cal + d$cal,
      co2 = b$co2 + l$co2 + d$co2,
      meal_share = c(b$cal, l$cal, d$cal),
      beef_total_qty = b$beef + l$beef + d$beef
    )
  })
  
  # Calorie output with explicit 2500 reference
  output$calorie_summary <- renderUI({
    tot <- dailyTotals()$cal
    
    if (tot < DAILY_NEED) {
      HTML(
        paste0(
          "Total calories: ", round(tot), " kcal. ",
          "You need ", DAILY_NEED - round(tot),
          " more kcal to reach ", DAILY_NEED, " kcal."
        )
      )
    } else {
      excess <- round(tot - DAILY_NEED)
      HTML(
        paste0(
          "Total calories: ", round(tot), " kcal. ",
          "<span style='color:red;'>You exceeded the daily need (",
          DAILY_NEED, " kcal) by ", excess, " kcal.</span>"
        )
      )
    }
  })
  
  # CO2 output
  output$co2_summary <- renderText({
    paste("Total daily CO2 emissions:", round(dailyTotals()$co2, 2), "kg CO2e")
  })
  
  output$calorie_bar <- renderPlotly({
    total <- dailyTotals()$cal
    limit <- DAILY_NEED
    
    within <- min(total, limit)
    excess <- max(total - limit, 0)
    max_x <- max(limit, total, 1)
    
    df <- data.frame(
      segment = c("Within target", "Excess"),
      value = c(within, excess)
    ) %>%
      dplyr::filter(value > 0)
    
    # Explicit color control
    df$color <- ifelse(df$segment == "Within target", "darkgreen", "red")
    
    p <- plot_ly(
      df,
      x = ~value,
      y = ~factor(""),
      type = "bar",
      orientation = "h",
      hovertemplate = ~paste0(segment, ": ", round(value), " kcal"),
      marker = list(color = df$color)   # fixed coloring
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(range = c(0, max_x), title = "kcal"),
        yaxis = list(showticklabels = FALSE, title = ""),
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 10, b = 30),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    p
  })
  
  # Helper for shaded red based on 5 kg steps
  co2Color <- function(value) {
    steps <- 20
    cols <- colorRampPalette(c("mistyrose", "red", "darkred"))(steps)
    index <- min(steps, floor(value / 5) + 1)
    cols[max(1, index)]
  }
  
  # Vertical CO2 bar using plotly with y axis and 20 percent headroom
  output$co2_bar <- renderPlotly({
    total <- dailyTotals()$co2
    max_y <- max(total * 1.2, 1)
    
    df <- data.frame(
      x = "Emissions",
      y = total
    )
    
    p <- plot_ly(
      df,
      x = ~x,
      y = ~y,
      type = "bar",
      hovertemplate = ~paste0(round(y, 2), " kg CO2e"),
      marker = list(color = co2Color(total))
    ) %>%
      layout(
        xaxis = list(showticklabels = FALSE, title = "", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(
          title = "kg CO2e",
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(0, max_y)
        ),
        showlegend = FALSE,
        margin = list(l = 40, r = 10, t = 10, b = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = list(
          list(
            x = 0.5,
            y = total / 2,
            text = paste0(round(total, 2), " kg CO2e"),
            showarrow = FALSE,
            xref = "paper",
            yref = "y",
            font = list(size = 14)
          )
        )
      )
    
    p
  })
  
  # Meal shares text
  output$share_text <- renderText({
    shares <- dailyTotals()$meal_share
    tot <- sum(shares)
    
    if (tot == 0) return("No meals selected yet.")
    
    pct <- round(shares / tot * 100)
    
    paste0(
      "Breakfast: ", round(shares[1]), " kcal (", pct[1], " percent); ",
      "Lunch: ", round(shares[2]), " kcal (", pct[2], " percent); ",
      "Dinner: ", round(shares[3]), " kcal (", pct[3], " percent)."
    )
  })
  
  # Meal shares table
  output$share_table <- renderTable({
    shares <- dailyTotals()$meal_share
    tot <- sum(shares)
    if (tot == 0) {
      return(NULL)
    }
    
    pct <- round(shares / tot * 100)
    
    data.frame(
      Meal = c("Breakfast", "Lunch", "Dinner"),
      Calories = round(shares),
      Percent = paste0(pct, " %")
    )
  })
  
  # Meal shares pie chart using plotly
  output$share_pie <- renderPlotly({
    shares <- dailyTotals()$meal_share
    if (sum(shares) == 0) return(NULL)
    
    df <- data.frame(
      meal = c("Breakfast", "Lunch", "Dinner"),
      cal = shares
    )
    df$perc <- round(df$cal / sum(df$cal) * 100)
    
    p <- plot_ly(
      df,
      labels = ~meal,
      values = ~cal,
      type = "pie",
      text = ~paste0(meal, " ", perc, " %"),
      textposition = "inside",
      textinfo = "text",
      hovertemplate = ~paste0(meal, "<br>", round(cal), " kcal (", perc, " %)"),
      marker = list(colors = c("#1f77b4", "#d62728", "#2ca02c"))
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 20, r = 20, t = 20, b = 20),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    p
  })
  
  # Beef replacement tip
  output$beef_swap_text <- renderUI({
    qty <- dailyTotals()$beef_total_qty
    if (qty == 0) return("")
    
    beef_val <- menu_df %>% filter(food_short == "Beef") %>% pull(CO2e)
    chicken_val <- menu_df %>% filter(food_short == "Chicken") %>% pull(CO2e)
    
    reduction <- qty * (beef_val - chicken_val)
    total_co2 <- dailyTotals()$co2
    
    if (total_co2 <= 0) {
      pct <- 0
    } else {
      pct <- round(reduction / total_co2 * 100)
    }
    
    HTML(
      paste0(
        "<div style='text-align:center;'>",
        "<div style='font-size:18px; font-weight:bold;'>Swap with chicken</div>",
        "<div style='font-size:20px; font-weight:bold; color:darkgreen;'>",
        round(reduction, 2), " kg CO2e reduced</div>",
        "<div style='font-size:18px; font-weight:bold; color:darkgreen;'>",
        pct, " percent lower emissions</div>",
        "</div>"
      )
    )
  })
  
  # Reset button
  observeEvent(input$reset, {
    for (meal_name in c("Breakfast", "Lunch", "Dinner")) {
      foods <- menu_df %>% filter(meal == meal_name)
      for (i in 1:nrow(foods)) {
        updateNumericInput(session, paste0(meal_name, "_qty_", i), value = 0)
      }
    }
  })
}

shinyApp(ui, server)

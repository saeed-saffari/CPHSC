library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

library(shinyWidgets)


#file_path <- "/Users/saeed/Library/CloudStorage/OneDrive-DalhousieUniversity/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working/export_result_oct21.xlsx"
file_path <- "export_result_oct21.xlsx"

# --- Food IDs (must match Excel exactly) ---
all_foods <- c(
  "A_beef_consumed_ON","A_milk_consumed_ON",
  "Supp_EnsureChocolate_Water_WheyProtein_Sugar",
  "PuddingCup_Proxy_Milk_Eggs_Sugar_Consumed",
  "S_icecream_consumed_ON",
  "Supp_EnsureVanillaStrawberry_Water_SoyProtein_Milk",
  "Supp_BoostDrink_Water_Sugar_WheyProtein_Poore&Nemecek",
  "A_cheese_consumed_ON","A_egg_consumed_ON","A_chicken_consumed_ON",
  "Supp_Isosource+HN_MilkProtein_SoyProtein_Water_Sugar",
  "V_tomato_unheated_greenhouse_consumed_ON","A_yogurt_consumed_ON",
  "MagicCup_Proxy_Icecream ","F_banana_consumed_ON",
  "Supp_Glucerna_Water_Milk_SoyProtein","Muffin_Proxy_Bread_Sugar",
  "Turkey_Proxy_Chicken_Consumed","G_bread_consumed_ON",
  "Haddock_Bianchietal2022","Sorbet_Water_Sugar_Orange",
  "F_mandarin_consumed_ON","Apple_Oat_Sugar","G_pasta_consumed_ON",
  "B_coffee_brewed_consumed_ON","Vegetables_MontegoBlend",
  "F_orange_juice_consumed_ON","Peptamen_Water_Sugar_WheyProtein",
  "G_oat_consumed_ON","A_butter_consumed_ON","Tuna_Bianchietal2022",
  "A_mixed_meat_consumed_ON","V_potato_consumed_ON",
  "Supp_Resource_Water_Milk_SoyProtein_Sugar","FrenchToast_Proxy_Eggs_Bread",
  "F_apple_consumed_ON","Cookies_Proxy_Bread+Sugar","Pizza_Flatbread_Cheese_Flatbred_Tomato",
  "V_lettuce_consumed_ON","Supp_NovaSource_Water_Milk_SoyProtein",
  "B_carbonated_drink_consumed_ON","G_rice_consumed_ON","V_pepper_consumed_ON",
  "PromoteVanilla_Water_SoyProtein_Sugar","S_candied_fruit_consumed_ON",
  "O_margarine_consumed_ON","V_carrot_consumed_ON","A_pork_consumed_ON",
  "PeachJuice_Proxy_OrangeJuice"
)

# --- Short labels (same order and same length!) ---
label_food <- c(
  "Beef","Milk",
  "Chocolate_Water_WheyProtein",
  "PuddingCup",
  "Icecream",
  "VanillaStrawberry_Water_SoyProtein",
  "BoostDrink_Water_Sugar_WheyProtein",
  "Cheese","Egg","Chicken",
  "Isosource+HN_MilkProtein_SoyProtein",
  "Tomato","Yogurt",
  "MagicCup","Banana",
  "Glucerna_Water_Milk_SoyProtein","Muffin",
  "Turkey","Bread",
  "Haddock","Sorbet_Water_Sugar_Orange",
  "Mandarin","Apple_Oat_Sugar","Pasta",
  "Coffee","Vegetables",
  "Orange_juice","Peptamen_Water_Sugar_WheyProtein",
  "Oat","Butter","Tuna",
  "Mixed_meat","Potato",
  "Resource_Water_Milk_SoyProtein","FrenchToast",
  "Apple","Cookies","Pizza",
  "Lettuce","NovaSource_Water_Milk_SoyProtein",
  "Carbonated_drink","Rice","Pepper",
  "PromoteVanilla_Water_SoyProtein","Candied_fruit",
  "Margarine","Carrot","Pork",
  "PeachJuice"
)

stopifnot(length(all_foods) == length(label_food))  # safety check

# Create named vector: names = long IDs (values returned), labels = short text
food_choices <- setNames(all_foods, label_food)

# --- Load months automatically ---
available_sheets <- excel_sheets(file_path)
month_order <- available_sheets

# --- Extract functions (same as before) ---
extract_table1 <- function(sheet) {
  raw <- read_excel(file_path, sheet = sheet, col_names = FALSE)
  if (nrow(raw) == 0) return(tibble())
  col1 <- tolower(trimws(as.character(raw[[1]])))
  hdr_idx <- which(col1 == "food_items")
  if (!length(hdr_idx)) return(tibble())
  hdr_idx <- hdr_idx[1]
  t2_idx <- which(col1 == "food_items" & seq_along(col1) > hdr_idx)
  end_t1 <- if (length(t2_idx)) t2_idx[1] - 1 else nrow(raw)
  dat <- raw[(hdr_idx + 1):end_t1, 1:5, drop = FALSE]
  hdr <- tolower(trimws(as.character(unlist(raw[hdr_idx, 1:5]))))
  names(dat) <- hdr
  if (!("food_items" %in% names(dat) && "gwp_x_kg" %in% names(dat))) return(tibble())
  dat
}
clean_table1 <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble(food_items=character(), gwp_x_kg=numeric()))
  names(df) <- tolower(names(df))
  if (!all(c("food_items","gwp_x_kg") %in% names(df)))
    return(tibble(food_items=character(), gwp_x_kg=numeric()))
  df |>
    mutate(food_items = trimws(as.character(food_items)),
           gwp_x_kg = suppressWarnings(as.numeric(gwp_x_kg))) |>
    group_by(food_items) |>
    summarise(gwp_x_kg = sum(gwp_x_kg, na.rm = TRUE), .groups="drop")
}
tables <- lapply(available_sheets, \(s) tryCatch(extract_table1(s), error = \(e) tibble()))
names(tables) <- available_sheets

# --- UI ---
ui <- fluidPage(
  tags$style(HTML("
    body { background-color:#f8fafc; font-family:'Inter',sans-serif; }
    .card { background:white; border-radius:16px; box-shadow:0 2px 10px rgba(0,0,0,0.05); padding:1.2rem; }
    h2 { font-weight:600; color:#1e293b; }
    #months .shiny-options-group { column-count: 3; column-gap: 12px; }
    @media (max-width: 1200px) { #months .shiny-options-group { column-count: 2; } }
    @media (max-width: 700px)  { #months .shiny-options-group { column-count: 1; } }
  ")),
  
  titlePanel("Hospital Food Emissions – GWP Plotter"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # 🔹 Searchable checkbox picker for foods
      pickerInput(
        inputId = "foods",
        label = "Select food items:",
        choices = food_choices,       # named vector: names = labels, values = IDs
        selected = c("A_milk_consumed_ON", "S_icecream_consumed_ON"),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,        # search bar
          `actions-box` = TRUE,        # select all / deselect all
          `selected-text-format` = "count > 2",
          `virtual-scroll` = 10
        )
      ),
      # 🟩 show selected foods below picker
      uiOutput("selected_foods_display"),
      
      tags$hr(),
      
      # 🔹 Multi-column month selector
      checkboxGroupInput(
        inputId = "months",
        label = "Select months:",
        choices = month_order,
        selected = month_order
      ),
      
      fluidRow(
        column(6, actionButton("select_all_months", "Select All")),
        column(6, actionButton("clear_months", "Clear"))
      ),
      
      tags$hr(),
      
      checkboxInput("sort_avg", "Sort by yearly average (descending)", TRUE),
      downloadButton("downloadPlot", "⬇️ Download PNG", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      div(class = "card", plotOutput("gwpPlot", height = "600px"))
    )
  )
)


server <- function(input, output, session) {
  
  # --- Month buttons
  observeEvent(input$select_all_months, {
    updateCheckboxGroupInput(session, "months", selected = month_order)
  })
  observeEvent(input$clear_months, {
    updateCheckboxGroupInput(session, "months", selected = character(0))
  })
  
  # --- Food buttons (built-in actions-box covers this, but keeping manual option)
  observeEvent(input$select_all_foods, {
    updatePickerInput(session, "foods", selected = all_foods)
  })
  observeEvent(input$clear_foods, {
    updatePickerInput(session, "foods", selected = character(0))
  })
  
  # --- Build the plot dynamically
  make_plot <- reactive({
    req(input$foods)
    sel_foods  <- input$foods
    sel_months <- input$months
    if (length(sel_months) == 0) return(NULL)
    
    records <- lapply(names(tables), function(s) {
      if (!s %in% sel_months) return(NULL)
      cdf <- clean_table1(tables[[s]])
      if (nrow(cdf) == 0) return(NULL)
      cdf$month <- s
      cdf
    }) |> bind_rows()
    
    if (is.null(records) || nrow(records) == 0) return(NULL)
    
    df_plot <- records |> filter(food_items %in% sel_foods)
    if (nrow(df_plot) == 0) return(NULL)
    
    avg_order <- df_plot |>
      group_by(food_items) |>
      summarise(avg_gwp = mean(gwp_x_kg, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(avg_gwp)) |>
      pull(food_items)
    
    if (isTRUE(input$sort_avg))
      df_plot$food_items <- factor(df_plot$food_items, levels = avg_order)
    else
      df_plot$food_items <- factor(df_plot$food_items, levels = sel_foods)
    
    df_plot$month <- factor(df_plot$month, levels = sel_months)
    x_labels <- label_food[match(levels(df_plot$food_items), all_foods)]
    
    ggplot(df_plot, aes(x = food_items, y = gwp_x_kg, fill = month)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.6) +
      scale_x_discrete(labels = x_labels) +
      scale_fill_manual(values = brewer.pal(12, "Paired")) +
      labs(y = "kg CO₂e", x = NULL, fill = "Month") +
      theme_minimal(base_family = "Helvetica") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.3),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.2, linetype = "dotted"),
        legend.position = "right"
      )
  })
  
  output$selected_foods_display <- renderUI({
    req(input$foods)
    selected_labels <- label_food[match(input$foods, all_foods)]
    if (length(selected_labels) == 0) return(NULL)
    
    tagList(
      tags$div(
        style = "margin-top:6px; font-size:0.9em; color:#374151;",
        HTML(paste0(
          "<strong>Selected:</strong> ",
          paste(selected_labels, collapse = ", ")
        ))
      )
    )
  })
  
  # --- Render plot
  output$gwpPlot <- renderPlot({
    p <- make_plot()
    if (is.null(p)) {
      plot.new(); text(0.5, 0.5, "No matching data found.", cex = 1.2)
    } else print(p)
  })
  
  # --- Download as PNG
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("Hospital_Food_GWP_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = make_plot(), width = 12, height = 6, dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui,server)

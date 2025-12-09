# install.packages(c("shiny", "plotly", "readxl", "dplyr"))

library(shiny)
library(plotly)
library(readxl)
library(dplyr)

library(jsonlite)



ui <- fluidPage(
  titlePanel("Hospital Food Emissions Sankey"),
  fluidRow(
    column(
      8,
      plotlyOutput("sankeyPlot", height = "600px")
    ),
    column(
      4,
      h4("Details"),
      textOutput("hoverTitle"),
      verbatimTextOutput("hoverInfo")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Load Excel and calculate totals
  path <- "Hospital_GWP_Data_dec4_beta.xlsx"
  #path <- "/Users/saeed/Library/CloudStorage/OneDrive-DalhousieUniversity/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working/Hospital_GWP_Data.xlsx"
  df <- read_excel(path)
  
  months <- c("Dec","Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov")
  
  df <- df %>%
    mutate(Total_GWP = rowSums(across(all_of(months)), na.rm = TRUE))
  
  # Optional: add kcal and protein if you have them
  # For demo, create fake columns if missing
  if (!"kcal" %in% names(df)) df$kcal <- round(runif(nrow(df), 20000, 80000))
  if (!"protein" %in% names(df)) df$protein <- round(runif(nrow(df), 2000, 10000))
  
  # 2. Define groups
  animal <- c("Beef", "All Other Animal Meat/Fish", "Dairy & Alternatives", "Supplement Drinks")
  non_animal <- setdiff(df$Category, animal)
  
  animal_total <- sum(df$Total_GWP[df$Category %in% animal])
  non_animal_total <- sum(df$Total_GWP) - animal_total
  
  # 3. Create links
  links <- data.frame(
    source = c("Total", "Total",
               rep("Animal-based", length(animal)),
               rep("Non-animal-based", length(non_animal))),
    target = c("Animal-based", "Non-animal-based",
               animal,
               non_animal),
    value = c(animal_total, non_animal_total,
              df$Total_GWP[df$Category %in% animal],
              df$Total_GWP[df$Category %in% non_animal])
  )
  
  # 4. Create nodes
  nodes <- data.frame(name = unique(c(links$source, links$target)))
  
  # Map IDs
  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1
  
  # 5. Add dummy kcal and protein for hover info
  # (replace with real data if available)
  links$kcal <- round(runif(nrow(links), 1000, 5000))
  links$protein <- round(runif(nrow(links), 100, 500))
  
  write_json(nodes$name, "nodes_sankey.json", pretty = TRUE)
  write_json(links[, c("IDsource","IDtarget","value")], "links_sankey.json", pretty = TRUE)
  
  # 6. Render Sankey with detailed hover popups
  output$sankeyPlot <- renderPlotly({
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(
        label = nodes$name,
        pad = 15,
        thickness = 20,
        line = list(color = "gray", width = 0.5)
      ),
      link = list(
        source = links$IDsource,
        target = links$IDtarget,
        value = links$value,
        customdata = cbind(links$kcal, links$protein),
        hovertemplate = paste(
          "<b>%{source.label} → %{target.label}</b><br>",
          "GHG: %{value:,.0f} kg CO₂e<br>",
          #"Energy: %{customdata[0]:,.0f} kcal<br>",
          #"Protein: %{customdata[1]:,.0f} g<br>",
          "<extra></extra>"
        ),
        #color = "rgba(100,149,237,0.6)"
        color = "rgba(200,200,200,0.6)"
        
      )
    ) %>%
      layout(
        title = list(text = "Total Hospital Food GHG Emissions", font = list(size = 18)),
        font = list(size = 13)
      )
  })
  
  # 7. Capture hover event to show details on right
  observeEvent(event_data("plotly_hover"), {
    ed <- event_data("plotly_hover")
    if (is.null(ed)) return()
    i <- ed$pointNumber + 1
    source <- nodes$name[links$IDsource[i] + 1]
    target <- nodes$name[links$IDtarget[i] + 1]
    
    output$hoverTitle <- renderText(paste(source, "→", target))
    output$hoverInfo <- renderText({
      paste0("GHG: ", round(links$value[i]/1000), " Tons CO₂e\n",
             "", "",
             "", "")
    })
  })
  
  output$hoverTitle <- renderText("Hover over a flow to see details")
  output$hoverInfo <- renderText("")
}

shinyApp(ui, server)





# install packages if not yet installed
# install.packages(c("readxl", "dplyr", "networkD3"))

library(readxl)
library(dplyr)
library(networkD3)

# 1. Load your Excel file
path <- "Hospital_GWP_Data.xlsx"
#path <- "/Users/saeed/Library/CloudStorage/OneDrive-DalhousieUniversity/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working/Hospital_GWP_Data.xlsx"
df <- read_excel(path)

# 2. Calculate total GWP for each category
months <- c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
df <- df %>%
  mutate(Total_GWP = rowSums(across(all_of(months))))

# 3. Define groups
animal <- c("Beef", "All Other Animal Meat/Fish", "Dairy & Alternatives")
non_animal <- setdiff(df$Category, animal)

animal_total <- sum(df$Total_GWP[df$Category %in% animal])
non_animal_total <- sum(df$Total_GWP) - animal_total

# 4. Create links
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

# 5. Create nodes
nodes <- data.frame(name = unique(c(links$source, links$target)))

# 6. Map source/target IDs
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1


# 7. Plot Sankey
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  fontSize = 14,
  nodeWidth = 30,
)


library(plotly)

p <- plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(label = nodes$name, pad = 15, thickness = 20),
  link = list(
    source = links$IDsource,
    target = links$IDtarget,
    value = links$value,
    hovertemplate = paste(
      "<b>%{source.label} → %{target.label}</b><br>",
      "GHG: %{value} kg CO₂e<br>",
      "<extra></extra>"
    )
  )
)
p

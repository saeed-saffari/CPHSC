library(dplyr)
library(arrow)
library(readr)
library(janitor)
library(openxlsx)

setwd("~/Sharing - Dalhousie/OneDrive - Dalhousie University/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working")

# possible variants of "mass standardized in grams"
fiwo_possible_names <- c(
  "portion_standardized_units_g",
  "sum_of_portionstandardized_units_g",
  "portion_standardized_units_in_kg",
  "portionstandardizedunits_g",
  "portion_standardized_units_grams",
  "sum_of_portion_standardized_units_g",
  "portion_standardized_units_in_grams",
  "portion_standardized_units",
  "mass_standardized_in_g",
  "portion_standardizedunits_g"
)


standardize_fiwo_column <- function(df) {
  current_names <- names(df)
  match_col <- intersect(fiwo_possible_names, current_names)
  if (length(match_col) > 0) {
    df <- df %>%
      rename(mass_standardized_in_g = all_of(match_col[1]))
  } else {
    warning("⚠️ Could not find a standardized mass column in FIWO_GWP table.")
  }
  return(df)
}

#FIWO_GWP       <- read_feather(paste0("data_processed/", "jan24", "_FIWO_GWP.feather"))       %>% clean_names()
#colnames(FIWO_GWP)
# ----------------------------------------------------------------
#  FUNCTION : process_month()
# ----------------------------------------------------------------
process_month <- function(prefix, month_label, wb) {
  
  # === Load data ===
  FIW_GWP        <- read_feather(paste0("data_processed/", prefix, "_FIW_GWP.feather"))        %>% clean_names()
  #FIWO_GWP       <- read_feather(paste0("data_processed/", prefix, "_FIWO_GWP.feather"))       %>% clean_names()
  FIWO_GWP <- read_feather(paste0("data_processed/", prefix, "_FIWO_GWP.feather")) %>%
    clean_names() %>%
    standardize_fiwo_column()
  animal         <- read_feather(paste0("data_processed/", prefix, "_animal.feather"))          %>% clean_names()
  animal_second  <- read_feather(paste0("data_processed/", prefix, "_animal_second.feather"))   %>% clean_names()
  
  # === Extract totals ===
  animal_last <- tail(animal, 1) %>% select(sum_portion_unit_standard_kg, gwp_x_kg)
  FIW_GWP_last <- tail(FIW_GWP, 1) %>% select(sum_portion_unit_standard_kg, gwp_x_kg)
  FIWO_GWP_last <- tail(FIWO_GWP, 1) %>%
    select(mass_standardized_in_g) %>%
    mutate(mass_standardized_in_kg = as.numeric(mass_standardized_in_g) / 1000)
  
  # === Base scenario calculations ===
  animal_kg  <- as.numeric(animal_last$sum_portion_unit_standard_kg)
  animal_gwp <- as.numeric(animal_last$gwp_x_kg)
  total_estimated_kg  <- as.numeric(FIW_GWP_last$sum_portion_unit_standard_kg)
  total_estimated_gwp <- as.numeric(FIW_GWP_last$gwp_x_kg)
  unestimated_kg <- as.numeric(FIWO_GWP_last$mass_standardized_in_kg)
  everything_else_kg  <- total_estimated_kg - animal_kg
  everything_else_gwp <- total_estimated_gwp - animal_gwp
  total_kg  <- animal_kg + everything_else_kg + unestimated_kg
  total_gwp <- animal_gwp + everything_else_gwp
  
  summary1 <- tibble(
    Category = c("Animal Products on Menu",
                 "Everything else on menu w/ estimates for GWP",
                 "Unestimated Food Mass",
                 "Total"),
    `Total (kg)` = c(animal_kg, everything_else_kg, unestimated_kg, total_kg),
    `GWP (kgCO2e)` = c(animal_gwp, everything_else_gwp, NA, total_gwp)
  )
  
  # --- Summary 2 ---
  total_wo_animal_mass <- summary1$`Total (kg)`[2] + summary1$`Total (kg)`[3]
  pct_unestimated <- summary1$`Total (kg)`[3] / summary1$`Total (kg)`[2]
  estimated_gwp_unestimated <- pct_unestimated * summary1$`GWP (kgCO2e)`[2]
  summary1$`GWP (kgCO2e)`[3] <- estimated_gwp_unestimated
  total_gwp_wo_animal <- estimated_gwp_unestimated + summary1$`GWP (kgCO2e)`[2]
  total_gwp_all_food  <- total_gwp_wo_animal + summary1$`GWP (kgCO2e)`[1]
  summary1$`GWP (kgCO2e)`[4] <- total_gwp_all_food
  total_kg  <- summary1$`Total (kg)`[4]
  total_gwp <- summary1$`GWP (kgCO2e)`[4]
  summary1 <- summary1 %>%
    mutate(`% of Total Mass` = round(`Total (kg)` / total_kg * 100, 1),
           `% of Total GWP`  = round(`GWP (kgCO2e)` / total_gwp * 100, 1))
  
  summary2 <- tibble(
    Metric = c("Total Mass of Food (W/o Animal Products)",
               "% of food mass w/o GWP estimates",
               "Estimate of Kg×GWP for remaining food mass",
               "Total Kg×GWP without Animal Products",
               "Total Kg×GWP (all food ordered)"),
    Value = c(total_wo_animal_mass,
              pct_unestimated,
              estimated_gwp_unestimated,
              total_gwp_wo_animal,
              total_gwp_all_food)
  )
  
  # === Animal-second scenario ===
  animal_second_last <- tail(animal_second, 1) %>% select(sum_portion_unit_standard_kg, gwp_x_kg)
  animal_diff_gwp <- as.numeric(animal_second_last$gwp_x_kg) - as.numeric(animal_last$gwp_x_kg)
  new_total_estimated_kg  <- total_estimated_kg                 # mass unchanged
  new_total_estimated_gwp <- total_estimated_gwp + animal_diff_gwp
  
  animal2_kg   <- as.numeric(animal_second_last$sum_portion_unit_standard_kg)
  animal2_gwp  <- as.numeric(animal_second_last$gwp_x_kg)
  everything_else_kg2  <- new_total_estimated_kg - animal2_kg
  everything_else_gwp2 <- new_total_estimated_gwp - animal2_gwp
  total_kg2  <- animal2_kg + everything_else_kg2 + unestimated_kg
  total_gwp2 <- animal2_gwp + everything_else_gwp2
  
  summary1_animal2 <- tibble(
    Category = c("Animal Products on Menu (Second Scenario)",
                 "Everything else on menu w/ estimates for GWP",
                 "Unestimated Food Mass",
                 "Total"),
    `Total (kg)` = c(animal2_kg, everything_else_kg2, unestimated_kg, total_kg2),
    `GWP (kgCO2e)` = c(animal2_gwp, everything_else_gwp2, NA, total_gwp2)
  )
  
  total_wo_animal2_mass <- summary1_animal2$`Total (kg)`[2] + summary1_animal2$`Total (kg)`[3]
  pct_unestimated2 <- summary1_animal2$`Total (kg)`[3] / summary1_animal2$`Total (kg)`[2]
  estimated_gwp_unestimated2 <- pct_unestimated2 * summary1_animal2$`GWP (kgCO2e)`[2]
  summary1_animal2$`GWP (kgCO2e)`[3] <- estimated_gwp_unestimated2
  total_gwp_wo_animal2 <- estimated_gwp_unestimated2 + summary1_animal2$`GWP (kgCO2e)`[2]
  total_gwp_all_food2  <- total_gwp_wo_animal2 + summary1_animal2$`GWP (kgCO2e)`[1]
  summary1_animal2$`GWP (kgCO2e)`[4] <- total_gwp_all_food2
  total_gwp2 <- summary1_animal2$`GWP (kgCO2e)`[4]
  summary1_animal2 <- summary1_animal2 %>%
    mutate(`% of Total Mass` = round(`Total (kg)` / total_kg2 * 100, 1),
           `% of Total GWP`  = round(`GWP (kgCO2e)` / total_gwp2 * 100, 1))
  
  summary2_animal2 <- tibble(
    Metric = c("Total Mass of Food (W/o Animal Products)",
               "% of food mass w/o GWP estimates",
               "Estimate of Kg×GWP for remaining food mass",
               "Total Kg×GWP without Animal Products",
               "Total Kg×GWP (all food ordered)"),
    Value = c(total_wo_animal2_mass,
              pct_unestimated2,
              estimated_gwp_unestimated2,
              total_gwp_wo_animal2,
              total_gwp_all_food2)
  )
  
  # === Write Excel ===
  #wb <- createWorkbook()
  addWorksheet(wb, month_label)
  
  write_table_with_gap <- function(data, sheet, startRow, startCol, title) {
    writeData(wb, sheet, title, startRow = startRow, startCol = startCol)
    writeData(wb, sheet, data, startRow = startRow + 1, startCol = startCol)
    n_rows <- nrow(data) + 3  # +1 title, +1 blank row
    return(startRow + n_rows)
  }
  
  r <- 1; c <- 1
  r <- write_table_with_gap(FIW_GWP, month_label, r, c, paste0(prefix, "_FIW_GWP"))
  r <- write_table_with_gap(FIWO_GWP, month_label, r, c, paste0(prefix, "_FIWO_GWP"))
  r <- 1; c <- ncol(FIW_GWP) + 3
  r <- write_table_with_gap(animal, month_label, r, c, paste0(prefix, "_animal"))
  r <- write_table_with_gap(summary1, month_label, r, c, paste0(prefix, "_summary1"))
  r <- write_table_with_gap(summary2, month_label, r, c, paste0(prefix, "_summary2"))
  r <- 1; c <- ncol(FIW_GWP) + ncol(animal) + 5
  r <- write_table_with_gap(animal_second, month_label, r, c, paste0(prefix, "_animal_second"))
  r <- write_table_with_gap(summary1_animal2, month_label, r, c, paste0(prefix, "_summary1_animal2"))
  r <- write_table_with_gap(summary2_animal2, month_label, r, c, paste0(prefix, "_summary2_animal2"))
  
  #saveWorkbook(wb, paste0("data_processed/", prefix, "_AllTables.xlsx"), overwrite = TRUE)
  cat("✅ Finished:", month_label, "\n")
}

# ----------------------------------------------------------------
#  LOOP for several months (test with 3)
# ----------------------------------------------------------------
#months <- c("dec23", "jan24", "feb24")
#labels <- c("Dec2023", "Jan2024", "Feb2024")

#for (i in seq_along(months)) {
#  process_month(months[i], labels[i])
#}


# ----------------------------------------------------------------
#  MASTER WORKBOOK for all months
# ----------------------------------------------------------------
wb_all <- createWorkbook()

months <- c("dec23", "jan24", "feb24")
labels <- c("Dec2023", "Jan2024", "Feb2024")

for (i in seq_along(months)) {
  process_month(months[i], labels[i], wb_all)
}

# Save once at the end
saveWorkbook(wb_all, "data_processed/HospitalFood_AllMonths.xlsx", overwrite = TRUE)

cat("🎉 All months saved to one Excel file successfully!\n")


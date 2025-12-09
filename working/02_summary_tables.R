library(dplyr)
library(arrow)
library(readr)
library(janitor)
library(openxlsx)

setwd("~/Sharing - Dalhousie/OneDrive - Dalhousie University/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working")


# Load tables
dec23_FIW_GWP        <- read_feather("data_processed/dec23_FIW_GWP.feather")
dec23_FIWO_GWP       <- read_feather("data_processed/dec23_FIWO_GWP.feather")
dec23_animal         <- read_feather("data_processed/dec23_animal.feather")
dec23_animal_second  <- read_feather("data_processed/dec23_animal_second.feather")

# ---- Clean columns if needed ----
dec23_FIW_GWP  <- dec23_FIW_GWP  %>% clean_names()
dec23_FIWO_GWP <- dec23_FIWO_GWP %>% clean_names()
dec23_animal   <- dec23_animal   %>% clean_names()
dec23_animal_second <- dec23_animal_second %>% clean_names()

# --- Convert numeric columns early ---
num_cols_FIW <- c("sum_portion_unit_standard_kg", "gwp_x_kg")
num_cols_animal <- c("sum_portion_unit_standard_kg", "gwp_x_kg")
num_cols_FIWO <- c("mass_standardized_in_g")

# --- Extract total (last row) values for each source ---
animal_last <- dec23_animal %>%
  tail(1) %>%
  select(sum_portion_unit_standard_kg, gwp_x_kg)

FIW_GWP_last <- dec23_FIW_GWP %>%
  tail(1) %>%
  select(sum_portion_unit_standard_kg, gwp_x_kg)

FIWO_GWP_last <- dec23_FIWO_GWP %>%
  tail(1) %>%
  select(mass_standardized_in_g) %>%
  mutate(
    mass_standardized_in_g = as.numeric(mass_standardized_in_g),
    mass_standardized_in_kg = mass_standardized_in_g / 1000
  )

# --- Compute each component ---
animal_kg <- as.numeric(animal_last$sum_portion_unit_standard_kg)
animal_gwp <- as.numeric(animal_last$gwp_x_kg)

total_estimated_kg <- as.numeric(FIW_GWP_last$sum_portion_unit_standard_kg)
total_estimated_gwp <- as.numeric(FIW_GWP_last$gwp_x_kg)

unestimated_kg <- as.numeric(FIWO_GWP_last$mass_standardized_in_kg)

everything_else_kg <- total_estimated_kg - animal_kg
everything_else_gwp <- total_estimated_gwp - animal_gwp

total_kg <- animal_kg + everything_else_kg + unestimated_kg
total_gwp <- animal_gwp + everything_else_gwp # (unestimated has no GWP yet)

# --- Build summary table ---
dec23_summary1 <- tibble::tibble(
  Category = c(
    "Animal Products on Menu",
    "Everything else on menu w/ estimates for GWP",
    "Unestimated Food Mass",
    "Total"
  ),
  `Total (kg)` = c(animal_kg, everything_else_kg, unestimated_kg, total_kg),
  `GWP (kgCO2e)` = c(animal_gwp, everything_else_gwp, NA, total_gwp)
) %>%
  mutate(
    `% of Total Mass` = round(`Total (kg)` / total_kg * 100, 1),
    `% of Total GWP` = round(`GWP (kgCO2e)` / total_gwp * 100, 1)
  )

# --- Save first summary ---
write_feather(dec23_summary1, "data_processed/dec23_summary1.feather")

# ===============================================================
# === SECOND SUMMARY ============================================
# ===============================================================

# 1️⃣ Total Mass of Food (W/o Animal Products)
total_wo_animal_mass <- dec23_summary1$`Total (kg)`[2] + dec23_summary1$`Total (kg)`[3]

# 2️⃣ % of food mass w/o GWP estimates
pct_unestimated <- dec23_summary1$`Total (kg)`[3] / dec23_summary1$`Total (kg)`[2]

# 3️⃣ Estimate of Kg×GWP for remaining (unestimated) food mass
estimated_gwp_unestimated <- pct_unestimated * dec23_summary1$`GWP (kgCO2e)`[2]

# Update first summary: fill in missing GWP for "Unestimated Food Mass"
dec23_summary1$`GWP (kgCO2e)`[3] <- estimated_gwp_unestimated

# 4️⃣ Total Kg×GWP without Animal Products
total_gwp_wo_animal <- estimated_gwp_unestimated + dec23_summary1$`GWP (kgCO2e)`[2]

# 5️⃣ Total Kg×GWP (all food ordered)
total_gwp_all_food <- total_gwp_wo_animal + dec23_summary1$`GWP (kgCO2e)`[1]

# --- Build SECOND SUMMARY ---
dec23_summary2 <- tibble::tibble(
  Metric = c(
    "Total Mass of Food (W/o Animal Products)",
    "% of food mass w/o GWP estimates",
    "Estimate of Kg×GWP for remaining food mass",
    "Total Kg×GWP without Animal Products",
    "Total Kg×GWP (all food ordered)"
  ),
  Value = c(
    total_wo_animal_mass,
    pct_unestimated,
    estimated_gwp_unestimated,
    total_gwp_wo_animal,
    total_gwp_all_food
  )
)

# ===============================================================
# === UPDATE FIRST SUMMARY (append after creating summary2) =====
# ===============================================================

# Fill in the estimated GWP for “Unestimated Food Mass”
dec23_summary1$`GWP (kgCO2e)`[3] <- estimated_gwp_unestimated

# Update total GWP (the last row)
dec23_summary1$`GWP (kgCO2e)`[4] <- total_gwp_all_food

# Recalculate percentages
total_kg  <- dec23_summary1$`Total (kg)`[4]
total_gwp <- dec23_summary1$`GWP (kgCO2e)`[4]

dec23_summary1 <- dec23_summary1 %>%
  mutate(
    `% of Total Mass` = round(`Total (kg)` / total_kg * 100, 1),
    `% of Total GWP`  = round(`GWP (kgCO2e)` / total_gwp * 100, 1)
  )

write_feather(dec23_summary1, "data_processed/dec23_summary1.feather")
write_feather(dec23_summary2, "data_processed/dec23_summary2.feather")

# --- Optional check ---
print(dec23_summary1)
print(dec23_summary2)


dec23_summary1  <- read_feather("data_processed/dec23_summary1.feather")
dec23_summary2  <- read_feather("data_processed/dec23_summary2.feather")



# ===============================================================
# === ANIMAL SECOND SCENARIO ===================================
# ===============================================================

# --- Extract last row for animal_second ---
animal_second_last <- dec23_animal_second %>%
  tail(1) %>%
  select(sum_portion_unit_standard_kg, gwp_x_kg)

# --- Calculate difference between scenarios (second - first) ---
animal_diff_kg  <- as.numeric(animal_second_last$sum_portion_unit_standard_kg) - as.numeric(animal_last$sum_portion_unit_standard_kg)
animal_diff_gwp <- as.numeric(animal_second_last$gwp_x_kg) - as.numeric(animal_last$gwp_x_kg)
# Expected: both should be <= 0 (lower GWP scenario)

# --- Adjust FIW_GWP total values to reflect new animal scenario ---
new_total_estimated_kg  <- total_estimated_kg + animal_diff_kg
new_total_estimated_gwp <- total_estimated_gwp + animal_diff_gwp

# --- Recalculate components for second scenario ---
animal2_kg   <- as.numeric(animal_second_last$sum_portion_unit_standard_kg)
animal2_gwp  <- as.numeric(animal_second_last$gwp_x_kg)
everything_else_kg2  <- new_total_estimated_kg - animal2_kg
everything_else_gwp2 <- new_total_estimated_gwp - animal2_gwp
total_kg2  <- animal2_kg + everything_else_kg2 + unestimated_kg
total_gwp2 <- animal2_gwp + everything_else_gwp2  # no GWP for unestimated yet

# --- Build FIRST SUMMARY (Animal Second Scenario) ---
dec23_summary1_animal2 <- tibble::tibble(
  Category = c(
    "Animal Products on Menu (Second Scenario)",
    "Everything else on menu w/ estimates for GWP",
    "Unestimated Food Mass",
    "Total"
  ),
  `Total (kg)` = c(animal2_kg, everything_else_kg2, unestimated_kg, total_kg2),
  `GWP (kgCO2e)` = c(animal2_gwp, everything_else_gwp2, NA, total_gwp2)
) %>%
  mutate(
    `% of Total Mass` = round(`Total (kg)` / total_kg2 * 100, 1),
    `% of Total GWP`  = round(`GWP (kgCO2e)` / total_gwp2 * 100, 1)
  )

# ===============================================================
# === SECOND SUMMARY (Animal Second Scenario) ===================
# ===============================================================

# 1️⃣ Total Mass of Food (W/o Animal Products)
total_wo_animal2_mass <- dec23_summary1_animal2$`Total (kg)`[2] + dec23_summary1_animal2$`Total (kg)`[3]

# 2️⃣ % of food mass w/o GWP estimates
pct_unestimated2 <- dec23_summary1_animal2$`Total (kg)`[3] / dec23_summary1_animal2$`Total (kg)`[2]

# 3️⃣ Estimate of Kg×GWP for remaining (unestimated) food mass
estimated_gwp_unestimated2 <- pct_unestimated2 * dec23_summary1_animal2$`GWP (kgCO2e)`[2]

# Update first summary: fill in missing GWP
dec23_summary1_animal2$`GWP (kgCO2e)`[3] <- estimated_gwp_unestimated2

# 4️⃣ Total Kg×GWP without Animal Products
total_gwp_wo_animal2 <- estimated_gwp_unestimated2 + dec23_summary1_animal2$`GWP (kgCO2e)`[2]

# 5️⃣ Total Kg×GWP (all food ordered)
total_gwp_all_food2 <- total_gwp_wo_animal2 + dec23_summary1_animal2$`GWP (kgCO2e)`[1]

# --- Build SECOND SUMMARY table (Animal Second) ---
dec23_summary2_animal2 <- tibble::tibble(
  Metric = c(
    "Total Mass of Food (W/o Animal Products)",
    "% of food mass w/o GWP estimates",
    "Estimate of Kg×GWP for remaining food mass",
    "Total Kg×GWP without Animal Products",
    "Total Kg×GWP (all food ordered)"
  ),
  Value = c(
    total_wo_animal2_mass,
    pct_unestimated2,
    estimated_gwp_unestimated2,
    total_gwp_wo_animal2,
    total_gwp_all_food2
  )
)

# --- Update total GWP in first summary and recalc percentages ---
dec23_summary1_animal2$`GWP (kgCO2e)`[4] <- total_gwp_all_food2

total_kg2  <- dec23_summary1_animal2$`Total (kg)`[4]
total_gwp2 <- dec23_summary1_animal2$`GWP (kgCO2e)`[4]

dec23_summary1_animal2 <- dec23_summary1_animal2 %>%
  mutate(
    `% of Total Mass` = round(`Total (kg)` / total_kg2 * 100, 1),
    `% of Total GWP`  = round(`GWP (kgCO2e)` / total_gwp2 * 100, 1)
  )

# --- Save both summaries for animal_second scenario ---
write_feather(dec23_summary1_animal2, "data_processed/dec23_summary1_animal2.feather")
write_feather(dec23_summary2_animal2, "data_processed/dec23_summary2_animal2.feather")

# --- Optional check ---
print(dec23_summary1_animal2)
print(dec23_summary2_animal2)


# ===============================================================
# === Save in Excel file together ===================
# ===============================================================


# Create a new workbook
wb <- createWorkbook()

# Add one worksheet
addWorksheet(wb, "Dec2023")

# --- Helper function to write table with offset ---
write_table_with_gap <- function(data, sheet, startRow, startCol, title) {
  writeData(wb, sheet, title, startRow = startRow, startCol = startCol)
  writeData(wb, sheet, data, startRow = startRow + 1, startCol = startCol)
  n_rows <- nrow(data) + 4  # +1 for title, +2 empty rows
  return(startRow + n_rows)
}

# === 1️⃣ Left section ===
r <- 1
c <- 1
r <- write_table_with_gap(dec23_FIW_GWP, "Dec2023", r, c, "dec23_FIW_GWP")
r <- write_table_with_gap(dec23_FIWO_GWP, "Dec2023", r, c, "dec23_FIWO_GWP")

# === 2️⃣ Middle section (start 2 columns after previous group) ===
r <- 1
c <- ncol(dec23_FIW_GWP) + 3
r <- write_table_with_gap(dec23_animal, "Dec2023", r, c, "dec23_animal")
r <- write_table_with_gap(dec23_summary1, "Dec2023", r, c, "dec23_summary1")
r <- write_table_with_gap(dec23_summary2, "Dec2023", r, c, "dec23_summary2")

# === 3️⃣ Right section (2 columns after previous group) ===
r <- 1
c <- ncol(dec23_FIW_GWP) + ncol(dec23_animal) + 5
r <- write_table_with_gap(dec23_animal_second, "Dec2023", r, c, "dec23_animal_second")
r <- write_table_with_gap(dec23_summary1_animal2, "Dec2023", r, c, "dec23_summary1_animal2")
r <- write_table_with_gap(dec23_summary2_animal2, "Dec2023", r, c, "dec23_summary2_animal2")

# Save workbook
saveWorkbook(wb, "data_processed/AllTables_R_calculation.xlsx", overwrite = TRUE)





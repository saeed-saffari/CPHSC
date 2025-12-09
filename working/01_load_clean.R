library(readxl)
library(dplyr)
library(arrow)

setwd("~/Sharing - Dalhousie/OneDrive - Dalhousie University/0- Dalhousie University - Saeed/CPHSC/basic docs/Food is Health/Hospital Food/working")

file_path <- "data_raw/HospitalFoodEmissions_Dec2023-Nov2024.xlsx"


# ============================================================
# === DECEMBER 2023 ==========================================
# ============================================================

dec23 <- read_excel(file_path, sheet = 1, col_names = FALSE)

# --- Table 1: FIW_GWP ---
dec23_FIW_GWP <- dec23[2:80, 1:7]   # rows 5–20, columns A–E
colnames(dec23_FIW_GWP) <- dec23[1, 1:7] # optional: use header row above it
write_feather(dec23_FIW_GWP, "data_processed/dec23_FIW_GWP.feather")

# --- Table 2: FIWO_GWP ---
dec23_FIWO_GWP <- dec23[83:126, 1:3]
colnames(dec23_FIWO_GWP) <- dec23[82, 1:3]
write_feather(dec23_FIWO_GWP, "data_processed/dec23_FIWO_GWP.feather")

# --- Table 3: animal ---
dec23_animal <- dec23[2:17, 9:15]
colnames(dec23_animal) <- dec23[1, 9:15]
write_feather(dec23_animal, "data_processed/dec23_animal.feather")

# --- Table 4: animal_second ---
dec23_animal_second <- dec23[43:58, 9:15]
colnames(dec23_animal_second) <- dec23[42, 9:15]
write_feather(dec23_animal_second, "data_processed/dec23_animal_second.feather")


# ============================================================
# === JANUARY 2024 ===========================================
# ============================================================

jan24 <- read_excel(file_path, sheet = "Jan '24 GWP", col_names = FALSE)

jan24_FIW_GWP <- jan24[2:79, 1:7]
colnames(jan24_FIW_GWP) <- jan24[1, 1:7]
write_feather(jan24_FIW_GWP, "data_processed/jan24_FIW_GWP.feather")

jan24_FIWO_GWP <- jan24[82:134, 1:4]
colnames(jan24_FIWO_GWP) <- jan24[81, 1:4]
write_feather(jan24_FIWO_GWP, "data_processed/jan24_FIWO_GWP.feather")

jan24_animal <- jan24[2:17, 11:17]
colnames(jan24_animal) <- jan24[1, 11:17]
write_feather(jan24_animal, "data_processed/jan24_animal.feather")

jan24_animal_second <- jan24[45:60, 11:17]
colnames(jan24_animal_second) <- jan24[44, 11:17]
write_feather(jan24_animal_second, "data_processed/jan24_animal_second.feather")


# ============================================================
# === February 2024 ===========================================
# ============================================================

feb24 <- read_excel(file_path, sheet = "Feb '24 GWP Data", col_names = FALSE)

feb24_FIW_GWP <- feb24[2:78, 1:7]
colnames(feb24_FIW_GWP) <- feb24[1, 1:7]
write_feather(feb24_FIW_GWP, "data_processed/feb24_FIW_GWP.feather")

feb24_FIWO_GWP <- feb24[83:142, 1:3]
colnames(feb24_FIWO_GWP) <- feb24[82, 1:3]
write_feather(feb24_FIWO_GWP, "data_processed/feb24_FIWO_GWP.feather")

feb24_animal <- feb24[2:17, 11:17]
colnames(feb24_animal) <- feb24[1, 11:17]
write_feather(feb24_animal, "data_processed/feb24_animal.feather")

feb24_animal_second <- feb24[40:55, 11:16]
colnames(feb24_animal_second) <- feb24[39, 11:16]
write_feather(feb24_animal_second, "data_processed/feb24_animal_second.feather")







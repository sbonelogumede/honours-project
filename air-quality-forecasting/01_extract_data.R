# ==============================================================================
# Data Extraction Script
# Purpose: Read raw air quality data and combine into unified datasets
# ==============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(readxl)

# Configuration ----------------------------------------------------------------
raw_data_dir <- "raw_data"
output_dir <- "data"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# File specifications ----------------------------------------------------------
file_names <- c(
  "2019_NO2_CCT.xls",
  "2019_PM10_CCT.xls",
  "2019_SO2_CCT.xls",
  "Wind_direction_and_speed_2019.xlsx"
)

# Cells of interest within each dataset
cell_ranges <- c("E6:E8742", "E6:E8742", "H6:H8742", "O6:O8742")

# Extract and combine data -----------------------------------------------------
message("Extracting data from Excel files...")

# Initialize dataset with date-time object
F_mat <- read_excel(
  path = file.path(raw_data_dir, file_names[1]),
  range = "A6:A8742",
  col_names = FALSE
) |>
  pull(var = 1) |>
  as.POSIXct(format = "%d/%m/%Y %H:%M")

S_mat <- F_mat

# Loop through each file and extract relevant columns
for (j in 1:4) {
  message(sprintf("  Processing file %d of 4: %s", j, file_names[j]))
  
  # Read the column of interest
  tmp <- read_excel(
    path = file.path(raw_data_dir, file_names[j]),
    range = cell_ranges[j],
    col_names = FALSE,
    col_types = "text"
  )
  
  # Handle special values and convert to numeric
  tmp <- tmp %>%
    mutate(across(everything(), ~ case_when(
      .x %in% c("Down", "InVld", "NoData", "<Samp") ~ NA_real_,
      .x == "Zero" ~ 0,
      TRUE ~ as.numeric(.x)
    )))
  
  # Scale the data for comparison between features
  scaled_tmp <- scale(x = tmp, center = TRUE, scale = TRUE) |>
    as_tibble()
  
  # Add columns to datasets
  F_mat <- cbind(F_mat, tmp)
  S_mat <- cbind(S_mat, scaled_tmp)
}

# Add column names -------------------------------------------------------------
colnames(F_mat) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
colnames(S_mat) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

# Save datasets ----------------------------------------------------------------
output_file <- file.path(output_dir, "extracted_data.RData")
save(F_mat, S_mat, file = output_file)

message(sprintf("Data extraction complete. Saved to: %s", output_file))
message(sprintf("  Observations: %d", nrow(F_mat)))
message(sprintf("  Variables: %s", paste(colnames(F_mat), collapse = ", ")))

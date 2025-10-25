# ==============================================================================
# Master Analysis Script
# Purpose: Run the complete air quality forecasting analysis pipeline
# ==============================================================================

# This script runs all analysis steps in sequence.
# You can also run individual scripts as needed.

message("========================================")
message("Air Quality Forecasting Analysis Pipeline")
message("========================================\n")

# Step 1: Extract raw data -----------------------------------------------------
message("STEP 1: Extracting data from raw files...")
source("01_extract_data.R")
message("Step 1 complete\n")

# Step 2: Preprocess and explore -----------------------------------------------
message("STEP 2: Preprocessing and exploratory analysis...")
source("02_preprocess_explore.R")
message("Step 2 complete\n")

# Step 3: Transform data for modeling ------------------------------------------
message("STEP 3: Transforming data for time series modeling...")
source("03_transform_data.R")
message("Step 3 complete\n")

# Step 4: Simple forecasting models --------------------------------------------
message("STEP 4: Evaluating simple forecasting models...")
source("04_simple_forecasting.R")
message("Step 4 complete\n")

# Step 5: SARIMA models --------------------------------------------------------
message("STEP 5: Evaluating SARIMA models...")
source("05_sarima_models.R")
message("Step 5 complete\n")

# Summary ----------------------------------------------------------------------
message("========================================")
message("Analysis pipeline completed successfully!")
message("========================================\n")

message("Generated outputs:")
message("Processed data: data/")
message("Figures: output/figures/")
message("Results: output/results/")
message("\nTo view results:")
message("Simple forecasting: output/results/simple_forecasting_results.RData")
message("SARIMA models: output/results/sarima_results.RData")

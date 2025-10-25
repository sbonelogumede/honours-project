# ==============================================================================
# Data Transformation for Time Series Modeling
# Purpose: Prepare train/test splits and format data for forecasting
# ==============================================================================

# Load required packages -------------------------------------------------------
library(forecast)
library(ggplot2)
library(parallel)

# Source utility functions -----------------------------------------------------
source("R/evaluation_metrics.R")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
output_dir <- "output"
fig_dir <- file.path(output_dir, "figures")

# Load preprocessed data -------------------------------------------------------
load(file.path(data_dir, "preprocessed_data.RData"))

message("Transforming data for time series modeling...")

# Find total number of observations
n <- nrow(S_mat)

# Add intercept and time components --------------------------------------------
YX_mat <- cbind(
  NO2 = S_mat[, "NO2"],
  Intercept = 1,
  Time = 1:n,
  S_mat[, c("PM10", "SO2", "Speed")]
)

# Remove incomplete observations -----------------------------------------------
CYX_mat <- na.omit(YX_mat)

message(sprintf("Complete cases: %d (%.1f%% of total)",
                nrow(CYX_mat),
                100 * nrow(CYX_mat) / nrow(YX_mat)))

# Extract response variable and covariates -------------------------------------
Y_vec <- ts(CYX_mat[, "NO2"], start = 1, frequency = 24)
X_mat <- ts(CYX_mat[, -1], start = 1, frequency = 24)

# Create complete data frame preserving DateTime -------------------------------
YX_mat2 <- cbind(
  DateTime = S_mat[as.numeric(rownames(CYX_mat)), "DateTime"],
  NO2 = as.data.frame(Y_vec),
  as.data.frame(X_mat[, -c(1, 2)])  # Exclude Intercept and Time
)

colnames(YX_mat2) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

# Summary statistics for complete data ----------------------------------------
S_complete <- summary_statistics(YX_mat2[, -1])
print("Complete data summary:")
print(round(t(S_complete), digits = 3))

# Create visualizations for complete data -------------------------------------
message("Creating plots for complete data...")

scatter_plot(X = YX_mat2, name = "complete", output_dir = fig_dir)
pairs_plot(X = YX_mat2, name = "complete", output_dir = fig_dir)

# Train/test split configuration ----------------------------------------------
# Training period: 7 days
d <- c(7, 24)

# Forecast horizon: 1 day
h <- c(1, 0)

# Create train set
X1 <- window(x = X_mat, start = c(1, 1), end = d)
Y1 <- window(x = Y_vec, start = c(1, 1), end = d)
n1 <- nrow(X1)
p <- ncol(X1)

# Create test set
X2 <- window(x = X_mat, start = end(X1) + c(1, -23), end = end(X1) + h)
Y2 <- window(x = Y_vec, start = end(Y1) + c(1, -23), end = end(Y1) + h)
n2 <- nrow(X2)

message(sprintf("Train set: %d observations", n1))
message(sprintf("Test set: %d observations", n2))

# Visualize train and test sets ------------------------------------------------
p1 <- autoplot(Y1) +
  autolayer(Y1, series = "Observed") +
  labs(y = expression(NO[2]), color = "Series") +
  scale_color_manual(values = c("Observed" = "lightskyblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- autoplot(Y2) +
  autolayer(Y2, series = "Observed") +
  labs(y = expression(NO[2]), color = "Series") +
  scale_color_manual(values = c("Observed" = "lightskyblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(fig_dir, "train_set.pdf"),
  plot = p1,
  width = 8,
  height = 4,
  dpi = 600
)

ggsave(
  filename = file.path(fig_dir, "test_set.pdf"),
  plot = p2,
  width = 8,
  height = 4,
  dpi = 600
)

# Detect available cores for parallel processing -------------------------------
cores <- detectCores()
message(sprintf("Available cores for parallel processing: %d", cores))

# Save transformed data --------------------------------------------------------
save(
  X_mat, Y_vec, X1, Y1, n1, X2, Y2, n2, p, h, p1, p2, cores,
  rmse, mae, smape,
  file = file.path(data_dir, "transformed_data.RData")
)

message("Data transformation complete!")

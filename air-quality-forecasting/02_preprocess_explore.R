# ==============================================================================
# Data Preprocessing and Exploratory Analysis
# Purpose: Clean data, generate visualizations, and compute summary statistics
# ==============================================================================

# Load required packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(lubridate)

# Source utility functions -----------------------------------------------------
source("R/summary_functions.R")
source("R/plotting_functions.R")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
output_dir <- "output"
fig_dir <- file.path(output_dir, "figures")

# Create directories if they don't exist
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# Load extracted data ----------------------------------------------------------
load(file.path(data_dir, "extracted_data.RData"))

message("Data loaded successfully")
message(sprintf("  F_mat dimensions: %d x %d", nrow(F_mat), ncol(F_mat)))

# Add weekday/weekend indicator ------------------------------------------------
F_mat$Weekday <- ifelse(
  wday(F_mat$DateTime, week_start = 1) %in% 6:7,
  "Weekend",
  "Weekday"
)

message("Added weekday/weekend indicator")

# Exploratory visualizations ---------------------------------------------------
message("Creating exploratory plots...")

# Weekday vs Weekend comparison for all variables
plot_data <- F_mat %>%
  select(Weekday, NO2, PM10, SO2, Speed) %>%
  pivot_longer(
    cols = c(NO2, PM10, SO2, Speed),
    names_to = "Variable",
    values_to = "Value"
  )

p_weekday_comparison <- ggplot(
  plot_data,
  aes(x = Weekday, y = Value, fill = Weekday)
) +
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Weekday" = "#9999CC", "Weekend" = "#E7D4A8")) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  ) +
  labs(x = "", y = "Value", fill = "")

ggsave(
  filename = file.path(fig_dir, "weekday_comparison.pdf"),
  plot = p_weekday_comparison,
  width = 10,
  height = 4,
  dpi = 600
)

# Summary statistics -----------------------------------------------------------
message("Computing summary statistics...")

F_summary <- summary_statistics(F_mat[, c("NO2", "PM10", "SO2", "Speed")])
S_summary <- summary_statistics(S_mat[, c("NO2", "PM10", "SO2", "Speed")])

print("Original data summary:")
print(round(t(F_summary), digits = 3))

print("Scaled data summary:")
print(round(t(S_summary), digits = 3))

# Time series plots ------------------------------------------------------------
message("Creating time series plots...")

scatter_plot(X = F_mat, name = "extracted", output_dir = fig_dir)
scatter_plot(X = S_mat, name = "scaled", output_dir = fig_dir)

# Pairs plots ------------------------------------------------------------------
message("Creating pairs plots...")

pairs_plot(X = F_mat, name = "extracted", output_dir = fig_dir)
pairs_plot(X = S_mat, name = "scaled", output_dir = fig_dir)

# Save processed data ----------------------------------------------------------
var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
x_name <- c(
  "DateTime",
  expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"),
  expression(PM[10] ~ "(" * mu * "g/m"^3 * ")"),
  expression(SO[2] ~ "(" * mu * "g/m"^3 * ")"),
  expression("Wind Speed" ~ "(m/s)")
)

save(
  F_mat, S_mat, var_name, x_name,
  summary_statistics, scatter_plot, pairs_plot,
  file = file.path(data_dir, "preprocessed_data.RData")
)

message("Preprocessing complete!")

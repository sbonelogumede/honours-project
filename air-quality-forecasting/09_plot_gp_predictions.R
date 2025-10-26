# ==============================================================================
# Plot GP Model Predictions with 95% Prediction Intervals
# Purpose: Visualize predictions from GP models across all CV folds
# ==============================================================================

# Load required packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
figures_dir <- "output/figures"

# Ensure output directories exist
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

cat("Loading transformed data...\n")
load(file.path(data_dir, "transformed_data.RData"))

cat("Loading GP model results...\n")
load(file.path(results_dir, "GP_MLR_SE_IR.RData"))
load(file.path(results_dir, "GP_MLR_PER_IR.RData"))
load(file.path(results_dir, "GP_MLR_SExPER_IR.RData"))

n_cv <- length(SE_draws)

# -----------------------------------------------------------------------------
# Helper Function: Extract Predictions and Intervals
# -----------------------------------------------------------------------------

extract_predictions <- function(draws, fold_idx, Y_vec, X_mat) {
  # Get fold-specific test window
  Y2_fold <- window(
    x = Y_vec,
    start = start(Y2) + c(fold_idx - 1, 0),
    end = end(Y2) + c(fold_idx - 1, 0)
  )
  
  # Extract y2 predictions from draws
  y2_cols <- grep("^y2\\[", colnames(draws), value = TRUE)
  y2_draws <- draws[, y2_cols]
  
  # Calculate mean and 95% prediction intervals
  y2_mean <- colMeans(y2_draws)
  y2_lower <- apply(y2_draws, 2, quantile, probs = 0.025)
  y2_upper <- apply(y2_draws, 2, quantile, probs = 0.975)
  
  # Create time index (normalized to [0, 1])
  n_obs <- length(y2_mean)
  time_norm <- seq(0, 1, length.out = n_obs)
  
  # Return data frame
  data.frame(
    time = time_norm,
    observed = as.numeric(Y2_fold),
    mean = y2_mean,
    lower = y2_lower,
    upper = y2_upper
  )
}

# -----------------------------------------------------------------------------
# Prepare Data for All Folds and Models
# -----------------------------------------------------------------------------

cat("Preparing plotting data...\n")

# Combine data for all folds and models
plot_data_list <- list()

for (fold_idx in 1:n_cv) {
  # Extract predictions for each model
  se_data <- extract_predictions(SE_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_SE", fold = fold_idx)
  
  per_data <- extract_predictions(PER_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_PER", fold = fold_idx)
  
  sexper_data <- extract_predictions(SExPER_draws[[fold_idx]], fold_idx, Y_vec, X_mat) %>%
    mutate(model = "GP_MLR_SExPER", fold = fold_idx)
  
  plot_data_list[[fold_idx]] <- bind_rows(se_data, per_data, sexper_data)
}

# Combine all folds
plot_data <- bind_rows(plot_data_list)

# Create factor for models with clean labels
plot_data$model_label <- factor(
  plot_data$model,
  levels = c("GP_MLR_SE", "GP_MLR_PER", "GP_MLR_SExPER"),
  labels = c("GP SE Kernel", "GP Periodic Kernel", "GP SE × Periodic Kernel")
)

# -----------------------------------------------------------------------------
# Create Plot
# -----------------------------------------------------------------------------

cat("Creating plot...\n")

# Define colors for models
model_colors <- c(
  "GP SE Kernel" = "#0072B2",
  "GP Periodic Kernel" = "#D55E00",
  "GP SE × Periodic Kernel" = "#009E73"
)

# Define colors for prediction intervals (lighter versions)
interval_colors <- c(
  "GP SE Kernel" = "#56B4E9",
  "GP Periodic Kernel" = "#E69F00",
  "GP SE × Periodic Kernel" = "#00D9A3"
)

# Create the plot
p <- ggplot(plot_data, aes(x = time)) +
  # 95% Prediction intervals
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = model_label),
    alpha = 0.3
  ) +
  # Prediction lines
  geom_line(
    aes(y = mean, color = model_label),
    linewidth = 0.8
  ) +
  # Observed values
  geom_line(
    aes(y = observed, linetype = "Observed"),
    color = "black",
    linewidth = 0.6
  ) +
  # Facet by fold
  facet_wrap(~ fold, ncol = 2, labeller = label_both) +
  # Scales
  scale_color_manual(
    name = "Forecasts",
    values = model_colors
  ) +
  scale_fill_manual(
    name = "95% Prediction Interval",
    values = interval_colors
  ) +
  scale_linetype_manual(
    name = "",
    values = c("Observed" = "dashed"),
    labels = c("Observed")
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = c("0.00", "0.25", "0.50", "0.75", "1.00")
  ) +
  # Labels
  labs(
    x = "Time",
    y = expression(NO[2]~levels)
  ) +
  # Theme
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold")
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    fill = guide_legend(order = 2, nrow = 1),
    linetype = guide_legend(order = 3, nrow = 1)
  )

# -----------------------------------------------------------------------------
# Save Plot
# -----------------------------------------------------------------------------

cat("Saving plot...\n")

# Save as high-resolution PDF
ggsave(
  filename = file.path(figures_dir, "gp_predictions_all_folds.pdf"),
  plot = p,
  width = 10,
  height = 14,
  units = "in",
  dpi = 300
)

cat("\n", rep("=", 70), "\n", sep = "")
cat("PLOT SAVED SUCCESSFULLY\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("\nPDF: %s\n", file.path(figures_dir, "gp_predictions_all_folds.pdf")))

# -----------------------------------------------------------------------------
# Display Summary Statistics
# -----------------------------------------------------------------------------

cat("\n", rep("-", 70), "\n", sep = "")
cat("SUMMARY STATISTICS\n")
cat(rep("-", 70), "\n", sep = "")

# Calculate coverage of prediction intervals
coverage_stats <- plot_data %>%
  group_by(model_label, fold) %>%
  summarize(
    coverage = mean(observed >= lower & observed <= upper) * 100,
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_coverage = mean(coverage),
    sd_coverage = sd(coverage),
    .groups = "drop"
  )

cat("\n95% Prediction Interval Coverage (%):\n")
print(coverage_stats, n = Inf)

# Calculate average prediction interval width
interval_width <- plot_data %>%
  group_by(model_label, fold) %>%
  summarize(
    avg_width = mean(upper - lower),
    .groups = "drop"
  ) %>%
  group_by(model_label) %>%
  summarize(
    mean_width = mean(avg_width),
    sd_width = sd(avg_width),
    .groups = "drop"
  )

cat("\nAverage Prediction Interval Width:\n")
print(interval_width, n = Inf)

cat("\n", rep("=", 70), "\n", sep = "")

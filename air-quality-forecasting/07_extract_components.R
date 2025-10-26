# ==============================================================================
# Extract and Visualize GP Components
# Purpose: Decompose GP predictions into trend and seasonal components
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(posterior)
})

cat("Starting component extraction...\n")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
fig_dir <- "output/figures"

# Ensure output directories exist
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------

cat("Loading data...\n")
load(file.path(data_dir, "transformed_data.RData"))

# -----------------------------------------------------------------------------
# Function to Extract Components - Memory Efficient
# -----------------------------------------------------------------------------

extract_components_efficient <- function(fold_idx = 1) {
  
  cat(sprintf("Processing fold %d...\n", fold_idx))
  
  # Get fold data
  X2_fold <- window(
    x = X_mat, 
    start = start(X2) + c(fold_idx-1, 0), 
    end = end(X2) + c(fold_idx-1, 0)
  )
  Y2_fold <- window(
    x = Y_vec, 
    start = start(Y2) + c(fold_idx-1, 0), 
    end = end(Y2) + c(fold_idx-1, 0)
  )
  
  # Load and process SE kernel (trend)
  load(file.path(results_dir, "GP_MLR_SE_IR.RData"))
  draws_se <- SE_draws[[fold_idx]]
  
  # Get all variable names and filter for gp_component
  all_vars <- variables(draws_se)
  gp_vars <- all_vars[grepl("^gp_component\\[", all_vars)]
  beta_vars <- all_vars[grepl("^beta\\[", all_vars)]
  
  gp_se <- subset_draws(draws_se, variable = gp_vars)
  gp_se_mean <- colMeans(as_draws_matrix(gp_se))
  
  beta_se <- subset_draws(draws_se, variable = beta_vars)
  beta_se_mean <- colMeans(as_draws_matrix(beta_se))
  linear_se <- as.matrix(X2_fold) %*% beta_se_mean
  
  rm(SE_draws, draws_se, gp_se, beta_se)
  gc()
  
  # Load and process Periodic kernel (seasonal)
  load(file.path(results_dir, "GP_MLR_PER_IR.RData"))
  draws_per <- PER_draws[[fold_idx]]
  
  all_vars <- variables(draws_per)
  gp_vars <- all_vars[grepl("^gp_component\\[", all_vars)]
  beta_vars <- all_vars[grepl("^beta\\[", all_vars)]
  
  gp_per <- subset_draws(draws_per, variable = gp_vars)
  gp_per_mean <- colMeans(as_draws_matrix(gp_per))
  
  beta_per <- subset_draws(draws_per, variable = beta_vars)
  beta_per_mean <- colMeans(as_draws_matrix(beta_per))
  linear_per <- as.matrix(X2_fold) %*% beta_per_mean
  
  rm(PER_draws, draws_per, gp_per, beta_per)
  gc()
  
  # Load and process Product kernel
  load(file.path(results_dir, "GP_MLR_SExPER_IR.RData"))
  draws_product <- SExPER_draws[[fold_idx]]
  
  all_vars <- variables(draws_product)
  y2_vars <- all_vars[grepl("^y2\\[", all_vars)]
  
  y2_product <- subset_draws(draws_product, variable = y2_vars)
  y2_product_mean <- colMeans(as_draws_matrix(y2_product))
  y2_lower <- apply(as_draws_matrix(y2_product), 2, quantile, probs = 0.025)
  y2_upper <- apply(as_draws_matrix(y2_product), 2, quantile, probs = 0.975)
  
  rm(SExPER_draws, draws_product, y2_product)
  gc()
  
  # Create result data frame
  time_idx <- time(Y2_fold)
  
  df <- data.frame(
    time = time_idx,
    observations = as.numeric(Y2_fold),
    trend_effect = gp_se_mean,
    trend_linear = as.numeric(linear_se),
    trend_total = as.numeric(linear_se) + gp_se_mean,
    seasonal_effect = gp_per_mean,
    seasonal_linear = as.numeric(linear_per),
    seasonal_total = as.numeric(linear_per) + gp_per_mean,
    prediction = y2_product_mean,
    pred_lower = y2_lower,
    pred_upper = y2_upper
  )
  
  return(df)
}

# -----------------------------------------------------------------------------
# Create Decomposition Plots - Process ONE fold at a time
# -----------------------------------------------------------------------------

cat("\n=== Creating Component Decomposition Plots ===\n\n")

fold_to_plot <- 1
df_components <- extract_components_efficient(fold_idx = fold_to_plot)

# Plot 1: Observations with Trend and Seasonal Overlaid
p1 <- ggplot(df_components, aes(x = time)) +
  # Observations
  geom_line(aes(y = observations, color = "Observations"), 
            linewidth = 0.5, alpha = 0.6) +
  # Trend (from SE kernel)
  geom_line(aes(y = trend_total, color = "Trend (SE)"), 
            linewidth = 1) +
  # Seasonal (from Periodic kernel)
  geom_line(aes(y = seasonal_effect, color = "Seasonal (Periodic)"), 
            linewidth = 0.8, linetype = "dashed") +
  # Combined prediction
  geom_line(aes(y = prediction, color = "Combined (SE×PER)"), 
            linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Observations" = "gray40",
      "Trend (SE)" = "#E41A1C",
      "Seasonal (Periodic)" = "#377EB8",
      "Combined (SE×PER)" = "#4DAF4A"
    )
  ) +
  labs(
    title = sprintf("GP Decomposition - Fold %d", fold_to_plot),
    subtitle = "Trend (SE kernel) + Seasonal (Periodic kernel) + Observations",
    x = "Time",
    y = "Value (standardized)",
    color = "Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "decomposition_overlay.pdf"), 
       plot = p1, width = 14, height = 6)
cat(sprintf("  ✓ Saved: %s\n", file.path(fig_dir, "decomposition_overlay.pdf")))

# Plot 2: Separated Components (Faceted)
df_long <- df_components %>%
  pivot_longer(
    cols = c(observations, trend_total, seasonal_effect, prediction),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = factor(
      component, 
      levels = c("observations", "trend_total", 
                "seasonal_effect", "prediction"),
      labels = c("Observations", "Trend (SE)", 
                "Seasonal (Periodic)", "Combined (SE×PER)")
    )
  )

p2 <- ggplot(df_long, aes(x = time, y = value)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ component, ncol = 1, scales = "free_y") +
  labs(
    title = sprintf("GP Component Decomposition - Fold %d", fold_to_plot),
    x = "Time",
    y = "Value (standardized)"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.background = element_rect(fill = "gray90"))

ggsave(file.path(fig_dir, "decomposition_faceted.pdf"), 
       plot = p2, width = 14, height = 10)
cat(sprintf("  ✓ Saved: %s\n", file.path(fig_dir, "decomposition_faceted.pdf")))

# Plot 3: Observations + Trend + Seasonal (Stacked View)
p3 <- ggplot(df_components, aes(x = time)) +
  # Shaded seasonal pattern
  geom_ribbon(
    aes(ymin = trend_total - abs(seasonal_effect)/2, 
        ymax = trend_total + abs(seasonal_effect)/2),
    fill = "#377EB8", alpha = 0.2
  ) +
  # Trend line
  geom_line(aes(y = trend_total, color = "Trend"), 
            linewidth = 1.2) +
  # Observations
  geom_point(aes(y = observations, color = "Observations"), 
             size = 0.8, alpha = 0.5) +
  # Combined prediction
  geom_line(aes(y = prediction, color = "Prediction"), 
            linewidth = 1, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Observations" = "gray30",
      "Trend" = "#E41A1C",
      "Prediction" = "#4DAF4A"
    )
  ) +
  labs(
    title = sprintf("Trend with Seasonal Variation - Fold %d", fold_to_plot),
    subtitle = "Shaded area shows seasonal (periodic) effect around trend",
    x = "Time",
    y = "Value (standardized)",
    color = "Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "decomposition_shaded.pdf"), 
       plot = p3, width = 14, height = 6)
cat(sprintf("  ✓ Saved: %s\n", file.path(fig_dir, "decomposition_shaded.pdf")))

# Plot 4: Only Trend and Seasonal (Clean Decomposition)
p4 <- ggplot(df_components, aes(x = time)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_line(aes(y = trend_total, color = "Trend (SE)"), 
            linewidth = 1.2) +
  geom_line(aes(y = seasonal_effect, color = "Seasonal (Periodic)"), 
            linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Trend (SE)" = "#E41A1C",
      "Seasonal (Periodic)" = "#377EB8"
    )
  ) +
  labs(
    title = sprintf("Extracted Components - Fold %d", fold_to_plot),
    subtitle = "Trend from SE kernel | Seasonal from Periodic kernel",
    x = "Time",
    y = "Effect (standardized)",
    color = "Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "decomposition_clean.pdf"), 
       plot = p4, width = 14, height = 6)
cat(sprintf("  ✓ Saved: %s\n", file.path(fig_dir, "decomposition_clean.pdf")))

# -----------------------------------------------------------------------------
# Create Plots for Selected Folds Only (to save memory)
# -----------------------------------------------------------------------------

cat("\n=== Creating plots for selected CV folds ===\n\n")

# Process folds 1, 4, and 7 (beginning, middle, end)
for (fold_i in c(1, 4, 7)) {
  df_fold <- extract_components_efficient(fold_idx = fold_i)
  
  p_fold <- ggplot(df_fold, aes(x = time)) +
    geom_line(aes(y = observations, color = "Observations"), 
              linewidth = 0.5, alpha = 0.6) +
    geom_line(aes(y = trend_total, color = "Trend (SE)"), 
              linewidth = 1) +
    geom_line(aes(y = seasonal_effect, color = "Seasonal (Periodic)"), 
              linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = prediction, color = "Combined (SE×PER)"), 
              linewidth = 1.2) +
    scale_color_manual(
      values = c(
        "Observations" = "gray40",
        "Trend (SE)" = "#E41A1C",
        "Seasonal (Periodic)" = "#377EB8",
        "Combined (SE×PER)" = "#4DAF4A"
      )
    ) +
    labs(
      title = sprintf("GP Decomposition - Fold %d", fold_i),
      x = "Time",
      y = "Value (standardized)",
      color = "Component"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  filename <- sprintf("decomposition_fold%d.pdf", fold_i)
  ggsave(file.path(fig_dir, filename), plot = p_fold, width = 14, height = 6)
  cat(sprintf("  ✓ Saved: %s\n", file.path(fig_dir, filename)))
  
  rm(df_fold, p_fold)
  gc()
}

# -----------------------------------------------------------------------------
# Save Component Data for Fold 1 Only
# -----------------------------------------------------------------------------

cat("\n=== Saving component data ===\n\n")

save(df_components, file = file.path(results_dir, "gp_components_fold1.RData"))
cat(sprintf("Saved: %s\n", file.path(results_dir, "gp_components_fold1.RData")))

# Save summary statistics
component_summary <- df_components %>%
  summarise(
    obs_mean = mean(observations),
    obs_sd = sd(observations),
    trend_mean = mean(trend_total),
    trend_sd = sd(trend_total),
    seasonal_mean = mean(seasonal_effect),
    seasonal_sd = sd(seasonal_effect),
    seasonal_amplitude = (max(seasonal_effect) - min(seasonal_effect)) / 2
  )

# -----------------------------------------------------------------------------
# Print Summary
# -----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n", sep = "")
cat("COMPONENT DECOMPOSITION SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nComponent Statistics (Fold 1):\n")
print(component_summary)

cat("\n", rep("=", 70), "\n", sep = "")
cat("DECOMPOSITION COMPLETE\n")
cat(rep("=", 70), "\n\n", sep = "")
cat(sprintf("All plots saved to: %s\n", fig_dir))
cat(sprintf("Component data saved to: %s\n", 
            file.path(results_dir, "gp_components_fold1.RData")))

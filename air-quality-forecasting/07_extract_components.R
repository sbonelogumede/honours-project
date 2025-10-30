# ==============================================================================
# Extract and Visualize GP Components - SIMPLE VERSION
# Purpose: Create 3 plots showing component decomposition for fold 1
#          - Plot 1: SE Model (MLR_SE + SE + WN)
#          - Plot 2: PER Model (MLR_PER + PER + WN)
#          - Plot 3: SExPER Model (MLR_SExPER + SExPER + WN)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(posterior)
})

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
fig_dir <- "output/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# Load Data --------------------------------------------------------------------
cat("Loading data...\n")
load(file.path(data_dir, "transformed_data.RData"))

# Extract components for fold 1 -----------------------------------------------
fold_idx <- 1

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

time_idx <- time(Y2_fold)
observations <- as.numeric(Y2_fold)

# Extract SE Model Components --------------------------------------------------
cat("Extracting SE model components...\n")
load(file.path(results_dir, "GP_MLR_SE_IR.RData"))
draws_se <- SE_draws[[fold_idx]]

# Get GP component (SE kernel)
gp_vars <- variables(draws_se)[grepl("^gp_component\\[", variables(draws_se))]
gp_se <- colMeans(as_draws_matrix(subset_draws(draws_se, variable = gp_vars)))

# Get beta and compute MLR
beta_vars <- variables(draws_se)[grepl("^beta\\[", variables(draws_se))]
beta_se <- colMeans(as_draws_matrix(subset_draws(draws_se, variable = beta_vars)))
mlr_se <- as.numeric(as.matrix(X2_fold) %*% beta_se)

# Get predictions
y2_vars <- variables(draws_se)[grepl("^y2\\[", variables(draws_se))]
y2_se <- colMeans(as_draws_matrix(subset_draws(draws_se, variable = y2_vars)))

# Compute white noise
wn_se <- y2_se - (mlr_se + gp_se)

rm(SE_draws, draws_se)
gc()

# Extract PER Model Components -------------------------------------------------
cat("Extracting PER model components...\n")
load(file.path(results_dir, "GP_MLR_PER_IR.RData"))
draws_per <- PER_draws[[fold_idx]]

# Get GP component (PER kernel)
gp_vars <- variables(draws_per)[grepl("^gp_component\\[", variables(draws_per))]
gp_per <- colMeans(as_draws_matrix(subset_draws(draws_per, variable = gp_vars)))

# Get beta and compute MLR
beta_vars <- variables(draws_per)[grepl("^beta\\[", variables(draws_per))]
beta_per <- colMeans(as_draws_matrix(subset_draws(draws_per, variable = beta_vars)))
mlr_per <- as.numeric(as.matrix(X2_fold) %*% beta_per)

# Get predictions
y2_vars <- variables(draws_per)[grepl("^y2\\[", variables(draws_per))]
y2_per <- colMeans(as_draws_matrix(subset_draws(draws_per, variable = y2_vars)))

# Compute white noise
wn_per <- y2_per - (mlr_per + gp_per)

rm(PER_draws, draws_per)
gc()

# Extract SExPER Model Components ----------------------------------------------
cat("Extracting SExPER model components...\n")
load(file.path(results_dir, "GP_MLR_SExPER_IR.RData"))
draws_sexper <- SExPER_draws[[fold_idx]]

# Get GP component (SExPER kernel)
gp_vars <- variables(draws_sexper)[grepl("^gp_component\\[", variables(draws_sexper))]
gp_sexper <- colMeans(as_draws_matrix(subset_draws(draws_sexper, variable = gp_vars)))

# Get beta and compute MLR
beta_vars <- variables(draws_sexper)[grepl("^beta\\[", variables(draws_sexper))]
beta_sexper <- colMeans(as_draws_matrix(subset_draws(draws_sexper, variable = beta_vars)))
mlr_sexper <- as.numeric(as.matrix(X2_fold) %*% beta_sexper)

# Get predictions
y2_vars <- variables(draws_sexper)[grepl("^y2\\[", variables(draws_sexper))]
y2_sexper <- colMeans(as_draws_matrix(subset_draws(draws_sexper, variable = y2_vars)))

# Compute white noise
wn_sexper <- y2_sexper - (mlr_sexper + gp_sexper)

rm(SExPER_draws, draws_sexper)
gc()

# Create Plot 1: SE Model ------------------------------------------------------
cat("Creating Plot 1: SE Model components...\n")

df_se <- data.frame(
  time = time_idx,
  observations = observations,
  mlr = mlr_se,
  kernel = gp_se,
  noise = wn_se,
  fitted = y2_se
)

p1 <- ggplot(df_se, aes(x = time)) +
  geom_point(aes(y = observations, color = "Observations"), 
             size = 1.5, alpha = 0.5) +
  geom_line(aes(y = mlr, color = "MLR"), linewidth = 1) +
  geom_line(aes(y = kernel, color = "SE"), linewidth = 1) +
  geom_line(aes(y = noise, color = "WN"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = fitted, color = "Fitted"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Observations" = "gray30",
      "MLR" = "#E41A1C",
      "SE" = "#377EB8",
      "WN" = "#984EA3",
      "Fitted" = "#4DAF4A"
    ),
    breaks = c("Observations", "MLR", "SE", "WN", "Fitted")
  ) +
  labs(
    title = NULL,
    x = "Time",
    y = "Value",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "SE_components_fold1.pdf"), 
       plot = p1, width = 12, height = 6)
cat("  Saved: SE_components_fold1.pdf\n")

# Create Plot 2: PER Model -----------------------------------------------------
cat("Creating Plot 2: PER Model components...\n")

df_per <- data.frame(
  time = time_idx,
  observations = observations,
  mlr = mlr_per,
  kernel = gp_per,
  noise = wn_per,
  fitted = y2_per
)

p2 <- ggplot(df_per, aes(x = time)) +
  geom_point(aes(y = observations, color = "Observations"), 
             size = 1.5, alpha = 0.5) +
  geom_line(aes(y = mlr, color = "MLR"), linewidth = 1) +
  geom_line(aes(y = kernel, color = "PER"), linewidth = 1) +
  geom_line(aes(y = noise, color = "WN"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = fitted, color = "Fitted"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Observations" = "gray30",
      "MLR" = "#E41A1C",
      "PER" = "#377EB8",
      "WN" = "#984EA3",
      "Fitted" = "#4DAF4A"
    ),
    breaks = c("Observations", "MLR", "PER", "WN", "Fitted")
  ) +
  labs(
    title = NULL,
    x = "Time",
    y = "Value",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "PER_components_fold1.pdf"), 
       plot = p2, width = 12, height = 6)
cat("  Saved: PER_components_fold1.pdf\n")

# Create Plot 3: SExPER Model --------------------------------------------------
cat("Creating Plot 3: SExPER Model components...\n")

df_sexper <- data.frame(
  time = time_idx,
  observations = observations,
  mlr = mlr_sexper,
  kernel = gp_sexper,
  noise = wn_sexper,
  fitted = y2_sexper
)

p3 <- ggplot(df_sexper, aes(x = time)) +
  geom_point(aes(y = observations, color = "Observations"), 
             size = 1.5, alpha = 0.5) +
  geom_line(aes(y = mlr, color = "MLR"), linewidth = 1) +
  geom_line(aes(y = kernel, color = "SExPER"), linewidth = 1) +
  geom_line(aes(y = noise, color = "WN"), linewidth = 0.8, alpha = 0.7) +
  geom_line(aes(y = fitted, color = "Fitted"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "Observations" = "gray30",
      "MLR" = "#E41A1C",
      "SExPER" = "#377EB8",
      "WN" = "#984EA3",
      "Fitted" = "#4DAF4A"
    ),
    breaks = c("Observations", "MLR", "SExPER", "WN", "Fitted")
  ) +
  labs(
    title = NULL,
    x = "Time",
    y = "Value",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "SExPER_components_fold1.pdf"), 
       plot = p3, width = 12, height = 6)
cat("  Saved: SExPER_components_fold1.pdf\n")

# Done -------------------------------------------------------------------------
cat("\n=== COMPLETE ===\n")
cat("Generated 3 plots for fold 1:\n")
cat("  1. SE_components_fold1.pdf\n")
cat("  2. PER_components_fold1.pdf\n")
cat("  3. SExPER_components_fold1.pdf\n")
cat(sprintf("\nSaved to: %s\n", fig_dir))

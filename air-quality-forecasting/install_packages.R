# install_packages.R
# ------------------------------
# Define user library path (use existing R_LIBS_USER or default to ~/R/…)
lib_path <- Sys.getenv("R_LIBS_USER")
if (lib_path == "") {
  lib_path <- file.path(Sys.getenv("HOME"), "R",
                        paste0("x86_64-pc-linux-gnu-library/",
                               paste(R.version$major, R.version$minor, sep = ".")))
  Sys.setenv(R_LIBS_USER = lib_path)
}
# Ensure the library path exists
if (!dir.exists(lib_path)) dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
# ------------------------------
# Install and load required packages
# ------------------------------
required_pkgs <- c("dplyr", "forecast", "GGally", "gridExtra", "grid", "ggplot2", "ggcorrplot", "lubridate", "parallel", "readxl", "tidyr", "qqconf", "remotes")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = lib_path, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE, lib.loc = lib_path)
}
# ------------------------------
# Install CmdStanR if needed
# ------------------------------
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  remotes::install_github("stan-dev/cmdstanr", lib = lib_path)
}
library(cmdstanr, lib.loc = lib_path)
# ------------------------------
# Confirmation message
# ------------------------------
message("All packages installed and loaded successfully!")
message("CmdStanR version: ", as.character(packageVersion("cmdstanr")))
message("CmdStan path: ", cmdstanr::cmdstan_path())

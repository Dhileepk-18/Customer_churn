# ==============================================================================
# File: install_packages.R
# Project: Predicting Customer Churn (College-Level Probability & Statistics)
# Description: Automated dependency installer and environment verification script.
# ==============================================================================

cat("==================================================================\n")
cat(" Predicting Customer Churn - Package Installer & Environment Check\n")
cat("==================================================================\n\n")

# Ensure local r_libs folder exists and is registered in .libPaths()
local_lib <- file.path(getwd(), "r_libs")
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(local_lib, .libPaths()))

# Set default CRAN repository if none is set
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# List of required packages for dashboard, visualization, and data manipulation
required_packages <- c(
  "shiny",        # Interactive Web Dashboard
  "bslib",        # Modern UI styling and bootstrap theme
  "ggplot2",      # Publication-quality statistical visualizations
  "dplyr",        # Data transformation and pipeline operations
  "scales"        # Formatting axis labels (percentages, currency)
)

all_target_packages <- required_packages

cat("Checking installed packages...\n")
installed <- rownames(installed.packages())
missing_pkgs <- setdiff(all_target_packages, installed)

if (length(missing_pkgs) == 0) {
  cat("All required packages are already installed and available!\n")
} else {
  cat("The following packages are missing and will be installed into 'r_libs':\n")
  cat(paste(" -", missing_pkgs, collapse = "\n"), "\n\n")
  
  install.packages(missing_pkgs, lib = local_lib, dependencies = TRUE)
}

cat("\nVerification check:\n")
for (pkg in all_target_packages) {
  avail <- requireNamespace(pkg, quietly = TRUE)
  cat(sprintf(" - %-12s: %s\n", pkg, ifelse(avail, "INSTALLED & READY", "MISSING")))
}
cat("\nSetup complete!\n")

# Generates default.nix for the llmpublic development shell.
# Run with: Rscript default.R   (requires the rix package)

library(rix)

r_pkgs <- c(
  # Imports
  "checkmate", "cli", "DBI", "dplyr", "duckdb", "htmltools", "jsonlite",
  "purrr", "scales", "tibble",
  # Development
  "devtools", "testthat", "roxygen2", "withr",
  # Optional: browser-engine test of the hover popup (skipped when absent)
  "chromote"
) |>
  unique() |>
  sort()

rix(
  date = "2026-10-13",
  project_path = ".",
  overwrite = TRUE,
  r_pkgs = r_pkgs,
  system_pkgs = NULL,
  ide = "none"
)

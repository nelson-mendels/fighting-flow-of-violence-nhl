# Builds the master play-by-play dataset used in the downstream analysis.
# Run this script from the repository root.

# Packages ----------------------------------------------------------------
library(tidyverse)
library(nhlscraper)
library(jsonlite)

# Project setup -----------------------------------------------------------
# Use the cloned repository's root on any computer. The trailing slash is
# retained for compatibility with the existing data-construction functions.
working.dir <- paste0(normalizePath(".", winslash = "/", mustWork = TRUE), "/")

scripts_dir <- file.path(working.dir, "R", "get_data_scripts")

if (!dir.exists(scripts_dir)) {
  stop(
    "Could not find R/get_data_scripts. ",
    "Run create_dataset.R from the repository root."
  )
}

# Data-construction functions --------------------------------------------
source(file.path(scripts_dir, "get_schedule_info.R"))
source(file.path(scripts_dir, "download_data.R"))
source(file.path(scripts_dir, "cleaning_function.R"))
source(file.path(scripts_dir, "clean_data.R"))
source(file.path(scripts_dir, "stack_seasons.R"))

# Build master dataset ---------------------------------------------------
# Download NHL play-by-play data for seasons 2010-11 through 2025-26.
download_data(working.dir)

# Clean each season-level file.
clean_data(working.dir)

# Combine the cleaned season files.
master_data <- stack_seasons(working.dir)

# Save output -------------------------------------------------------------
output_dir <- file.path(working.dir, "generated", "pbp_data")
output_file <- file.path(output_dir, "master_data.rds")

# Replace the existing master-data output directory.
unlink(output_dir, recursive = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(master_data, file = output_file)

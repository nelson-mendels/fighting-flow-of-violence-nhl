# Cleans each season-level play-by-play file produced by download_data().
# Saves valid files to data/game_logs_cleaned/ under their original names.

clean_data <- function(working.dir) {
  input_folder <- file.path(working.dir, "data", "game_logs")
  output_folder <- file.path(working.dir, "data", "game_logs_cleaned")

  if (!dir.exists(input_folder)) {
    stop("Downloaded data directory not found: ", input_folder)
  }

  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  rds_files <- sort(list.files(
    input_folder, pattern = "\\.rds$", full.names = TRUE
  ))

  if (length(rds_files) == 0) {
    stop("No downloaded season files found in: ", input_folder)
  }

  cat("Found", length(rds_files), "season files.\n")

  for (rds_file in rds_files) {
    cat("Cleaning:", basename(rds_file), "\n")
    pbp_clean <- cleaning_function(rds_file)

    if (is.null(pbp_clean)) {
      warning("Cleaned data are NULL for ", basename(rds_file))
      next
    }

    if (nrow(pbp_clean) == 0) {
      warning("Cleaned data have zero rows for ", basename(rds_file))
      next
    }

    saveRDS(pbp_clean, file.path(output_folder, basename(rds_file)))
  }
}

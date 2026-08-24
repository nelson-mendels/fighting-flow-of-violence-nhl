# Reads all cleaned season-level play-by-play files and combines them into
# one master dataset for downstream analysis.

stack_seasons <- function(working.dir) {
  input_folder <- file.path(working.dir, "data", "game_logs_cleaned")

  if (!dir.exists(input_folder)) {
    stop("Cleaned data directory not found: ", input_folder)
  }

  season_logs <- sort(list.files(
    input_folder, pattern = "\\.rds$", full.names = TRUE
  ))

  if (length(season_logs) == 0) {
    stop("No cleaned season files found in: ", input_folder)
  }

  cat("Found", length(season_logs), "cleaned season files.\n")

  data_list <- list()

  for (season_file in season_logs) {
    cat("Reading:", basename(season_file), "\n")

    season_data <- tryCatch(
      readRDS(season_file),
      error = function(e) {
        warning(
          "Could not read ", basename(season_file), ": ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (!is.null(season_data)) {
      data_list[[length(data_list) + 1]] <- season_data
    }
  }

  if (length(data_list) == 0) {
    stop("None of the cleaned season files could be read.")
  }

  bind_rows(data_list)
}

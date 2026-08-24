# Downloads regular-season NHL play-by-play data from 2010-11 through 2025-26.
# Saves one type-standardized RDS file per season and checks game coverage.

download_data <- function(working.dir) {
  seasons <- c(
    20102011, 20112012, 20122013, 20132014,
    20142015, 20152016, 20162017, 20172018,
    20182019, 20192020, 20202021, 20212022,
    20222023, 20232024, 20242025, 20252026
  )

  game_log_dir <- file.path(working.dir, "data", "game_logs")
  dir.create(game_log_dir, recursive = TRUE, showWarnings = FALSE)

  standardize_types <- function(df) {
    id_cols <- intersect(
      c(
        "gameId",
        "eventId",
        "seasonId",
        "gameTypeId",
        "gameNumber",
        "eventOwnerTeamId",
        "eventTypeCode",
        grep("PlayerId", names(df), value = TRUE)
      ),
      names(df)
    )

    chr_cols <- intersect(
      c(
        "periodType",
        "eventTypeDescKey",
        "situationCode",
        "strengthState",
        "homeTeamDefendingSide",
        "zoneCode",
        "shotType",
        "reason",
        "secondaryReason",
        "penaltyTypeCode",
        "penaltyTypeDescKey"
      ),
      names(df)
    )

    lgl_cols <- intersect(
      c(
        "isHome",
        "homeIsEmptyNet",
        "awayIsEmptyNet",
        "isEmptyNetFor",
        "isEmptyNetAgainst",
        "isRush",
        "isRebound",
        "createdRebound"
      ),
      names(df)
    )

    df %>%
      mutate(
        across(all_of(id_cols), as.character),
        across(all_of(chr_cols), as.character),
        across(all_of(lgl_cols), as.logical)
      )
  }

  for (season_id in seasons) {
    season_std <- as.character(season_id)
    suffix <- paste0(substr(season_std, 1, 4), "-", substr(season_std, 5, 8))
    filename <- file.path(game_log_dir, paste0("pbp_", suffix, ".rds"))

    cat("Working on season", suffix, "...\n")

    season_pbp <- tryCatch(
      gc_play_by_plays(season_id),
      error = function(e) {
        warning(
          "gc_play_by_plays() failed for ",
          season_id,
          ": ",
          conditionMessage(e)
        )
        NULL
      }
    )

    if (is.null(season_pbp) || !is.data.frame(season_pbp) ||
        nrow(season_pbp) == 0) {
      warning("No data returned for season: ", suffix)
      next
    }

    if (!"gameTypeId" %in% names(season_pbp)) {
      stop("Downloaded data do not contain gameTypeId for season ", suffix)
    }

    season_pbp <- standardize_types(season_pbp) %>%
      filter(as.integer(gameTypeId) == 2)

    if (nrow(season_pbp) == 0) {
      warning("No regular-season rows for season: ", suffix)
      next
    }

    expected_ids <- get_schedule_info(season_std) %>%
      pull(game_id_full)

    actual_ids <- season_pbp %>%
      distinct(gameId) %>%
      pull(gameId)

    missing_ids <- setdiff(expected_ids, actual_ids)

    cat("  Expected regular-season games:", length(expected_ids), "\n")
    cat("  Downloaded games:             ", length(actual_ids), "\n")

    if (length(missing_ids) > 0) {
      warning(
        length(missing_ids),
        " game(s) missing for ",
        suffix,
        ": ",
        paste(head(missing_ids, 20), collapse = ", "),
        if (length(missing_ids) > 20) " ..." else ""
      )
    } else {
      cat("  No missing games.\n")
    }

    cat("  Total rows:", nrow(season_pbp), "\n")
    cat("  Saving:", filename, "\n\n")

    saveRDS(season_pbp, file = filename)
  }
}

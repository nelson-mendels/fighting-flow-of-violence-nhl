# Counts the number of fighting penalties assessed at each unique fight moment.
# Saves the resulting distribution and summary totals for the paper table.

build_fight_penalty_distribution <- function(working.dir, pbp_master) {
  
  # Identify individual fighting penalties
  fight_penalties <- pbp_master %>%
    filter(
      event_type == "PENALTY",
      penalty_severity == "Major",
      str_detect(str_to_lower(description), "fight"),
      !is.na(season),
      !is.na(game_id),
      is.finite(game_seconds)
    ) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id)
    )
  
  if (nrow(fight_penalties) == 0) {
    stop("No fighting penalties found in pbp_master.")
  }
  
  # Count penalties assessed at each unique fight moment
  per_fight <- fight_penalties %>%
    count(
      season,
      game_id,
      game_seconds,
      name = "n_penalties"
    )
  
  # Build the distribution of penalties per fight moment
  max_row <- max(10, max(per_fight$n_penalties))
  
  distribution <- tibble(
    n_penalties = 1:max_row
  ) %>%
    left_join(
      per_fight %>%
        count(
          n_penalties,
          name = "n_fights"
        ),
      by = "n_penalties"
    ) %>%
    mutate(
      n_fights = replace_na(n_fights, 0L),
      percent_of_fights = 100 * n_fights / sum(n_fights)
    )
  
  # Store distribution metadata
  seasons_present <- sort(unique(fight_penalties$season))
  
  out <- list(
    distribution = distribution,
    n_fight_moments = nrow(per_fight),
    n_fighting_penalties = nrow(fight_penalties),
    first_season = seasons_present[1],
    last_season = seasons_present[length(seasons_present)]
  )
  
  # Save the completed distribution
  out_dir <- file.path(
    working.dir,
    "generated",
    "fights"
  )
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  out_path <- file.path(
    out_dir,
    "fight_penalty_distribution.rds"
  )
  
  saveRDS(out, out_path)
  
  message("Saved: generated/fights/fight_penalty_distribution.rds")
  message(
    "Fight moments: ",
    format(out$n_fight_moments, big.mark = ","),
    " | fighting penalties: ",
    format(out$n_fighting_penalties, big.mark = ",")
  )
  
  invisible(out)
}
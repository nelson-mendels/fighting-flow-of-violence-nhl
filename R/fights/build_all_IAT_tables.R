# Builds and combines the event-level interarrival tables for all single-fight games.
# Adds game characteristics and each game's relative month within its NHL season.

build_all_IAT_tables <- function(
    working.dir,
    pbp_master,
    events,
    one_fight_games = readRDS(
      file.path(working.dir, "generated", "fights", "single_fight_games.rds")
    ),
    pre_fight_window_min = 1
) {
  
  # Standardize identifiers
  pbp_master <- pbp_master %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      game_id_full = as.character(game_id_full)
    )
  
  if (!"game_date" %in% names(pbp_master)) {
    stop(
      "pbp_master has no game_date column, so month-of-season cannot be ",
      "assigned. Check the schedule merge in the cleaning step."
    )
  }
  
  # Assign each calendar month its relative position within the season
  month_lookup <- pbp_master %>%
    distinct(season, game_id, game_date) %>%
    mutate(month_key = format(as.Date(game_date), "%Y-%m")) %>%
    group_by(season) %>%
    mutate(season_month = dense_rank(month_key)) %>%
    ungroup() %>%
    select(season, game_id, season_month)
  
  n_bad_dates <- sum(is.na(month_lookup$season_month))
  
  if (n_bad_dates > 0) {
    warning(
      n_bad_dates,
      " games have missing or unparseable game_date, so their season_month ",
      "is NA. fit_IAT_log_model() will refuse to run if any of these are ",
      "one-fight games."
    )
  }
  
  # Classify violent-contact events once before processing individual games
  if ("VIOLENT_CONTACT" %in% events) {
    pbp_master <- filter_penalties(pbp_master)
  }
  
  # Build and combine the interarrival table for each single-fight game
  single_fight_games_IAT_table <- map_dfr(
    seq_len(nrow(one_fight_games)),
    function(i) {
      if (i %% 50 == 0 || i == 1 || i == nrow(one_fight_games)) {
        message(
          "Building IAT table for single-fight game ",
          i,
          " of ",
          nrow(one_fight_games),
          "..."
        )
      }
      
      build_IAT_table(
        working.dir = working.dir,
        pbp_master = pbp_master,
        events = events,
        game_id_input = one_fight_games$game_id[i],
        season_input = one_fight_games$season[i],
        pre_fight_window_min = pre_fight_window_min
      )
    }
  ) %>%
    left_join(
      one_fight_games %>%
        select(
          game_id,
          season,
          intradivision,
          fight_score_diff,
          fight_period
        ),
      by = c("game_id", "season")
    ) %>%
    left_join(
      month_lookup,
      by = c("game_id", "season")
    )
  
  # Save the combined interarrival dataset
  output_folder <- file.path(working.dir, "generated", "fights")
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  write_rds(
    single_fight_games_IAT_table,
    file.path(output_folder, "single_fight_games_IAT_table.rds")
  )
  
  return(single_fight_games_IAT_table)
}
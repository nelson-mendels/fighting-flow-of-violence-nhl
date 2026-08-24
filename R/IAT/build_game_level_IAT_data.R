# Builds a game-level inter-arrival-time dataset for the selected event types.
# Calculates distributional statistics for each game and records its fight count.

build_game_level_IAT_data <- function(
    working.dir,
    pbp_master,
    events,
    progress_every = 100
) {
  
  # Standardize identifiers
  pbp_master <- pbp_master %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      game_id_full = as.character(game_id_full)
    )
  
  # Classify violent-contact events when requested
  if ("VIOLENT_CONTACT" %in% events) {
    pbp_master <- filter_penalties(pbp_master)
  }
  
  # Count unique fight times within each game
  fights <- pbp_master %>%
    filter(fight_flag == 1) %>%
    distinct(season, game_id_full, game_seconds) %>%
    group_by(season, game_id_full) %>%
    summarise(n_fights = n(), .groups = "drop")
  
  # Retain the selected event types
  game_event_data <- pbp_master %>%
    filter(event_type %in% events) %>%
    arrange(season, game_id_full, game_seconds)
  
  games_to_run <- game_event_data %>%
    distinct(
      season,
      game_id,
      game_id_full,
      game_date,
      home_name,
      away_name
    ) %>%
    arrange(season, game_id_full)
  
  message("Games to process: ", nrow(games_to_run))
  
  IAT_results_game <- vector("list", nrow(games_to_run))
  
  # Calculate IAT statistics for each game
  for (i in seq_len(nrow(games_to_run))) {
    
    if (
      i %% progress_every == 0 ||
      i == 1 ||
      i == nrow(games_to_run)
    ) {
      message(
        "Building game-level IAT data: game ",
        i,
        " of ",
        nrow(games_to_run)
      )
    }
    
    season_i <- games_to_run$season[i]
    game_id_i <- games_to_run$game_id[i]
    game_id_full_i <- games_to_run$game_id_full[i]
    
    game_data <- game_event_data %>%
      filter(
        season == season_i,
        game_id_full == game_id_full_i
      ) %>%
      arrange(game_seconds) %>%
      mutate(
        game_num = 1L,
        season_seconds = game_seconds
      )
    
    iat_stats <- calculate_IAT(game_data)
    
    n_fights_i <- fights %>%
      filter(
        season == season_i,
        game_id_full == game_id_full_i
      ) %>%
      pull(n_fights)
    
    n_fights_i <- ifelse(
      length(n_fights_i) == 0,
      0L,
      as.integer(n_fights_i)
    )
    
    IAT_results_game[[i]] <- tibble(
      season = season_i,
      event = paste(events, collapse = ", "),
      game_id = game_id_i,
      game_id_full = game_id_full_i,
      game_date = games_to_run$game_date[i],
      home_name = games_to_run$home_name[i],
      away_name = games_to_run$away_name[i],
      lambda = round(as.numeric(iat_stats$lambda), 2),
      ad_p_value = as.numeric(iat_stats$ad_raw_p_value),
      min_IAT = min(iat_stats$IAT_values),
      max_IAT = max(iat_stats$IAT_values),
      n = nrow(game_data),
      n_fights = n_fights_i
    )
  }
  
  # Combine and save game-level results
  game_level_data <- bind_rows(IAT_results_game)
  
  dir.create(
    file.path(working.dir, "generated/IAT_data"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  saveRDS(
    game_level_data,
    file.path(
      working.dir,
      "generated/IAT_data/IAT_game_level_data.rds"
    )
  )
  
  message("Saved: generated/IAT_data/IAT_game_level_data.rds")
  
  return(game_level_data)
}

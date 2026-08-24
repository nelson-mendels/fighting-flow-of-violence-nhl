# pre_fight_window_min: width, in minutes, of the window immediately before
# the fight used to flag the run-up to a fight. An interval is flagged when
# it is a pre-fight interval and ENDS within that many minutes of the fight
# (the interval's terminating event is the violent act, and the censored
# pre-fight interval ends exactly at the fight, so it is flagged too).
build_IAT_table <- function(working.dir, pbp_master, events, game_id_input, season_input,
                            pre_fight_window_min = 1) {
  # Filter to game, keep relevant events and fights, dedupe and sort
  game_data <- pbp_master %>% 
    filter(
      game_id == game_id_input,
      season == season_input
    ) %>% 
    {
      bind_rows(
        filter(., event_type %in% events),
        filter(., fight_flag == 1) %>% 
          distinct(game_seconds, .keep_all = TRUE)
      )
    } %>% 
    distinct() %>% 
    arrange(game_seconds) %>% 
    select(season, game_id, game_seconds, fight_flag)
  
  if (nrow(game_data) == 0) { 
    return(tibble(
      season = character(0), 
      game_id = character(0), 
      IAT = numeric(0), 
      RC = numeric(0), 
      post_fight = numeric(0), 
      interval_start = numeric(0), 
      interval_end = numeric(0), 
      current_period = integer(0), 
      pre_fight_window = integer(0) 
    )) 
  }
  
  
  game_data <- game_data %>%
    mutate(synthetic = 0L) %>% 
    bind_rows( ## <-- PATCH
      tibble( ## <-- PATCH
        season = game_data$season[1], 
        game_id = game_data$game_id[1],
        game_seconds = 0, ## <-- PATCH
        fight_flag = 0, ## <-- PATCH
        synthetic = 1L ## <-- PATCH
      ) ## <-- PATCH
    ) %>% ## <-- PATCH
    arrange(game_seconds, desc(fight_flag), synthetic) %>% 
    distinct(game_seconds, .keep_all = TRUE) 
  
  # Extract timestamps of fight events
  fight_time <- game_data %>% 
    filter(fight_flag == 1) %>% 
    distinct(game_seconds) %>% 
    pull(game_seconds)
  
  # Build IAT table: inter-arrival times, right-censoring flag, and post-fight indicator
  game_data %>% 
    arrange(game_seconds) %>% 
    mutate(
      next_time = lead(game_seconds),
      next_fight = lead(fight_flag)
    ) %>% 
    transmute(
      season,
      game_id,
      IAT = case_when(
        is.na(next_time) ~ 3600 - game_seconds,
        TRUE ~ next_time - game_seconds
      ),
      RC = case_when(
        is.na(next_time) ~ 1,
        next_fight == 1 ~ 1,
        TRUE ~ 0
      ),
      post_fight = case_when(
        length(fight_time) == 0 ~ 0,
        fight_flag == 1 ~ 1, 
        game_seconds > fight_time ~ 1,
        TRUE ~ 0
      ),
      interval_start = game_seconds,
      interval_end = interval_start + IAT,
      current_period = pmin(
        floor(((interval_start + interval_end) / 2) / 1200) + 1L,
        3L
      ),
      # 1 for pre-fight intervals ending within pre_fight_window_min minutes
      # of the fight, 0 otherwise (including every post-fight interval and
      # every interval in a game with no fight).
      pre_fight_window = case_when(
        length(fight_time) == 0 ~ 0L,
        post_fight == 1 ~ 0L,
        interval_end >= (min(fight_time) - pre_fight_window_min * 60) ~ 1L,
        TRUE ~ 0L
      )
    ) %>% 
    filter(IAT > 0) 
}
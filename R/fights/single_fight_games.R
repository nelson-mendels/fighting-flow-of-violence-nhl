# Identifies games containing exactly one fight and summarizes violent events
# before and after that fight, including event counts, rates, and binomial tests.

single_fight_games <- function(working.dir, pbp_master, events) {
  
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
  
  # Identify games with exactly one unique fight time
  fight_pattern <- "^(.*?)\\s+[Ff]ighting\\s+against\\s+(.*?)\\s*$"
  
  fights <- pbp_master %>%
    filter(fight_flag == 1) %>%
    mutate(
      fighter_1 = str_match(description, fight_pattern)[, 2],
      fighter_2 = str_match(description, fight_pattern)[, 3]
    ) %>%
    distinct(
      game_id,
      season,
      game_date,
      game_seconds,
      fighter_1,
      fighter_2
    ) %>%
    group_by(game_id, season, game_date) %>%
    summarise(
      n_fights = n_distinct(game_seconds),
      fight_time = first(game_seconds),
      fighter_1 = first(fighter_1),
      fighter_2 = first(fighter_2),
      .groups = "drop"
    ) %>%
    filter(n_fights == 1)
  
  # Add divisional matchup information
  game_meta <- pbp_master %>%
    distinct(game_id, intradivision)
  
  fights <- fights %>%
    left_join(game_meta, by = "game_id")
  
  # Record the score differential and period at the fight
  score_at_fight <- pbp_master %>%
    inner_join(
      fights %>%
        select(game_id, season, fight_time),
      by = c("game_id", "season")
    ) %>%
    filter(game_seconds <= fight_time) %>%
    group_by(game_id, season, fight_time) %>%
    arrange(
      desc(game_seconds),
      desc(period),
      .by_group = TRUE
    ) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      fight_score_diff = abs(home_score - away_score)
    ) %>%
    select(
      game_id,
      season,
      fight_score_diff,
      fight_period = period
    )
  
  fights <- fights %>%
    left_join(
      score_at_fight,
      by = c("game_id", "season")
    )
  
  # Calculate pre- and post-fight statistics for each event type
  results <- vector("list", length(events))
  
  for (i in seq_along(events)) {
    ev <- events[i]
    
    per_game <- pbp_master %>%
      filter(event_type == ev) %>%
      inner_join(
        fights %>%
          select(
            game_id,
            season,
            game_date,
            fight_time,
            fighter_1,
            fighter_2
          ),
        by = c("game_id", "season", "game_date")
      ) %>%
      group_by(game_id, season, game_date, fight_time) %>%
      summarise(
        n_pre = sum(game_seconds < fight_time),
        n_post = sum(game_seconds > fight_time),
        .groups = "drop"
      ) %>%
      mutate(
        t_before = fight_time,
        t_after = 3600 - fight_time,
        t_total = 3600,
        n_total = n_pre + n_post,
        p0 = t_before / t_total
      ) %>%
      mutate(
        p_lower = pbinom(n_pre, n_total, p0),
        p_upper = pbinom(
          n_pre - 1,
          n_total,
          p0,
          lower.tail = FALSE
        ),
        
        !!paste0("p_value_", ev) :=
          pmin(1, 2 * pmin(p_lower, p_upper)),
        
        !!paste0("n_before_", ev) := n_pre,
        !!paste0("n_after_", ev) := n_post,
        !!paste0("n_total_", ev) := n_total,
        
        !!paste0("lambda_before_", ev) :=
          ifelse(
            t_before > 0,
            n_pre / (t_before / 60),
            NA_real_
          ),
        
        !!paste0("lambda_after_", ev) :=
          ifelse(
            t_after > 0,
            n_post / (t_after / 60),
            NA_real_
          ),
        
        !!paste0("lambda_change_after_minus_before_", ev) :=
          !!sym(paste0("lambda_after_", ev)) -
          !!sym(paste0("lambda_before_", ev))
      ) %>%
      select(
        game_id,
        season,
        game_date,
        fight_time,
        t_before,
        t_after,
        t_total,
        paste0("n_before_", ev),
        paste0("n_after_", ev),
        paste0("n_total_", ev),
        paste0("lambda_before_", ev),
        paste0("lambda_after_", ev),
        paste0("lambda_change_after_minus_before_", ev),
        paste0("p_value_", ev)
      )
    
    results[[i]] <- per_game
  }
  
  # Combine results across event types and restore fight metadata
  full_table <- Reduce(
    function(x, y) {
      full_join(
        x,
        y,
        by = c(
          "game_id",
          "season",
          "game_date",
          "fight_time",
          "t_before",
          "t_after",
          "t_total"
        )
      )
    },
    results
  ) %>%
    left_join(
      fights %>%
        select(
          game_id,
          season,
          game_date,
          fight_time,
          n_fights,
          fighter_1,
          fighter_2,
          intradivision,
          fight_score_diff,
          fight_period
        ),
      .,
      by = c(
        "game_id",
        "season",
        "game_date",
        "fight_time"
      )
    )
  
  # Save the single-fight game dataset
  output_folder <- file.path(
    working.dir,
    "generated",
    "fights"
  )
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  write_rds(
    full_table,
    file.path(
      output_folder,
      "single_fight_games.rds"
    )
  )
  
  return(full_table)
}
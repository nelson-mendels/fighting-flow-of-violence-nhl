# Combines game-level interarrival statistics with the single-fight game dataset.
# Games without a qualifying single fight retain a fight count of zero.

merge_game_and_fight_data <- function(working.dir) {
  
  # Load game-level and single-fight datasets
  game_level <- readRDS(
    file.path(
      working.dir,
      "generated/IAT_data/IAT_game_level_data.rds"
    )
  )
  
  single_fight <- readRDS(
    file.path(
      working.dir,
      "generated/fights/single_fight_games.rds"
    )
  )
  
  # Merge the datasets and reconcile fight counts
  merged <- game_level %>%
    left_join(
      single_fight,
      by = c("game_id", "season", "game_date")
    ) %>%
    mutate(
      n_fights = coalesce(n_fights.x, n_fights.y, 0L)
    ) %>%
    select(-n_fights.x, -n_fights.y)
  
  # Save the merged dataset
  output_folder <- file.path(
    working.dir,
    "generated",
    "fights"
  )
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  write_rds(
    merged,
    file.path(
      output_folder,
      "game_level_with_single_fight_lambdas.rds"
    )
  )
  
  return(merged)
}
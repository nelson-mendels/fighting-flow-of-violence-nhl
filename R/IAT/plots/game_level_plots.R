# Generates and saves interarrival-time distribution plots for every selected game and event type.
# Optional season and game filters can be used to restrict the plots produced.

game_level_plots <- function(
    working.dir,
    pbp_master,
    events,
    seasons = NULL,
    game_ids = NULL,
    overwrite_existing_plots = TRUE,
    progress_every = 50,
    plot_width = 8,
    plot_height = 3.5
) {
  
  working.dir <- sub("/+$", "", working.dir)
  plots_dir_game <- file.path(working.dir, "plots")
  
  # Standardize identifiers and event labels
  pbp_master <- pbp_master %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      game_id_full = as.character(game_id_full),
      event_type = str_to_upper(as.character(event_type))
    )
  
  # Apply optional season and game filters
  if (!is.null(seasons)) {
    seasons <- as.character(seasons)
    
    pbp_master <- pbp_master %>%
      filter(season %in% .env$seasons)
  }
  
  if (!is.null(game_ids)) {
    game_ids <- as.character(game_ids)
    
    pbp_master <- pbp_master %>%
      filter(
        game_id_full %in% .env$game_ids |
          game_id %in% .env$game_ids
      )
  }
  
  # Classify violent-contact events when requested
  if ("VIOLENT_CONTACT" %in% events) {
    pbp_master <- filter_penalties(pbp_master)
  }
  
  # Generate plots for each event type
  for (event in events) {
    safe_event <- gsub("[^A-Za-z0-9_\\-]", "_", event)
    event_root <- file.path(plots_dir_game, safe_event)
    
    dir.create(
      event_root,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    plot_games <- pbp_master %>%
      filter(event_type == .env$event) %>%
      distinct(season, game_id_full) %>%
      arrange(season, game_id_full)
    
    message(
      "Plots to make for ",
      event,
      ": ",
      nrow(plot_games)
    )
    
    if (nrow(plot_games) == 0) {
      next
    }
    
    for (i in seq_len(nrow(plot_games))) {
      season_i <- plot_games$season[i]
      game_id_full_i <- plot_games$game_id_full[i]
      
      if (
        i %% progress_every == 0 ||
        i == 1 ||
        i == nrow(plot_games)
      ) {
        message(
          "  [",
          i,
          "/",
          nrow(plot_games),
          "] ",
          event,
          " | ",
          season_i,
          " | ",
          game_id_full_i
        )
      }
      
      # Set the output path for the game
      season_root <- file.path(event_root, season_i)
      game_dir <- file.path(season_root, "game")
      
      dir.create(
        game_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      output_path <- file.path(
        game_dir,
        paste0(
          gsub(
            "[^A-Za-z0-9_\\-]",
            "_",
            game_id_full_i
          ),
          ".pdf"
        )
      )
      
      if (
        !overwrite_existing_plots &&
        file.exists(output_path)
      ) {
        next
      }
      
      # Build the selected game plot
      game_data <- pbp_master %>%
        filter(
          season == .env$season_i,
          game_id_full == .env$game_id_full_i,
          event_type == .env$event
        )
      
      iat_result <- game_plot(
        working.dir = working.dir,
        master = game_data,
        season = season_i,
        event = event
      )
      
      if (!is.null(iat_result)) {
        ggsave(
          filename = output_path,
          plot = iat_result$plot,
          width = plot_width,
          height = plot_height,
          dpi = 300
        )
      }
    }
  }
  
  invisible(TRUE)
}
plot_fights_per_game_by_season <- function(working.dir, IAT_game_data) {
  # Filter IAT game data and aggregate for fighters per season
  fights_per_game_by_season_data <- IAT_game_data %>%
    filter(event == "VIOLENT_CONTACT") %>%
    distinct(season, game_id_full, .keep_all = TRUE) %>%
    mutate(
      season_start = as.integer(substr(season, 1, 4)),
      season_label = paste0(
        substr(season, 1, 4),
        "-",
        substr(season, 7, 8)
      )
    ) %>%
    group_by(season, season_start, season_label) %>%
    summarise(
      n_games = n(),
      n_fights = sum(n_fights, na.rm = TRUE),
      fights_per_game = n_fights / n_games,
      .groups = "drop"
    ) %>%
    arrange(season_start)
  
  # Create plot
  fights_per_game_by_season_plot <- fights_per_game_by_season_data %>%
    ggplot(aes(x = season_start, y = fights_per_game)) +
    geom_line(
      linewidth = 0.8,
      color = "grey25"
    ) +
    geom_point(
      size = 2,
      color = "grey25"
    ) +
    scale_x_continuous(
      breaks = fights_per_game_by_season_data$season_start,
      labels = fights_per_game_by_season_data$season_label
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = seq(0, 0.6, by = 0.1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    # Force the axis to reach 0.6 so the top tick is drawn, without
    # clipping any point that might exceed it.
    expand_limits(y = 0.6) +
    labs(
      x = "NHL season",
      y = "Fights per game"
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 10,
        color = "grey20"
      ),
      axis.text.y = element_text(
        size = 11,
        color = "grey20"
      ),
      axis.title = element_text(size = 13),
      panel.grid.major.y = element_line(
        color = "grey88",
        linewidth = 0.3
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(paste0(working.dir, "plots/fights/fights_per_season.jpg"), plot = fights_per_game_by_season_plot)
}
# Creates the regulation-time distribution of fights in one-fight games.
# Saves a season-range-specific histogram with NHL period boundaries marked.

make_fight_time_distribution_plot <- function(
    working.dir,
    pbp_master_used,
    binwidth = 1,
    plot_width = 5,
    plot_height = 3.5,
    fill_color = "grey35"
) {
  
  # Identify the season range represented in the supplied data
  seasons_used <- pbp_master_used %>%
    distinct(season) %>%
    filter(!is.na(season)) %>%
    pull(season) %>%
    as.character()
  
  if (length(seasons_used) == 0) {
    stop("pbp_master_used contains no valid seasons.")
  }
  
  season_numbers <- as.numeric(seasons_used)
  
  if (any(is.na(season_numbers))) {
    stop("At least one season could not be converted to numeric.")
  }
  
  season_folder_name <- paste0(
    min(season_numbers),
    "_",
    max(season_numbers)
  )
  
  output_dir <- file.path(
    working.dir,
    "plots",
    "fights",
    season_folder_name
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load one-fight games and retain the requested seasons
  fight_times <- readRDS(
    file.path(
      working.dir,
      "generated",
      "fights",
      "single_fight_games.rds"
    )
  ) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id)
    ) %>%
    filter(season %in% seasons_used) %>%
    distinct(season, game_id, .keep_all = TRUE) %>%
    filter(
      is.finite(fight_time),
      fight_time >= 0,
      fight_time <= 3600
    ) %>%
    mutate(
      # Keep an exact 60-minute fight inside the final visible histogram bin
      fight_min = pmin(fight_time / 60, 60 - 1e-9)
    )
  
  if (nrow(fight_times) == 0) {
    stop("No one-fight games in the seasons covered by pbp_master_used.")
  }
  
  n_fights <- nrow(fight_times)
  
  # Calculate histogram heights before plotting
  bin_counts <- fight_times %>%
    mutate(
      bin = pmin(
        floor(fight_min / binwidth),
        ceiling(60 / binwidth) - 1
      )
    ) %>%
    count(bin)
  
  y_max <- max(bin_counts$n)
  
  # Keep horizontal gridlines below the period-label area
  y_breaks <- pretty_breaks(n = 5)(c(0, y_max))
  y_breaks <- y_breaks[y_breaks <= y_max]
  
  # Create the fight-time distribution
  p <- ggplot(fight_times, aes(x = fight_min)) +
    geom_histogram(
      binwidth = binwidth,
      boundary = 0,
      closed = "left",
      fill = fill_color,
      color = "white",
      linewidth = 0.15
    ) +
    geom_vline(
      xintercept = c(20, 40),
      color = "grey50",
      linetype = "22",
      linewidth = 0.35
    ) +
    annotate(
      "text",
      x = c(10, 30, 50),
      y = y_max * 1.10,
      label = c("Period 1", "Period 2", "Period 3"),
      size = 3,
      color = "grey30"
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, by = 10),
      expand = c(0, 0.4)
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = comma,
      expand = c(0, 0),
      limits = c(0, y_max * 1.17)
    ) +
    labs(
      x = "Minutes elapsed in game",
      y = "Number of fights"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "grey88",
        linewidth = 0.3
      ),
      axis.line = element_line(
        color = "grey35",
        linewidth = 0.4
      ),
      axis.ticks = element_line(
        color = "grey35",
        linewidth = 0.4
      ),
      axis.ticks.length = unit(3, "pt"),
      axis.title = element_text(
        size = 8.5,
        color = "grey20"
      ),
      axis.text = element_text(
        size = 8,
        color = "grey20"
      ),
      plot.margin = margin(8, 10, 8, 8)
    )
  
  # Save the season-range-specific figure
  file_out <- file.path(
    output_dir,
    paste0(
      "fight_time_distribution_",
      season_folder_name,
      ".jpg"
    )
  )
  
  ggsave(
    filename = file_out,
    plot = p,
    width = plot_width,
    height = plot_height,
    units = "in",
    dpi = 600,
    bg = "white"
  )
  
  # Report the most populated bins and summary statistics
  top_bins <- bin_counts %>%
    arrange(desc(n)) %>%
    slice(1:3) %>%
    mutate(
      txt = paste0(
        "[",
        bin * binwidth,
        ", ",
        (bin + 1) * binwidth,
        ") min: ",
        n
      )
    )
  
  message("Saved: ", file_out)
  message(
    "Largest bins -- ",
    paste(top_bins$txt, collapse = " | ")
  )
  message(
    "Fights plotted: ",
    format(n_fights, big.mark = ","),
    " | median fight time: ",
    sprintf("%.1f", median(fight_times$fight_min)),
    " min"
  )
  message("\nIn LaTeX, use:")
  message(
    "\\includegraphics[width=0.7\\textwidth]{plots/fights/",
    season_folder_name,
    "/fight_time_distribution_",
    season_folder_name,
    ".jpg}"
  )
  
  invisible(
    list(
      plot = p,
      data = fight_times,
      output_path = file_out,
      n_fights = n_fights
    )
  )
}
make_game_fixed_effects_plots <- function(
    working.dir,
    pbp_master_used,
    scale = c("log", "rate"),
    facet_by = c("intradivision", "fight_period", "fight_score_diff_cat", "season"),
    fill_color = "grey35",
    plot_width = 5,
    plot_height = 3.5,
    file_ext = "jpg"
) {
  
  scale <- match.arg(scale)
  facet_by <- match.arg(facet_by)
  
  # Season range from pbp_master_used, mirroring the other loop functions ----
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
  
  season_folder_name <- paste0(min(season_numbers), "_", max(season_numbers))
  
  fe_path <- file.path(
    working.dir,
    "generated",
    "fights",
    season_folder_name,
    paste0("IAT_game_fixed_effects_", season_folder_name, ".rds")
  )
  
  if (!file.exists(fe_path)) {
    stop(
      "Game fixed effects file not found:\n  ", fe_path,
      "\nRe-run fit_IAT_log_model() for this season range to generate it."
    )
  }
  
  fe_data <- readRDS(fe_path)
  
  output_dir <- file.path(
    working.dir, "plots", "fights", season_folder_name, "game_FE"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  season_pretty <- function(x) {
    x <- as.character(x)
    ifelse(
      str_detect(x, "^\\d{8}$"),
      paste0(substr(x, 1, 4), "-", substr(x, 7, 8)),
      x
    )
  }
  
  fmt_int <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  # Plotted quantity and its axis label
  if (scale == "log") {
    fe_data <- fe_data %>% mutate(value = game_fe)
    value_label <- "Estimated game fixed effect"
  } else {
    fe_data <- fe_data %>% mutate(value = game_rate_per_min)
    value_label <- "Implied violent events per minute"
  }
  
  fe_data <- fe_data %>% filter(is.finite(value))
  
  if (nrow(fe_data) == 0) {
    stop("No finite game fixed effects to plot.")
  }
  
  n_games <- nrow(fe_data)
  
  base_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
      axis.line = element_line(color = "grey35", linewidth = 0.4),
      axis.ticks = element_line(color = "grey35", linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.title = element_text(size = 8.5, color = "grey20"),
      axis.text = element_text(size = 8, color = "grey20"),
      strip.text = element_text(size = 8.5, color = "grey20"),
      plot.margin = margin(8, 10, 8, 8)
    )
  
  # ============================================================
  # 1. Distribution of the game fixed effects
  # ============================================================
  
  median_value <- median(fe_data$value)
  
  p_dist <- ggplot(fe_data, aes(x = value)) +
    geom_histogram(
      bins = 50,
      fill = fill_color,
      color = "white",
      linewidth = 0.15
    ) +
    geom_vline(
      xintercept = median_value,
      color = "grey50",
      linetype = "22",
      linewidth = 0.35
    ) +
    scale_x_continuous(breaks = breaks_pretty(n = 6)) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(x = value_label, y = "Number of games") +
    base_theme
  
  file_dist <- file.path(
    output_dir,
    paste0("game_FE_distribution_", season_folder_name, ".", file_ext)
  )
  
  ggsave(
    filename = file_dist, plot = p_dist,
    width = plot_width, height = plot_height,
    units = "in", dpi = 600, bg = "white"
  )
  
  message("Saved: ", file_dist)
  
  # ============================================================
  # 2. Fixed effect against the number of intervals in the game
  # ============================================================
  
  p_precision <- ggplot(fe_data, aes(x = n_iats, y = value)) +
    geom_hline(
      yintercept = median_value,
      color = "grey50",
      linetype = "22",
      linewidth = 0.35
    ) +
    geom_point(
      color = fill_color,
      alpha = 0.30,
      size = 0.7
    ) +
    scale_x_continuous(breaks = breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = breaks_pretty(n = 6)) +
    labs(
      x = "Interarrival times in game",
      y = value_label
    ) +
    base_theme +
    theme(panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3))
  
  file_precision <- file.path(
    output_dir,
    paste0("game_FE_vs_interval_count_", season_folder_name, ".", file_ext)
  )
  
  ggsave(
    filename = file_precision, plot = p_precision,
    width = plot_width, height = plot_height,
    units = "in", dpi = 600, bg = "white"
  )
  
  message("Saved: ", file_precision)
  
  # ============================================================
  # 3. Distribution faceted by a game-level covariate
  # ============================================================
  
  p_facet <- NULL
  
  if (!facet_by %in% names(fe_data)) {
    
    message(
      "Column '", facet_by, "' not found in the saved fixed effects; ",
      "skipping the faceted plot."
    )
    
  } else {
    
    facet_data <- fe_data %>%
      filter(!is.na(.data[[facet_by]]))
    
    # facet_by is a single value for the whole call, so the label rule is
    # chosen once here rather than row by row.
    facet_column <- facet_data[[facet_by]]
    
    facet_data$facet_group <- if (facet_by == "intradivision") {
      if_else(as.numeric(facet_column) == 1, "Intradivision game", "Other game")
    } else if (facet_by == "fight_period") {
      paste("Fight in period", facet_column)
    } else if (facet_by == "season") {
      season_pretty(facet_column)
    } else {
      as.character(facet_column)
    }
    
    group_counts <- facet_data %>%
      count(facet_group, name = "n_group")
    
    facet_data <- facet_data %>%
      left_join(group_counts, by = "facet_group") %>%
      mutate(
        facet_group = paste0(facet_group, " (n = ", fmt_int(n_group), ")")
      )
    
    p_facet <- ggplot(facet_data, aes(x = value)) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 40,
        fill = fill_color,
        color = "white",
        linewidth = 0.15
      ) +
      geom_vline(
        xintercept = median_value,
        color = "grey50",
        linetype = "22",
        linewidth = 0.35
      ) +
      facet_wrap(~ facet_group, ncol = 2) +
      scale_x_continuous(breaks = breaks_pretty(n = 5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(x = value_label, y = "Density") +
      base_theme
    
    n_facets <- n_distinct(facet_data$facet_group)
    facet_height <- plot_height * max(1, ceiling(n_facets / 2) * 0.62)
    
    file_facet <- file.path(
      output_dir,
      paste0("game_FE_by_", facet_by, "_", season_folder_name, ".", file_ext)
    )
    
    ggsave(
      filename = file_facet, plot = p_facet,
      width = plot_width, height = facet_height,
      units = "in", dpi = 600, bg = "white"
    )
    
    message("Saved: ", file_facet)
  }
  
  # ---- Console summary, for the text or a caption -------------------------
  message("")
  message(
    "Game fixed effects (", scale, " scale): ",
    fmt_int(n_games), " games | median ", sprintf("%.4f", median_value),
    " | IQR ", sprintf("%.4f", quantile(fe_data$value, 0.25)),
    " to ", sprintf("%.4f", quantile(fe_data$value, 0.75)),
    " | SD ", sprintf("%.4f", sd(fe_data$value))
  )
  message(
    "Interarrival times per game: median ",
    fmt_int(median(fe_data$n_iats)),
    " (range ", fmt_int(min(fe_data$n_iats)),
    " to ", fmt_int(max(fe_data$n_iats)), ")"
  )
  message("")
  message("In LaTeX, use:")
  message(
    "\\includegraphics[width=0.7\\textwidth]{plots/fights/",
    season_folder_name, "/game_FE/game_FE_distribution_",
    season_folder_name, ".", file_ext, "}"
  )
  
  invisible(list(
    distribution = p_dist,
    precision = p_precision,
    facet = p_facet,
    data = fe_data
  ))
}

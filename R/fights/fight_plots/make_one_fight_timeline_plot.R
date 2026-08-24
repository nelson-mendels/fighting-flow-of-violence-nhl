make_one_fight_timeline_plot <- function(
    working.dir,
    pbp_master,
    game_ids,
    season_ids = NULL,
    panel_labels = NULL,
    output_path = NULL
) {
  
  working.dir <- sub("/+$", "", working.dir)
  game_ids <- as.character(game_ids)
  
  if (is.null(season_ids)) {
    season_ids <- rep(NA_character_, length(game_ids))
  } else {
    season_ids <- as.character(season_ids)
  }
  
  if (is.null(panel_labels)) {
    panel_labels <- paste0("Panel ", seq_along(game_ids))
  }
  
  if (length(season_ids) != length(game_ids)) {
    stop("season_ids must be NULL or the same length as game_ids.")
  }
  
  if (length(panel_labels) != length(game_ids)) {
    stop("panel_labels must be NULL or the same length as game_ids.")
  }
  
  requested_games <- tibble(
    requested_game_id = game_ids,
    season_requested = season_ids,
    panel_label = panel_labels,
    plot_order = seq_along(game_ids)
  )
  
  single_fight <- readRDS(file.path(
    working.dir,
    "generated/fights/single_fight_games.rds"
  )) %>%
    mutate(
      game_id = as.character(game_id),
      season = as.character(season),
      game_date = as.Date(game_date)
    )
  
  if (!"game_id_full" %in% names(single_fight)) {
    single_fight$game_id_full <- single_fight$game_id
  } else {
    single_fight <- single_fight %>%
      mutate(game_id_full = as.character(game_id_full))
  }
  
  single_fight_lookup <- bind_rows(
    single_fight %>% mutate(requested_game_id = game_id),
    single_fight %>% mutate(requested_game_id = game_id_full)
  ) %>%
    filter(!is.na(requested_game_id), requested_game_id != "") %>%
    distinct(requested_game_id, season, game_id, game_id_full, .keep_all = TRUE)
  
  selected_games <- requested_games %>%
    left_join(single_fight_lookup, by = "requested_game_id") %>%
    filter(is.na(season_requested) | season == season_requested) %>%
    group_by(plot_order) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(plot_order)
  
  if (nrow(selected_games) != length(game_ids) || any(is.na(selected_games$fight_time))) {
    print(selected_games)
    stop("At least one requested game was not found in generated/fights/single_fight_games.rds.")
  }
  
  game_meta <- readRDS(file.path(
    working.dir,
    "generated/IAT_data/IAT_game_level_data.rds"
  )) %>%
    mutate(
      game_id = as.character(game_id),
      season = as.character(season),
      game_date_meta = as.Date(game_date)
    )
  
  if (!"game_id_full" %in% names(game_meta)) {
    game_meta$game_id_full <- game_meta$game_id
  } else {
    game_meta <- game_meta %>%
      mutate(game_id_full = as.character(game_id_full))
  }
  
  game_meta <- game_meta %>%
    distinct(
      game_id,
      game_id_full,
      season,
      game_date_meta,
      home_name_meta = home_name,
      away_name_meta = away_name
    )
  
  selected_games <- selected_games %>%
    left_join(
      game_meta,
      by = c("game_id", "game_id_full", "season")
    ) %>%
    mutate(
      game_date = coalesce(as.Date(game_date), game_date_meta),
      home_name = home_name_meta,
      away_name = away_name_meta
    ) %>%
    select(-game_date_meta, -home_name_meta, -away_name_meta)
  
  if (any(is.na(selected_games$home_name)) || any(is.na(selected_games$away_name))) {
    print(
      selected_games %>%
        select(requested_game_id, game_id, game_id_full, season, home_name, away_name)
    )
    stop("Could not find matchup information for at least one selected game.")
  }
  
  selected_game_keys <- selected_games %>%
    select(plot_order, season, game_id, game_id_full) %>%
    pivot_longer(
      cols = c(game_id, game_id_full),
      names_to = "key_type",
      values_to = "game_key"
    ) %>%
    filter(!is.na(game_key), game_key != "") %>%
    distinct(plot_order, season, game_key)
  
  pbp_base <- pbp_master %>%
    mutate(pbp_row_id = row_number())
  
  if (!"game_id_full" %in% names(pbp_base)) {
    pbp_base$game_id_full <- pbp_base$game_id
  }
  
  pbp_base <- pbp_base %>%
    mutate(
      game_id = as.character(game_id),
      game_id_full = as.character(game_id_full),
      season = as.character(season),
      event_type = str_to_upper(as.character(event_type))
    )
  
  pbp_match_game_id <- pbp_base %>%
    inner_join(
      selected_game_keys,
      by = c("season", "game_id" = "game_key")
    )
  
  pbp_match_game_id_full <- pbp_base %>%
    inner_join(
      selected_game_keys,
      by = c("season", "game_id_full" = "game_key")
    )
  
  pbp_selected_raw <- bind_rows(
    pbp_match_game_id,
    pbp_match_game_id_full
  ) %>%
    distinct(pbp_row_id, plot_order, .keep_all = TRUE) %>%
    filter(
      str_sub(coalesce(game_id_full, game_id), 5, 6) == "02",
      period < 4
    )
  
  message("Rows selected before filter_penalties(): ", nrow(pbp_selected_raw))
  
  pbp_selected <- pbp_selected_raw %>%
    filter_penalties()
  
  message("Rows selected after filter_penalties():  ", nrow(pbp_selected))
  
  plot_events <- pbp_selected %>%
    filter(event_type == "VIOLENT_CONTACT") %>%
    left_join(
      selected_games %>%
        select(
          plot_order,
          fight_time,
          panel_label,
          game_date,
          home_name,
          away_name
        ),
      by = "plot_order"
    ) %>%
    arrange(plot_order, game_seconds) %>%
    mutate(
      y = 0,
      game_minutes = game_seconds / 60
    )
  
  message("Violent-contact events used in timeline:")
  print(
    plot_events %>%
      count(plot_order, game_id, panel_label, name = "n_events")
  )
  
  if (nrow(plot_events) == 0) {
    message("Event types after filter_penalties():")
    print(
      pbp_selected %>%
        count(plot_order, game_id, event_type, sort = TRUE)
    )
    
    message("Selected penalties after filter_penalties():")
    print(
      pbp_selected %>%
        filter(event_type %in% c("PENALTY", "VIOLENT_CONTACT")) %>%
        count(plot_order, game_id, event_type, penalty_severity, sort = TRUE)
    )
    
    stop("No VIOLENT_CONTACT events found for selected games.")
  }
  
  fmt_p <- function(p, digits = 3) {
    if (is.na(p)) return("= NA")
    if (p >= 0.999) return("\u2248 1")
    paste0("= ", signif(p, digits))
  }
  
  make_one_panel <- function(i) {
    
    info <- selected_games %>%
      filter(plot_order == i) %>%
      slice(1)
    
    event_df <- plot_events %>%
      filter(plot_order == i)
    
    fight_minute <- info$fight_time / 60
    
    matchup <- paste0(info$away_name, " at ", info$home_name)
    date_formatted <- format(info$game_date, "%B %d, %Y")
    subtitle_text <- paste0(matchup, " | ", date_formatted)
    
    t_before <- info$fight_time
    t_after <- 3600 - info$fight_time
    t_total <- 3600
    
    n_pre <- sum(event_df$game_seconds < info$fight_time)
    n_post <- sum(event_df$game_seconds > info$fight_time)
    n_total <- n_pre + n_post
    
    lambda_before <- ifelse(t_before > 0, n_pre / (t_before / 60), NA_real_)
    lambda_after <- ifelse(t_after > 0, n_post / (t_after / 60), NA_real_)
    
    if (n_total > 0) {
      p0 <- t_before / t_total
      p_lower <- pbinom(n_pre, n_total, p0)
      p_upper <- pbinom(n_pre - 1, n_total, p0, lower.tail = FALSE)
      p_value <- pmin(1, 2 * pmin(p_lower, p_upper))
    } else {
      p_value <- 1
    }
    
    before_text <- paste0(
      "Before fight: ", n_pre, " events, ",
      round(lambda_before, 2), " per minute"
    )
    
    after_text <- paste0(
      "After fight: ", n_post, " events, ",
      round(lambda_after, 2), " per minute",
      " (p ", fmt_p(p_value), ")"
    )
    
    ggplot(event_df, aes(x = game_minutes, y = y)) +
      annotate(
        "segment",
        x = 0,
        xend = 60,
        y = 0,
        yend = 0,
        linewidth = 0.35,
        color = "#2C3E50"
      ) +
      geom_point(
        size = 1.9,
        alpha = 0.80,
        color = "#2C3E50"
      ) +
      geom_segment(
        aes(
          x = fight_minute,
          xend = fight_minute,
          y = -0.09,
          yend = 0.13
        ),
        inherit.aes = FALSE,
        color = "#E74C3C",
        linetype = "dashed",
        linewidth = 0.8
      ) +
      annotate(
        "label",
        x = fight_minute,
        y = 0.15,
        label = "Fight",
        color = "#E74C3C",
        fill = "white",
        label.size = 0.25,
        size = 3.2
      ) +
      annotate(
        "text",
        x = fight_minute / 2,
        y = -0.155,
        label = before_text,
        size = 3.2,
        hjust = 0.5,
        color = "#2C3E50"
      ) +
      annotate(
        "text",
        x = fight_minute + (60 - fight_minute) / 2,
        y = -0.155,
        label = after_text,
        size = 3.2,
        hjust = 0.5,
        color = "#2C3E50"
      ) +
      scale_x_continuous(
        limits = c(0, 60),
        breaks = seq(0, 60, by = 10),
        expand = c(0.01, 0)
      ) +
      scale_y_continuous(
        limits = c(-0.19, 0.19),
        breaks = NULL
      ) +
      labs(
        title = info$panel_label,
        subtitle = subtitle_text,
        x = "Game time in minutes",
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.4),
        axis.line.x = element_line(color = "#2C3E50", linewidth = 0.4),
        axis.ticks.x = element_line(color = "#2C3E50", linewidth = 0.4),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 11, color = "#2C3E50"),
        axis.text.x = element_text(size = 9.5, color = "#2C3E50"),
        plot.title = element_text(face = "bold", size = 13, color = "#2C3E50"),
        plot.subtitle = element_text(size = 9.2, color = "#2C3E50"),
        plot.margin = margin(8, 12, 12, 12)
      )
  }
  
  plot_list <- map(seq_along(game_ids), make_one_panel)
  combined_plot <- wrap_plots(plot_list, ncol = 1)
  
  out_dir <- file.path(working.dir, "plots", "fights")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(output_path)) {
    output_path <- file.path(out_dir, "single_fight_timeline_comparison.png")
  }
  
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  
  ggsave(
    filename = output_path,
    plot = combined_plot,
    width = 11,
    height = 3.25 * length(game_ids),
    units = "in",
    dpi = 300,
    device = "png"
  )
  
  message("Saved plot to: ", output_path)
  
  list(
    plot = combined_plot,
    selected_games = selected_games,
    pbp_selected = pbp_selected,
    plot_events = plot_events,
    output_path = output_path
  )
}
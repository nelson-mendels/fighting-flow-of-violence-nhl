# Plots estimated pre-fight and post-fight violent-event rates for one-fight games.
# Produces full and restricted views at each specified per-side event-count floor.

make_pre_post_lambda_plots <- function(
    working.dir,
    single_fight,
    pbp_master_used,
    floors = c(10, 25),
    zoom_limit = 3,
    min_each_side = 0,
    point_color = "grey35",
    plot_width = 2,
    plot_height = 2,
    file_ext = "jpg",
    label_size = 1.55,
    panel_width = NULL
) {
  
  # Use the plot width when a separate panel width is not supplied
  if (is.null(panel_width)) {
    panel_width <- plot_width
  }
  
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
  
  # Prepare single-fight games with valid pre-fight and post-fight rates
  base_df <- single_fight %>%
    mutate(season = as.character(season)) %>%
    filter(season %in% seasons_used) %>%
    mutate(
      post_less_than_pre =
        lambda_after_VIOLENT_CONTACT < lambda_before_VIOLENT_CONTACT
    ) %>%
    filter(
      is.finite(lambda_before_VIOLENT_CONTACT),
      is.finite(lambda_after_VIOLENT_CONTACT),
      n_before_VIOLENT_CONTACT >= min_each_side,
      n_after_VIOLENT_CONTACT >= min_each_side
    )
  
  if (nrow(base_df) == 0) {
    stop("No single-fight games in the seasons covered by pbp_master_used.")
  }
  
  fmt_int <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  # Create one full or restricted plot for a specified per-side floor
  make_one_plot <- function(
    floor_value,
    view = c("full", "zoom3")
  ) {
    
    view <- match.arg(view)
    
    floor_df <- base_df %>%
      filter(
        n_before_VIOLENT_CONTACT >= floor_value,
        n_after_VIOLENT_CONTACT >= floor_value
      )
    
    if (nrow(floor_df) == 0) {
      warning("No observations for per-side floor = ", floor_value)
      return(NULL)
    }
    
    n_total <- nrow(floor_df)
    n_post_less_pre <- sum(floor_df$post_less_than_pre, na.rm = TRUE)
    pct_post_less_pre <- 100 * n_post_less_pre / n_total
    
    data_max <- max(
      floor_df$lambda_before_VIOLENT_CONTACT,
      floor_df$lambda_after_VIOLENT_CONTACT,
      na.rm = TRUE
    )
    
    if (!is.finite(data_max) || data_max <= 0) {
      data_max <- 1
    }
    
    # Apply equal x- and y-axis limits
    axis_limit <- if (view == "zoom3") {
      zoom_limit
    } else {
      ceiling(data_max + 1)
    }
    
    # Remove observations outside the restricted plotting range
    plot_df <- floor_df %>%
      filter(
        lambda_before_VIOLENT_CONTACT >= 0,
        lambda_after_VIOLENT_CONTACT >= 0,
        lambda_before_VIOLENT_CONTACT <= axis_limit,
        lambda_after_VIOLENT_CONTACT <= axis_limit
      )
    
    # Add padding so boundary points appear as complete circles
    pad <- 0.035 * axis_limit
    span <- axis_limit + 2 * pad
    
    # Construct the statistics-box labels
    line1_expr <- str2expression(
      paste0(
        "n == '",
        fmt_int(n_total),
        " games'"
      )
    )
    
    line2_expr <- str2expression(
      paste0(
        "hat(lambda)[Post] < hat(lambda)[Pre]*':'~'",
        fmt_int(n_post_less_pre),
        " (",
        sprintf("%.1f", pct_post_less_pre),
        "%)'"
      )
    )
    
    # Size the statistics box according to its text and panel width
    line1_chars <- nchar(
      paste0(
        "n = ",
        fmt_int(n_total),
        " games"
      )
    )
    
    line2_chars <- nchar(
      paste0(
        "L_Post < L_Pre: ",
        fmt_int(n_post_less_pre),
        " (",
        sprintf("%.1f", pct_post_less_pre),
        "%)"
      )
    )
    
    max_chars <- max(line1_chars, line2_chars)
    
    panel_draw_mm <- panel_width * 25.4 * 0.74
    data_per_mm <- span / panel_draw_mm
    
    text_w_mm <- max_chars * 0.55 * label_size
    line_h_mm <- 1.45 * label_size
    
    box_w <- min(
      text_w_mm * data_per_mm * 1.10,
      0.92 * span
    )
    
    box_h <- (2 * line_h_mm) * data_per_mm * 1.15
    
    box_x0 <- -pad + 0.055 * span
    box_x1 <- box_x0 + box_w
    box_y1 <- axis_limit + pad - 0.030 * span
    box_y0 <- box_y1 - box_h
    
    line1_y <- box_y1 - box_h * 0.27
    line2_y <- box_y1 - box_h * 0.72
    box_xmid <- (box_x0 + box_x1) / 2
    
    # Create the pre-fight versus post-fight rate plot
    p <- ggplot(
      plot_df,
      aes(
        x = lambda_before_VIOLENT_CONTACT,
        y = lambda_after_VIOLENT_CONTACT
      )
    ) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "grey50",
        linetype = "dashed",
        linewidth = 0.3
      ) +
      geom_point(
        color = point_color,
        alpha = 0.35,
        size = 0.45
      ) +
      annotate(
        "rect",
        xmin = box_x0,
        xmax = box_x1,
        ymin = box_y0,
        ymax = box_y1,
        fill = "white",
        color = "grey15",
        linewidth = 0.15
      ) +
      annotate(
        "text",
        x = box_xmid,
        y = line1_y,
        label = line1_expr,
        size = label_size,
        vjust = 0.5,
        color = "grey15"
      ) +
      annotate(
        "text",
        x = box_xmid,
        y = line2_y,
        label = line2_expr,
        size = label_size,
        vjust = 0.5,
        color = "grey15"
      ) +
      scale_x_continuous(
        breaks = breaks_pretty(n = 5)
      ) +
      scale_y_continuous(
        breaks = breaks_pretty(n = 5)
      ) +
      coord_fixed(
        ratio = 1,
        xlim = c(-pad, axis_limit + pad),
        ylim = c(-pad, axis_limit + pad),
        expand = FALSE,
        clip = "off"
      ) +
      labs(
        x = expression(hat(lambda)[Pre]),
        y = expression(hat(lambda)[Post])
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        axis.title.x = element_text(
          size = 6.5,
          margin = margin(t = 2)
        ),
        axis.title.y = element_text(
          size = 6.5,
          margin = margin(r = 2)
        ),
        axis.text = element_text(
          size = 6,
          color = "grey20"
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          linewidth = 0.2,
          color = "grey85"
        ),
        plot.margin = margin(4, 6, 4, 4)
      )
    
    suffix <- if (view == "zoom3") "zoom3" else "full"
    
    file_out <- file.path(
      output_dir,
      paste0(
        "pre_post_lambda_",
        season_folder_name,
        "_floor",
        floor_value,
        "_",
        suffix,
        ".",
        file_ext
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
    
    message("Saved: ", file_out)
    
    list(
      plot = p,
      floor_df = floor_df,
      plot_df = plot_df,
      output_path = file_out,
      floor = floor_value,
      view = view,
      axis_limit = axis_limit
    )
  }
  
  # Produce full and restricted plots for each per-side floor
  results <- list()
  
  for (f in floors) {
    results[[paste0("floor_", f, "_full")]] <-
      make_one_plot(f, "full")
    
    results[[paste0("floor_", f, "_zoom3")]] <-
      make_one_plot(f, "zoom3")
  }
  
  message("")
  message("Plots saved under: ", output_dir)
  message("In LaTeX, use:")
  message(
    "\\includegraphics[width=0.6\\textwidth]{plots/fights/",
    season_folder_name,
    "/pre_post_lambda_",
    season_folder_name,
    "_floor",
    floors[1],
    "_full.",
    file_ext,
    "}"
  )
  
  invisible(results)
}
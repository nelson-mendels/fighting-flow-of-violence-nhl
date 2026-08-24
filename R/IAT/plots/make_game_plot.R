# Creates an empirical interarrival-time distribution plot for one game and event type.
# Compares the observed distribution with its fitted exponential distribution.

fmt_p_plotmath <- function(p) {
  if (is.na(p)) {
    return("p[AD] == 'NA'")
  }
  
  if (p > 0.99) {
    return("p[AD] %~~% 1")
  }
  
  if (p >= 0.01) {
    return(paste0("p[AD] == '", sprintf("%.2f", p), "'"))
  }
  
  e <- floor(log10(p))
  m <- p / 10^e
  
  paste0(
    "p[AD] == '",
    sprintf("%.1f", m),
    "' %*% 10^{",
    e,
    "}"
  )
}


game_plot <- function(
    working.dir,
    master,
    season,
    event,
    lambda_hat = TRUE
) {
  
  if (nrow(master) == 0) {
    message(paste("No data for", season, event))
    return(NULL)
  }
  
  # Prepare event times for interarrival analysis
  filtered_data <- master %>%
    arrange(game_id, game_seconds) %>%
    mutate(
      game_num = dense_rank(paste(game_date, game_id)),
      season_seconds = game_seconds + ((game_num - 1) * 3600)
    ) %>%
    arrange(season_seconds)
  
  IAT_stats <- calculate_IAT(filtered_data)
  
  if (length(IAT_stats$IAT_values) == 0) {
    message(paste("No IAT values for", season, event))
    return(NULL)
  }
  
  df_IAT <- tibble(IAT = IAT_stats$IAT_values)
  
  # Set axis range and fitted exponential curve
  xmin <- 0
  xmax <- ceiling(max(IAT_stats$IAT_values, na.rm = TRUE))
  xmax <- max(xmax, 2)
  ticks <- ifelse(xmax > 10, 2, 1)
  
  x_vals <- seq(xmin, xmax, length.out = 300)
  
  exp_curve <- tibble(
    x = x_vals,
    y = pexp(x_vals, rate = as.numeric(IAT_stats$lambda))
  )
  
  # Position the statistics box
  x_right <- xmax * 0.98
  x_left <- x_right - 0.22 * xmax
  x_text <- x_left + 0.015 * xmax
  
  y_top <- 0.36
  y_step <- 0.068
  y_vals <- c(
    y_top,
    y_top - y_step,
    y_top - 2 * y_step
  )
  
  # Construct plotmath labels
  n_label <- paste0("n == ", IAT_stats$n)
  
  lambda_label <- paste0(
    if (lambda_hat) "hat(lambda) == '" else "lambda == '",
    signif(IAT_stats$lambda, 2),
    "'"
  )
  
  adp_label <- fmt_p_plotmath(IAT_stats$ad_raw_p_value)
  
  event_words <- if (event == "VIOLENT_CONTACT") {
    "violent"
  } else {
    tolower(gsub("_", " ", event))
  }
  
  x_label <- paste0(
    "Time in minutes between ",
    event_words,
    " events"
  )
  
  # Create empirical and theoretical distribution plot
  p <- ggplot(df_IAT, aes(x = IAT)) +
    stat_ecdf(
      geom = "step",
      color = "#2C3E50",
      linewidth = 0.9
    ) +
    geom_point(
      stat = "ecdf",
      pad = FALSE,
      aes(alpha = after_stat(ifelse(y > 0, 1, 0))),
      shape = 21,
      fill = "white",
      color = "#2C3E50",
      size = 1.2,
      stroke = 0.5
    ) +
    scale_alpha_identity() +
    geom_line(
      data = exp_curve,
      aes(x = x, y = y),
      color = "#E74C3C",
      linetype = "dashed",
      linewidth = 0.85
    ) +
    annotate(
      "rect",
      xmin = x_left,
      xmax = x_right,
      ymin = y_vals[3] - 0.050,
      ymax = y_vals[1] + 0.055,
      color = "#2C3E50",
      fill = "white",
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = x_text,
      y = y_vals[1],
      hjust = 0,
      label = n_label,
      parse = TRUE,
      size = 3.2,
      color = "#2C3E50"
    ) +
    annotate(
      "text",
      x = x_text,
      y = y_vals[2],
      hjust = 0,
      label = lambda_label,
      parse = TRUE,
      size = 3.2,
      color = "#2C3E50"
    ) +
    annotate(
      "text",
      x = x_text,
      y = y_vals[3],
      hjust = 0,
      label = adp_label,
      parse = TRUE,
      size = 3.2,
      color = "#2C3E50"
    ) +
    scale_x_continuous(
      expand = c(0, 0.3),
      breaks = seq(xmin, xmax, by = ticks)
    ) +
    scale_y_continuous(
      expand = c(0, 0.02),
      breaks = seq(0, 1, by = 0.25),
      labels = c("0", "0.25", "0.50", "0.75", "1")
    ) +
    coord_cartesian(
      xlim = c(xmin, xmax),
      ylim = c(0, 1)
    ) +
    labs(
      x = x_label,
      y = "Cumulative probability"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.4
      ),
      axis.line = element_line(
        color = "#2C3E50",
        linewidth = 0.4
      ),
      axis.ticks = element_line(
        color = "#2C3E50",
        linewidth = 0.4
      ),
      axis.ticks.length = unit(3, "pt"),
      axis.title = element_text(
        size = 10,
        color = "#2C3E50"
      ),
      axis.text = element_text(
        size = 9,
        color = "#2C3E50"
      ),
      plot.margin = margin(10, 15, 10, 10)
    )
  
  return(
    list(
      plot = p,
      n = IAT_stats$n,
      lambda = IAT_stats$lambda,
      ad_p_value = IAT_stats$ad_raw_p_value,
      IAT = IAT_stats$IAT_values
    )
  )
}

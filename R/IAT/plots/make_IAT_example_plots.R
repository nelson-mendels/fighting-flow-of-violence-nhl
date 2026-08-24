# Creates the two game-level interarrival distribution plots used in the paper.
# It selects representative exponential and non-exponential games, saves both plots,
# and generates a corresponding LaTeX figure note.

make_IAT_example_plots <- function(
    working.dir,
    pbp_master,
    exponential_game_id = NULL,
    exponential_season = NULL,
    non_exponential_game_id = NULL,
    non_exponential_season = NULL,
    event_name = "VIOLENT_CONTACT",
    n_min = 50,
    n_max_quantile = 0.95,
    game_level_rds = file.path(
      working.dir,
      "generated/IAT_data/IAT_game_level_data.rds"
    ),
    plot_width = 8,
    plot_height = 3.5,
    overwrite = TRUE
) {
  
  # Set output paths
  out_dir <- file.path(working.dir, "plots", "VIOLENT_CONTACT", "paper")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  path_exp <- file.path(out_dir, "IAT_example_exponential.png")
  path_non_exp <- file.path(out_dir, "IAT_example_non_exponential.png")
  path_note <- file.path(out_dir, "IAT_example_note.tex")
  
  if (!overwrite && file.exists(path_exp) && file.exists(path_non_exp)) {
    message(
      "Both IAT example plots already exist — skipping. ",
      "Set overwrite = TRUE to regenerate."
    )
    return(invisible(c(path_exp, path_non_exp)))
  }
  
  # Identify games with event counts typical of the sample
  game_level <- readRDS(game_level_rds) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id)
    ) %>%
    filter(
      event == event_name,
      !is.na(n),
      !is.na(ad_p_value)
    )
  
  n_median <- median(game_level$n)
  n_max <- quantile(
    game_level$n,
    n_max_quantile,
    na.rm = TRUE
  )
  
  eligible <- game_level %>%
    filter(
      n >= n_min,
      n <= n_max
    )
  
  message(
    "Eligible example games (",
    n_min,
    " <= n <= ",
    n_max,
    ", the ",
    round(100 * n_max_quantile),
    "th percentile): ",
    format(nrow(eligible), big.mark = ",")
  )
  
  if (nrow(eligible) == 0) {
    stop(
      "No eligible games in the [n_min, n_max] band. ",
      "Loosen n_min or n_max_quantile."
    )
  }
  
  # Select the game with the largest AD p-value when none is supplied
  if (is.null(exponential_game_id)) {
    exp_pick <- eligible %>%
      arrange(desc(ad_p_value)) %>%
      slice(1)
    
    exponential_game_id <- exp_pick$game_id
    exponential_season <- exp_pick$season
    
    message(
      "Auto-selected exponential example candidates ",
      "(top 5 by AD p-value):"
    )
    
    print(
      eligible %>%
        arrange(desc(ad_p_value)) %>%
        select(season, game_id, n, ad_p_value) %>%
        head(5)
    )
  }
  
  # Select the game with the smallest AD p-value when none is supplied
  if (is.null(non_exponential_game_id)) {
    non_exp_pick <- eligible %>%
      arrange(ad_p_value) %>%
      slice(1)
    
    non_exponential_game_id <- non_exp_pick$game_id
    non_exponential_season <- non_exp_pick$season
    
    message(
      "Auto-selected non-exponential example candidates ",
      "(top 5 by smallest AD p-value):"
    )
    
    print(
      eligible %>%
        arrange(ad_p_value) %>%
        select(season, game_id, n, ad_p_value) %>%
        head(5)
    )
  }
  
  # Retrieve game-level information for the selected examples
  lookup_game <- function(gid, ssn) {
    game_level %>%
      filter(
        game_id == as.character(gid),
        season == as.character(ssn)
      ) %>%
      slice(1)
  }
  
  exp_info <- lookup_game(
    exponential_game_id,
    exponential_season
  )
  
  non_exp_info <- lookup_game(
    non_exponential_game_id,
    non_exponential_season
  )
  
  if (nrow(exp_info) == 0 || nrow(non_exp_info) == 0) {
    stop(
      "Could not find one of the requested games ",
      "in the game-level AD data."
    )
  }
  
  # Warn when a supplied example is outside the typical event-count range
  for (info in list(exp_info, non_exp_info)) {
    if (info$n > n_max) {
      warning(
        "Example game ",
        info$game_id,
        " (season ",
        info$season,
        ") has n = ",
        info$n,
        ", above the ",
        round(100 * n_max_quantile),
        "th percentile of ",
        n_max,
        ". A referee comparing this figure with the season summary ",
        "table may object."
      )
    }
  }
  
  message(
    "Exponential example: ",
    exponential_game_id,
    " (n = ",
    exp_info$n,
    ", AD p = ",
    signif(exp_info$ad_p_value, 2),
    ")"
  )
  
  message(
    "Non-exponential example: ",
    non_exponential_game_id,
    " (n = ",
    non_exp_info$n,
    ", AD p = ",
    signif(non_exp_info$ad_p_value, 2),
    ")"
  )
  
  # Prepare one selected game for plotting
  prep_game <- function(pbp, gid, ssn) {
    pbp %>%
      mutate(
        season = as.character(season),
        game_id = as.character(game_id),
        game_id_full = as.character(game_id_full),
        event_type = str_to_upper(as.character(event_type))
      ) %>%
      filter(
        season == ssn,
        game_id_full == gid | game_id == gid
      ) %>%
      filter_penalties() %>%
      filter(event_type == "VIOLENT_CONTACT")
  }
  
  # Build the exponential example
  message("Building exponential example: ", exponential_game_id)
  
  exp_data <- prep_game(
    pbp_master,
    exponential_game_id,
    as.character(exponential_season)
  )
  
  exp_result <- game_plot(
    working.dir = working.dir,
    master = exp_data,
    season = as.character(exponential_season),
    event = "VIOLENT_CONTACT"
  )
  
  if (is.null(exp_result)) {
    stop(
      "No data found for exponential game: ",
      exponential_game_id
    )
  }
  
  p_exp <- exp_result$plot
  
  # Build the non-exponential example
  message(
    "Building non-exponential example: ",
    non_exponential_game_id
  )
  
  non_exp_data <- prep_game(
    pbp_master,
    non_exponential_game_id,
    as.character(non_exponential_season)
  )
  
  non_exp_result <- game_plot(
    working.dir = working.dir,
    master = non_exp_data,
    season = as.character(non_exponential_season),
    event = "VIOLENT_CONTACT"
  )
  
  if (is.null(non_exp_result)) {
    stop(
      "No data found for non-exponential game: ",
      non_exponential_game_id
    )
  }
  
  p_non_exp <- non_exp_result$plot
  
  # Apply a shared x-axis based on the largest observed interarrival time
  shared_xmax <- ceiling(
    max(
      c(exp_result$IAT, non_exp_result$IAT),
      na.rm = TRUE
    )
  )
  
  shared_xmax <- max(shared_xmax, 2)
  ticks <- ifelse(shared_xmax > 10, 2, 1)
  
  shared_x_scale <- scale_x_continuous(
    expand = c(0, 0.3),
    breaks = seq(0, shared_xmax, by = ticks)
  )
  
  p_exp <- p_exp +
    shared_x_scale +
    coord_cartesian(
      xlim = c(0, shared_xmax),
      ylim = c(0, 1)
    )
  
  p_non_exp <- p_non_exp +
    shared_x_scale +
    coord_cartesian(
      xlim = c(0, shared_xmax),
      ylim = c(0, 1)
    )
  
  # Extend the fitted exponential curve to the shared axis maximum
  extend_exp_curve <- function(p, lambda, new_xmax) {
    for (i in seq_along(p$layers)) {
      df <- p$layers[[i]]$data
      
      if (
        !is.null(df) &&
        is.data.frame(df) &&
        "x" %in% names(df) &&
        "y" %in% names(df) &&
        nrow(df) > 10
      ) {
        new_x <- seq(
          0,
          new_xmax,
          length.out = 500
        )
        
        p$layers[[i]]$data <- data.frame(
          x = new_x,
          y = pexp(
            new_x,
            rate = as.numeric(lambda)
          )
        )
        
        return(p)
      }
    }
    
    p
  }
  
  # Reposition the statistics box after changing the x-axis
  reposition_stats_box <- function(p, new_xmax) {
    x_right <- new_xmax * 0.98
    x_left <- x_right - 0.22 * new_xmax
    x_text <- x_left + 0.015 * new_xmax
    
    for (i in seq_along(p$layers)) {
      df <- p$layers[[i]]$data
      
      if (
        is.null(df) ||
        !is.data.frame(df) ||
        nrow(df) != 1
      ) {
        next
      }
      
      if (
        inherits(p$layers[[i]]$geom, "GeomRect") &&
        all(c("xmin", "xmax") %in% names(df))
      ) {
        p$layers[[i]]$data$xmin <- x_left
        p$layers[[i]]$data$xmax <- x_right
      }
      
      if (
        inherits(p$layers[[i]]$geom, "GeomText") &&
        "x" %in% names(df)
      ) {
        p$layers[[i]]$data$x <- x_text
      }
    }
    
    p
  }
  
  p_exp <- extend_exp_curve(
    p_exp,
    exp_result$lambda,
    shared_xmax
  )
  
  p_exp <- reposition_stats_box(
    p_exp,
    shared_xmax
  )
  
  p_non_exp <- extend_exp_curve(
    p_non_exp,
    non_exp_result$lambda,
    shared_xmax
  )
  
  p_non_exp <- reposition_stats_box(
    p_non_exp,
    shared_xmax
  )
  
  # Save both paper figures
  ggsave(
    filename = path_exp,
    plot = p_exp,
    width = plot_width,
    height = plot_height,
    dpi = 300,
    device = "png"
  )
  
  if (!file.exists(path_exp)) {
    stop(
      "Exponential plot did not save. Expected: ",
      path_exp
    )
  }
  
  message(
    "Saved: plots/VIOLENT_CONTACT/paper/",
    "IAT_example_exponential.png"
  )
  
  ggsave(
    filename = path_non_exp,
    plot = p_non_exp,
    width = plot_width,
    height = plot_height,
    dpi = 300,
    device = "png"
  )
  
  if (!file.exists(path_non_exp)) {
    stop(
      "Non-exponential plot did not save. Expected: ",
      path_non_exp
    )
  }
  
  message(
    "Saved: plots/VIOLENT_CONTACT/paper/",
    "IAT_example_non_exponential.png"
  )
  
  # Format AD p-values for the LaTeX figure note
  fmt_p_tex <- function(p) {
    if (is.na(p)) {
      "\\text{ = NA}"
    } else if (p > 0.99) {
      "\\approx 1"
    } else if (p >= 0.01) {
      paste0("= ", sprintf("%.2f", p))
    } else {
      e <- floor(log10(p))
      m <- p / 10^e
      
      paste0(
        "= ",
        sprintf("%.1f", m),
        " \\times 10^{",
        e,
        "}"
      )
    }
  }
  
  # Retrieve team names and dates for the LaTeX figure note
  describe_game <- function(pbp, gid, ssn) {
    rows <- pbp %>%
      mutate(
        season = as.character(season),
        game_id = as.character(game_id)
      ) %>%
      filter(
        season == as.character(ssn),
        game_id == as.character(gid)
      )
    
    team_cols <- list(
      c("home_team", "away_team"),
      c("home_name", "away_name"),
      c("home_team_name", "away_team_name")
    )
    
    teams <- "[HOME TEAM] vs. [AWAY TEAM]"
    
    for (cols in team_cols) {
      if (
        all(cols %in% names(rows)) &&
        nrow(rows) > 0
      ) {
        teams <- paste0(
          rows[[cols[1]]][1],
          " vs. ",
          rows[[cols[2]]][1]
        )
        break
      }
    }
    
    date_cols <- c(
      "game_date",
      "date",
      "game_day"
    )
    
    game_date <- "[DATE]"
    
    for (col in date_cols) {
      if (
        col %in% names(rows) &&
        nrow(rows) > 0
      ) {
        parsed <- suppressWarnings(
          as.Date(rows[[col]][1])
        )
        
        if (!is.na(parsed)) {
          game_date <- format(
            parsed,
            "%B %e, %Y"
          )
          
          game_date <- gsub(
            "  ",
            " ",
            game_date
          )
          
          break
        }
      }
    }
    
    if (
      grepl("\\[", teams) ||
      grepl("\\[", game_date)
    ) {
      message(
        "NOTE: could not find team/date columns for game ",
        gid,
        " — placeholders written to the figure note. Fill these in ",
        "or provide the relevant column names."
      )
    }
    
    paste0(
      teams,
      " (",
      game_date,
      ")"
    )
  }
  
  exp_desc <- describe_game(
    pbp_master,
    exponential_game_id,
    exponential_season
  )
  
  non_exp_desc <- describe_game(
    pbp_master,
    non_exponential_game_id,
    non_exponential_season
  )
  
  # Generate the paper figure note from the selected games
  note_lines <- c(
    "\\parbox{\\textwidth}{",
    "\\footnotesize",
    paste0(
      "\\textit{Note:} the step functions in black denote empirical ",
      "distribution functions and dashed red lines are the fitted ",
      "exponential distribution functions. The upper panel shows ",
      exp_desc,
      ", a game consistent with exponential interarrival times ",
      "($p_{AD} ",
      fmt_p_tex(exp_info$ad_p_value),
      "$). The lower panel shows ",
      non_exp_desc,
      ", a game inconsistent with exponential interarrival times ",
      "($p_{AD} ",
      fmt_p_tex(non_exp_info$ad_p_value),
      "$). Both games have violent event counts typical of the sample ",
      "($n = ",
      exp_info$n,
      "$ and $n = ",
      non_exp_info$n,
      "$, against a sample median of ",
      round(n_median),
      " and a 95th percentile of ",
      round(n_max),
      ")."
    ),
    "}"
  )
  
  writeLines(
    note_lines,
    path_note
  )
  
  message(
    "Saved: plots/VIOLENT_CONTACT/paper/",
    "IAT_example_note.tex"
  )
  
  message("\nIn LaTeX, use:")
  message(
    "\\includegraphics[width=0.85\\textwidth]",
    "{plots/VIOLENT_CONTACT/paper/IAT_example_exponential.png}"
  )
  message(
    "\\includegraphics[width=0.85\\textwidth]",
    "{plots/VIOLENT_CONTACT/paper/IAT_example_non_exponential.png}"
  )
  message(
    "\\input{plots/VIOLENT_CONTACT/paper/IAT_example_note.tex}"
  )
  
  invisible(
    list(
      paths = c(
        path_exp,
        path_non_exp,
        path_note
      ),
      exponential = exp_info,
      non_exponential = non_exp_info,
      n_median = n_median,
      n_max = n_max
    )
  )
}
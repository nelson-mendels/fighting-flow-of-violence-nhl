# Examines the relationship between fight frequency and rejection of exponential
# interarrival times across event-count cutoffs, producing plots and chi-squared tables.

make_fight_vs_AD_rejection_plots <- function(
    working.dir,
    game_and_fight,
    pbp_master_used,
    event_used = "VIOLENT_CONTACT",
    cutoffs = c(0, 10, 20, 30, 40, 50, 60, 70),
    alpha = 0.05,
    plot_width = 5,
    plot_height = 3.5
) {
  
  # Identify the season range represented in the supplied play-by-play data
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
  
  first_season <- min(season_numbers)
  last_season <- max(season_numbers)
  season_folder_name <- paste0(first_season, "_", last_season)
  
  season_pretty <- function(x) {
    x <- as.character(x)
    
    if (str_detect(x, "^\\d{8}$")) {
      paste0(substr(x, 1, 4), "--", substr(x, 7, 8))
    } else {
      x
    }
  }
  
  season_range_text <- if (first_season == last_season) {
    paste0("the ", season_pretty(first_season), " regular season")
  } else {
    paste0(
      "the ", season_pretty(first_season),
      " through ", season_pretty(last_season),
      " regular seasons"
    )
  }
  
  # Create season-specific output directories
  plot_dir <- file.path(
    working.dir,
    "plots",
    "fights",
    season_folder_name
  )
  
  table_dir <- file.path(
    working.dir,
    "generated",
    "tables",
    season_folder_name
  )
  
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Prepare fight categories and Anderson–Darling test outcomes
  base_data <- game_and_fight %>%
    mutate(season = as.character(season)) %>%
    filter(
      season %in% seasons_used,
      event == event_used,
      !is.na(ad_p_value),
      !is.na(n_fights),
      !is.na(n)
    ) %>%
    mutate(
      ad_result = ifelse(
        ad_p_value <= alpha,
        "Reject",
        "Do not reject"
      ),
      ad_result = factor(
        ad_result,
        levels = c("Do not reject", "Reject")
      ),
      fights_cat = case_when(
        n_fights == 0 ~ "0",
        n_fights == 1 ~ "1",
        n_fights == 2 ~ "2",
        n_fights >= 3 ~ "3+"
      ),
      fights_cat = factor(
        fights_cat,
        levels = c("0", "1", "2", "3+")
      )
    )
  
  plots <- list()
  cutoff_summaries <- list()
  chi_squared_results <- list()
  
  # Calculate rejection rates and chi-squared tests at each cutoff
  for (cutoff in cutoffs) {
    chi_squared_ready <- base_data %>%
      filter(n >= cutoff)
    
    n_games_total <- nrow(chi_squared_ready)
    
    message(
      season_folder_name,
      " | cutoff n >= ",
      cutoff,
      ": ",
      format(n_games_total, big.mark = ","),
      " games."
    )
    
    if (n_games_total == 0) {
      warning("No games left at cutoff ", cutoff, ". Skipping.")
      next
    }
    
    # Test independence between fight count and AD rejection
    tbl <- table(
      droplevels(chi_squared_ready$fights_cat),
      droplevels(chi_squared_ready$ad_result)
    )
    
    chi_result <- tryCatch(
      suppressWarnings(chisq.test(tbl)),
      error = function(e) NULL
    )
    
    chi_squared_results[[as.character(cutoff)]] <- tibble(
      cutoff = cutoff,
      n_games = n_games_total,
      chi_squared = if (is.null(chi_result)) {
        NA_real_
      } else {
        unname(chi_result$statistic)
      },
      df = if (is.null(chi_result)) {
        NA_real_
      } else {
        unname(chi_result$parameter)
      },
      p_value = if (is.null(chi_result)) {
        NA_real_
      } else {
        chi_result$p.value
      },
      min_expected_count = if (is.null(chi_result)) {
        NA_real_
      } else {
        min(chi_result$expected)
      }
    )
    
    # Calculate the AD rejection rate for each fight category
    plot_reject_rate <- chi_squared_ready %>%
      group_by(fights_cat) %>%
      summarise(
        n_games = n(),
        n_reject = sum(ad_result == "Reject"),
        reject_rate = n_reject / n_games,
        .groups = "drop"
      ) %>%
      mutate(
        cutoff = cutoff,
        label = paste0(
          percent(reject_rate, accuracy = 1),
          "\n",
          "n = ",
          comma(n_games)
        )
      )
    
    cutoff_summaries[[as.character(cutoff)]] <- plot_reject_rate
    
    # Leave sufficient vertical space for labels above the bars
    y_upper <- max(plot_reject_rate$reject_rate) * 1.12 + 0.10
    
    p <- ggplot(
      plot_reject_rate,
      aes(x = fights_cat, y = reject_rate)
    ) +
      geom_col(
        width = 0.62,
        fill = "grey35"
      ) +
      geom_text(
        aes(label = label),
        vjust = -0.30,
        size = 3.1,
        lineheight = 0.92
      ) +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        breaks = seq(0, 1, by = 0.25),
        limits = c(0, y_upper),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = "Fights in game",
        y = "AD rejection rate"
      ) +
      theme_classic(base_size = 11) +
      theme(
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10, color = "grey20"),
        panel.grid.major.y = element_line(
          color = "grey88",
          linewidth = 0.3
        ),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    plots[[as.character(cutoff)]] <- p
    
    ggsave(
      filename = file.path(
        plot_dir,
        paste0(
          "fight_vs_AD_rejection_",
          season_folder_name,
          "_cutoff_",
          cutoff,
          ".png"
        )
      ),
      plot = p,
      width = plot_width,
      height = plot_height,
      dpi = 300
    )
  }
  
  summary_by_cutoff <- bind_rows(cutoff_summaries)
  chi_squared_by_cutoff <- bind_rows(chi_squared_results)
  
  # Format the chi-squared results for LaTeX
  fmt_chi <- function(x) {
    ifelse(
      is.na(x),
      "--",
      format(
        round(x, 1),
        big.mark = ",",
        nsmall = 1,
        trim = TRUE
      )
    )
  }
  
  fmt_p <- function(x) {
    case_when(
      is.na(x) ~ "--",
      x < 0.001 ~ "$<0.001$",
      TRUE ~ paste0("$", sprintf("%.3f", x), "$")
    )
  }
  
  chi_table_rows <- chi_squared_by_cutoff %>%
    mutate(
      row = paste0(
        "$n \\geq ",
        cutoff,
        "$ & ",
        format(n_games, big.mark = ",", trim = TRUE),
        " & ",
        fmt_chi(chi_squared),
        " & ",
        fmt_p(p_value),
        " \\\\"
      )
    ) %>%
    pull(row)
  
  df_used <- unique(na.omit(chi_squared_by_cutoff$df))
  
  df_text <- if (length(df_used) == 1) {
    paste0("All tests have ", df_used, " degrees of freedom. ")
  } else if (length(df_used) > 1) {
    paste0(
      "Tests have ",
      paste(sort(df_used), collapse = ", "),
      " degrees of freedom depending on the fight categories present ",
      "at each cutoff. "
    )
  } else {
    ""
  }
  
  # Generate the LaTeX chi-squared table
  chi_latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0(
      "\\caption{Fights and Anderson--Darling Rejections Across ",
      "Event-Count Cutoffs, ",
      season_pretty(first_season),
      " to ",
      season_pretty(last_season),
      "}"
    ),
    paste0(
      "\\label{tab:fight_vs_AD_rejection_chi_squared_",
      season_folder_name,
      "}"
    ),
    "\\small",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{6pt}",
    "",
    "\\begin{tabular*}{0.85\\textwidth}{@{\\extracolsep{\\fill}}lccc}",
    "\\toprule",
    "Sample & Games & $\\chi^2$ & $p$-value \\\\",
    "\\midrule",
    chi_table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.85\\textwidth}{",
    "\\footnotesize",
    paste0(
      "\\textit{Note:} Each row reports a Pearson chi-squared test ",
      "of independence between the number of fights in a game ",
      "(0, 1, 2, 3+) and whether the Anderson--Darling test rejects ",
      "exponentiality of that game's interarrival times at the ",
      sprintf("%.2f", alpha),
      " level, for games during ",
      season_range_text,
      " with at least the indicated number of violent contact events. ",
      df_text
    ),
    "}",
    "\\end{table}"
  )
  
  chi_table_file <- paste0(
    "fight_vs_AD_rejection_chi_squared_",
    season_folder_name,
    ".tex"
  )
  
  writeLines(
    chi_latex_lines,
    file.path(table_dir, chi_table_file)
  )
  
  message("")
  message(
    "Saved plots to: plots/fights/",
    season_folder_name,
    "/fight_vs_AD_rejection_",
    season_folder_name,
    "_cutoff_{cutoff}.png"
  )
  message(
    "Saved: generated/tables/",
    season_folder_name,
    "/",
    chi_table_file
  )
  message("In Overleaf, use:")
  message(
    "\\input{tables/",
    season_folder_name,
    "/",
    chi_table_file,
    "}"
  )
  
  print(chi_squared_by_cutoff)
  
  return(
    list(
      plots = plots,
      summary_by_cutoff = summary_by_cutoff,
      chi_squared_by_cutoff = chi_squared_by_cutoff
    )
  )
}
make_AD_rejection_table <- function(working.dir,
                                    game_data,
                                    data_coverage,
                                    event_used = "VIOLENT_CONTACT",
                                    cutoff = 50,
                                    alpha = 0.05,
                                    range_word = "through",
                                    table_file = "AD_rejection_rates.tex") {
  
  table_dir <- file.path(working.dir, "generated", "tables")
  if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)
  
  season_pretty <- function(x) {
    x <- as.character(x)
    ifelse(
      str_detect(x, "^\\d{8}$"),
      paste0(substr(x, 1, 4), "--", substr(x, 7, 8)),
      x
    )
  }
  
  group_label <- function(seasons) {
    seasons <- sort(unique(as.character(seasons)))
    k <- length(seasons)
    if (k == 1) {
      season_pretty(seasons)
    } else {
      paste0(
        season_pretty(seasons[1]), " ", range_word, " ", season_pretty(seasons[k])
      )
    }
  }
  
  base_data <- game_data %>%
    mutate(season = as.character(season)) %>%
    filter(
      event == event_used,
      !is.na(ad_p_value),
      !is.na(n)
    ) %>%
    mutate(reject = as.integer(ad_p_value <= alpha))
  
  if (nrow(base_data) == 0) {
    stop("No games left after filtering to event '", event_used, "'.")
  }
  
  groups <- lapply(data_coverage, function(df) {
    seasons <- df %>%
      mutate(season = as.character(season)) %>%
      filter(!is.na(season)) %>%
      distinct(season) %>%
      pull(season)
    if (length(seasons) == 0) {
      stop("An element of data_coverage contains no valid seasons.")
    }
    list(label = group_label(seasons), seasons = seasons, indent = FALSE)
  })
  
  # Nesting display: if exactly one group's seasons contain every other
  # group's seasons, list that overall sample first and indent the
  # sub-samples beneath it. If no group nests the others (or several tie),
  # keep the order of data_coverage with no indentation.
  season_sets <- lapply(groups, function(g) unique(g$seasons))
  
  is_superset_of_all <- vapply(seq_along(season_sets), function(i) {
    others <- season_sets[-i]
    length(others) == 0 ||
      all(vapply(others, function(s) all(s %in% season_sets[[i]]), logical(1)))
  }, logical(1))
  
  if (sum(is_superset_of_all) == 1 && length(groups) > 1) {
    overall_idx <- which(is_superset_of_all)
    sub_idx <- setdiff(seq_along(groups), overall_idx)
    groups <- c(groups[overall_idx], groups[sub_idx])
    for (j in seq_along(groups)[-1]) {
      groups[[j]]$indent <- TRUE
      groups[[j]]$label <- paste0("\\quad ", groups[[j]]$label)
    }
  }
  
  summarize_panel <- function(data, panel_name) {
    map_dfr(groups, function(g) {
      rows <- data %>% filter(season %in% g$seasons)
      tibble(
        panel = panel_name,
        seasons = g$label,
        n_games = nrow(rows),
        n_reject = sum(rows$reject),
        reject_rate = if (nrow(rows) > 0) mean(rows$reject) else NA_real_
      )
    })
  }
  
  panel_A <- summarize_panel(base_data, "All games")
  panel_B <- summarize_panel(
    base_data %>% filter(n >= cutoff),
    paste0("Games with at least ", cutoff, " violent events")
  )
  
  summary_table <- bind_rows(panel_A, panel_B)
  
  fmt_rate <- function(r) {
    ifelse(is.na(r), "--", paste0(sprintf("%.1f", 100 * r), "\\%"))
  }
  fmt_games <- function(x) format(x, big.mark = ",", trim = TRUE)
  
  panel_rows <- function(panel_df, panel_title) {
    c(
      paste0(
        "\\multicolumn{3}{l}{\\textit{", panel_title, "}} \\\\"
      ),
      "\\addlinespace[2pt]",
      paste0(
        panel_df$seasons, " & ",
        fmt_games(panel_df$n_games), " & ",
        fmt_rate(panel_df$reject_rate),
        " \\\\"
      )
    )
  }
  
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Anderson--Darling rejection rates across NHL seasons}",
    "\\label{tab:AD_rejection_rates}",
    "\\small",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{6pt}",
    "",
    "\\begin{tabular*}{0.85\\textwidth}{@{\\extracolsep{\\fill}}lrc}",
    "\\toprule",
    "Seasons & Games & A--D rejection rate \\\\",
    "\\midrule",
    panel_rows(panel_A, "All games"),
    "\\midrule",
    panel_rows(
      panel_B,
      paste0("Games with at least ", cutoff, " violent events")
    ),
    "\\bottomrule",
    "\\end{tabular*}",
    "\\end{table}"
  )
  
  table_path <- file.path(table_dir, table_file)
  writeLines(latex_lines, table_path)
  
  message("Saved: generated/tables/", table_file)
  message("In Overleaf, use:")
  message("\\input{tables/", table_file, "}")
  
  print(summary_table)
  
  invisible(list(
    summary_table = summary_table,
    table_path = table_path
  ))
}
make_post_fight_rate_ratio_tables <- function(working.dir, pbp_master_used) {
  
  # Derive season range from pbp_master_used, mirroring fit_IAT_log_model ---
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
    ifelse(
      str_detect(x, "^\\d{8}$"),
      paste0(substr(x, 1, 4), "--", substr(x, 7, 8)),
      x
    )
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
  
  # Read model results from the season-range folder -------------------------
  results_folder <- file.path(
    working.dir,
    "generated",
    "fights",
    season_folder_name
  )
  
  coef_results <- readRDS(file.path(
    results_folder,
    paste0("IAT_regression_coefficients_", season_folder_name, ".rds")
  ))
  
  # Displayed models: the LAST FOUR fitted specifications, matching the
  # main regression table.
  all_models <- unique(as.character(coef_results$model))
  n_display <- min(4, length(all_models))
  display_models <- tail(all_models, n_display)
  
  model_labels <- setNames(
    paste0("(", seq_along(display_models), ")"),
    display_models
  )
  
  model_order <- display_models
  
  label_for <- function(model_name) {
    if (model_name %in% names(model_labels)) {
      model_labels[[model_name]]
    } else {
      paste0("``", str_replace_all(model_name, "_", "\\\\_"), "''")
    }
  }
  
  fmt_num <- function(x) sprintf("%.4f", x)
  fmt_pct <- function(x) paste0(ifelse(x >= 0, "", "$-$"), sprintf("%.1f", abs(x)), "\\%")
  fmt_pct_plain <- function(x) paste0(ifelse(x >= 0, "", "$-$"), sprintf("%.1f", abs(x)))
  fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  
  # Per-season and per-month one-fight game counts (own Games column) --------
  iat_table_path <- file.path(
    working.dir,
    "generated/fights/single_fight_games_IAT_table.rds"
  )
  
  games_by_season <- NULL
  games_by_month <- NULL
  
  if (file.exists(iat_table_path)) {
    
    iat_games <- readRDS(iat_table_path) %>%
      mutate(
        season = as.character(season),
        game_id = as.character(game_id)
      ) %>%
      filter(season %in% seasons_used)
    
    games_by_season <- iat_games %>%
      distinct(season, game_id) %>%
      count(season, name = "n_games")
    
    if ("season_month" %in% names(iat_games)) {
      games_by_month <- iat_games %>%
        mutate(season_month = as.character(season_month)) %>%
        distinct(season_month, season, game_id) %>%
        count(season_month, name = "n_games")
    }
    
  } else {
    message(
      "single_fight_games_IAT_table.rds not found; Games columns will be omitted."
    )
  }
  
  out_dir <- file.path(working.dir, "generated", "tables", season_folder_name)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # ============================================================
  # Table A: Estimated post-fight rate ratios
  # ============================================================
  # Pre- and post-fight rates are averages of the fitted rates over all
  # observations. The rate ratio is the average of the per-observation
  # rate ratios, which is not in general the ratio of those two averages.
  
  ate_path <- file.path(
    results_folder,
    paste0("IAT_regression_avg_rate_ratios_", season_folder_name, ".rds")
  )
  
  if (!file.exists(ate_path)) {
    stop(
      "Average rate ratio file not found:\n  ", ate_path,
      "\nRe-run fit_IAT_log_model() for this season range to generate it."
    )
  }
  
  post_fight_by_model <- readRDS(ate_path) %>%
    filter(model %in% model_order) %>%
    mutate(model = factor(model, levels = model_order)) %>%
    arrange(model)
  
  tableA_rows <- post_fight_by_model %>%
    mutate(
      row = paste0(
        model_labels[as.character(model)], " & ",
        fmt_num(rate_pre), " & ",
        fmt_num(rate_post), " & ",
        fmt_num(avg_rate_ratio), " & ",
        fmt_num(std_error),
        " \\\\"
      )
    ) %>%
    pull(row)
  
  tableA_lines <- c(
    "\\begin{table}[!h]",
    "\\centering",
    paste0(
      "\\caption{Estimated post-fight rate ratios from exponential model of interarrival times in one-fight games, ",
      season_pretty(first_season), " to ", season_pretty(last_season), "}"
    ),
    paste0("\\label{tab:post_fight_rate_ratios_", season_folder_name, "}"),
    "",
    "\\begin{tabular}{ccccc}",
    "\\toprule",
    "Model & Pre-fight rate & Post-fight rate & Rate ratio & Standard error \\\\",
    "\\midrule",
    tableA_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.95\\textwidth}{",
    "\\footnotesize",
    paste0(
      "\\textit{Note:} Model numbers correspond to Table \\ref{tab:iat_regression_",
      season_folder_name, "}."),
    "}",
    "\\end{table}"
  )
  
  # ============================================================
  # Table B: Period effects, as rate ratios
  # ============================================================
  
  models_with_period <- coef_results %>%
    filter(term == "current_period2") %>%
    pull(model) %>%
    as.character() %>%
    unique()
  
  period_displayed <- intersect(display_models, models_with_period)
  
  period_source <- if (length(period_displayed) > 0) {
    tail(period_displayed, 1)
  } else if (length(models_with_period) > 0) {
    tail(models_with_period, 1)
  } else {
    NA_character_
  }
  
  period_effects <- NULL
  tableB_lines <- NULL
  
  if (is.na(period_source)) {
    
    message("No fitted model contains IAT-period effects; skipping the period effects table.")
    
  } else {
    
    period_coefs <- coef_results %>%
      filter(
        model == period_source,
        term %in% c("current_period2", "current_period3")
      ) %>%
      transmute(
        period_label = if_else(term == "current_period2", "Period 2", "Period 3"),
        rate_ratio = exp(estimate),
        # Delta method: se(exp(b)) = exp(b) * se(b)
        rate_ratio_se = exp(estimate) * std_error
      )
    
    period_effects <- bind_rows(
      tibble(
        period_label = "Period 1 (reference)",
        rate_ratio = 1,
        rate_ratio_se = NA_real_
      ),
      period_coefs
    )
    
    tableB_rows <- period_effects %>%
      mutate(
        row = paste0(
          period_label, " & ",
          fmt_num(rate_ratio), " & ",
          if_else(is.na(rate_ratio_se), "--", fmt_num(rate_ratio_se)),
          " \\\\"
        )
      ) %>%
      pull(row)
    
    tableB_lines <- c(
      "\\begin{table}[!h]",
      "\\centering",
      paste0(
        "\\caption{Estimated period effects from exponential model of interarrival times in one-fight games, ",
        season_pretty(first_season), " to ", season_pretty(last_season), "}"
      ),
      paste0("\\label{tab:period_effects_", season_folder_name, "}"),
      "",
      "\\begin{tabular}{lcc}",
      "\\toprule",
      "Period & Rate ratio & Standard error \\\\",
      "\\midrule",
      tableB_rows,
      "\\bottomrule",
      "\\end{tabular}",
      "",
      "\\vspace{0.4em}",
      "\\parbox{0.95\\textwidth}{",
      "\\footnotesize",
      paste0(
        "\\textit{Note:} Estimates are from model ", label_for(period_source),
        " of Table \\ref{tab:iat_regression_", season_folder_name, "}."
      ),
      "}",
      "\\end{table}"
    )
  }
  
  # ============================================================
  # Shared builder for the season and month effects tables
  # ============================================================
  # Columns: entity | Games | Rate ratio | Standard error
  
  build_effects_table <- function(effect_coefs, games_counts, ref_label,
                                  entity_col_name, caption_text, label_stub,
                                  source_model) {
    
    has_games <- !is.null(games_counts)
    
    effects <- bind_rows(
      tibble(
        entity_label = paste0(ref_label, " (reference)"),
        entity_key = "REF",
        rate_ratio = 1,
        rate_ratio_se = NA_real_
      ),
      effect_coefs
    )
    
    if (has_games) {
      effects <- effects %>%
        left_join(games_counts, by = "entity_key") %>%
        mutate(n_games = replace_na(n_games, 0))
    }
    
    header <- if (has_games) {
      paste0(entity_col_name, " & Games & Rate ratio & Standard error \\\\")
    } else {
      paste0(entity_col_name, " & Rate ratio & Standard error \\\\")
    }
    
    col_spec <- if (has_games) "lccc" else "lcc"
    
    rows <- effects %>%
      mutate(
        games_cell = if (has_games) paste0(fmt_int(n_games), " & ") else "",
        row = paste0(
          entity_label, " & ",
          games_cell,
          fmt_num(rate_ratio), " & ",
          if_else(is.na(rate_ratio_se), "--", fmt_num(rate_ratio_se)),
          " \\\\"
        )
      ) %>%
      pull(row)
    
    c(
      "\\begin{table}[!h]",
      "\\centering",
      paste0("\\caption{", caption_text, "}"),
      paste0("\\label{tab:", label_stub, "_", season_folder_name, "}"),
      "\\small",
      "\\renewcommand{\\arraystretch}{1.12}",
      "\\setlength{\\tabcolsep}{6pt}",
      "",
      paste0("\\begin{tabular*}{0.75\\textwidth}{@{\\extracolsep{\\fill}}", col_spec, "}"),
      "\\toprule",
      header,
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabular*}",
      "",
      "\\vspace{0.4em}",
      "\\parbox{0.75\\textwidth}{",
      "\\footnotesize",
      paste0(
        "\\textit{Note:} Estimates are from model ", label_for(source_model),
        " of Table \\ref{tab:iat_regression_", season_folder_name, "}."
      ),
      "}",
      "\\end{table}"
    )
  }
  
  # ============================================================
  # Table C: Season Effects (appendix)
  # ============================================================
  
  models_with_season <- coef_results %>%
    filter(str_detect(term, "^season\\d")) %>%
    pull(model) %>%
    as.character() %>%
    unique()
  
  season_source <- {
    cand <- tail(intersect(display_models, models_with_season), 1)
    if (length(cand) == 0) NA_character_ else cand
  }
  
  season_effects <- NULL
  tableC_lines <- NULL
  
  if (length(season_source) == 0 || is.na(season_source)) {
    
    message("No displayed model contains season effects; skipping the season effects table.")
    
  } else {
    
    season_coefs <- coef_results %>%
      filter(
        model == season_source,
        str_detect(term, "^season\\d")
      ) %>%
      transmute(
        entity_key = str_remove(term, "^season"),
        entity_label = season_pretty(entity_key),
        rate_ratio = exp(estimate),
        rate_ratio_se = exp(estimate) * std_error
      ) %>%
      arrange(entity_key)
    
    season_games <- NULL
    
    if (!is.null(games_by_season)) {
      ref_season <- sort(seasons_used)[1]
      season_games <- games_by_season %>%
        transmute(
          entity_key = if_else(season == ref_season, "REF", season),
          n_games = n_games
        )
    }
    
    season_effects <- season_coefs
    
    tableC_lines <- build_effects_table(
      effect_coefs = season_coefs,
      games_counts = season_games,
      ref_label = season_pretty(sort(seasons_used)[1]),
      entity_col_name = "Season",
      caption_text = paste0(
        "Estimated season effects from exponential model of interarrival times in one-fight games, ",
        season_pretty(first_season), " to ", season_pretty(last_season)
      ),
      label_stub = "season_effects",
      source_model = season_source
    )
  }
  
  # ============================================================
  # Table D: Month-of-Season Effects (appendix)
  # ============================================================
  
  models_with_month <- coef_results %>%
    filter(str_detect(term, "^season_month")) %>%
    pull(model) %>%
    as.character() %>%
    unique()
  
  month_source <- {
    cand <- tail(intersect(display_models, models_with_month), 1)
    if (length(cand) == 0) NA_character_ else cand
  }
  
  month_effects <- NULL
  tableD_lines <- NULL
  
  if (length(month_source) == 0 || is.na(month_source)) {
    
    message("No displayed model contains month-of-season effects; skipping the month effects table.")
    
  } else {
    
    month_coefs <- coef_results %>%
      filter(
        model == month_source,
        str_detect(term, "^season_month")
      ) %>%
      transmute(
        entity_key = str_remove(term, "^season_month"),
        entity_label = paste0("Month ", entity_key),
        rate_ratio = exp(estimate),
        rate_ratio_se = exp(estimate) * std_error
      ) %>%
      arrange(as.numeric(entity_key))
    
    month_games <- NULL
    
    if (!is.null(games_by_month)) {
      month_games <- games_by_month %>%
        transmute(
          entity_key = if_else(season_month == "1", "REF", season_month),
          n_games = n_games
        )
    }
    
    month_effects <- month_coefs
    
    tableD_lines <- build_effects_table(
      effect_coefs = month_coefs,
      games_counts = month_games,
      ref_label = "Month 1",
      entity_col_name = "Month of season",
      caption_text = paste0(
        "Estimated month-of-season effects from exponential model of interarrival times in one-fight games, ",
        season_pretty(first_season), " to ", season_pretty(last_season)
      ),
      label_stub = "month_effects",
      source_model = month_source
    )
  }
  
  # ============================================================
  # Save files
  # ============================================================
  
  save_table <- function(lines, stub) {
    if (is.null(lines)) return(invisible(NULL))
    file_name <- paste0(stub, "_", season_folder_name, ".tex")
    writeLines(lines, file.path(out_dir, file_name))
    message("Saved: generated/tables/", season_folder_name, "/", file_name)
    message("In Overleaf, use:")
    message("\\input{tables/", season_folder_name, "/", file_name, "}")
    message("")
  }
  
  save_table(tableA_lines, "post_fight_rate_ratios")
  save_table(tableB_lines, "period_effects")
  save_table(tableC_lines, "season_effects")
  save_table(tableD_lines, "month_effects")
  
  list(
    post_fight_by_model = post_fight_by_model %>%
      transmute(
        model = model_labels[as.character(model)],
        rate_pre,
        rate_post,
        avg_rate_ratio,
        std_error,
        p_value
      ),
    period_effects = period_effects,
    season_effects = season_effects,
    month_effects = month_effects
  )
}
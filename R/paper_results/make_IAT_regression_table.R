make_IAT_regression_table <- function(working.dir, pbp_master_used) {
  
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
  
  model_summary <- readRDS(file.path(
    results_folder,
    paste0("IAT_regression_model_summary_", season_folder_name, ".rds")
  ))
  
  # Display only the LAST THREE fitted specifications, relabeled (1)-(3).
  # All models are still estimated and saved; the earlier specs are
  # robustness checks and are not shown in the paper table.
  all_models <- unique(as.character(coef_results$model))
  n_display <- min(4, length(all_models))
  display_models <- tail(all_models, n_display)
  
  model_labels <- setNames(
    paste0("(", seq_along(display_models), ")"),
    display_models
  )
  
  model_order <- display_models
  
  # Terms: known labels first, in preferred order, then any remaining
  # non-season terms from the displayed models (nothing hardcoded).
  # Order: treatment, treatment interactions, within-game fixed effects,
  # game-level fixed effects last (adjacent to the game-level footer rows).
  term_labels <- c(
    "(Intercept)"                  = "Intercept",
    "post_fight"                   = "Post-fight",
    "post_fight:fight_score_diff"  = "Post-fight $\\times$ score differential",
    "post_fight:fight_period2"     = "Post-fight $\\times$ Fight in period 2",
    "post_fight:fight_period3"     = "Post-fight $\\times$ Fight in period 3",
    "fight_score_diff"             = "Score differential at fight",
    "fight_period2"                = "Fight in period 2",
    "fight_period3"                = "Fight in period 3",
    "current_period2"              = "Period 2",
    "current_period3"              = "Period 3",
    "intradivision"                = "Intradivision game",
    "pre_fight_window"             = "Immediately before fight",
    "fight_score_diff_cat1"        = "One-goal margin at fight",
    "fight_score_diff_cat2"        = "Two-goal margin at fight",
    "fight_score_diff_cat3plus"    = "Three-or-more-goal margin at fight",
    "post_fight:fight_score_diff_cat1" =
      "Post-fight $\\times$ one-goal margin",
    "post_fight:fight_score_diff_cat2" =
      "Post-fight $\\times$ two-goal margin",
    "post_fight:fight_score_diff_cat3plus" =
      "Post-fight $\\times$ three-or-more-goal margin"
  )
  
  present_terms <- coef_results %>%
    filter(
      model %in% display_models,
      !str_detect(term, "^season")
    ) %>%
    distinct(term) %>%
    pull(term)
  
  display_terms <- c(
    intersect(names(term_labels), present_terms),
    setdiff(present_terms, names(term_labels))
  )
  
  escape_tex <- function(x) str_replace_all(x, "_", "\\\\_")
  
  get_term_label <- function(term_name) {
    if (term_name %in% names(term_labels)) {
      term_labels[[term_name]]
    } else {
      escape_tex(term_name)
    }
  }
  
  sig_stars <- function(p) {
    case_when(
      p < 0.001 ~ "^{***}",
      p < 0.01  ~ "^{**}",
      p < 0.05  ~ "^{*}",
      TRUE      ~ ""
    )
  }
  
  make_coef_clean <- function(terms_to_keep) {
    coef_results %>%
      filter(term %in% terms_to_keep, model %in% model_order) %>%
      mutate(
        model = factor(model, levels = model_order),
        term = factor(term, levels = terms_to_keep),
        stars = sig_stars(p_value),
        estimate_cell = paste0("$", sprintf("%.4f", estimate), stars, "$"),
        se_cell = paste0("$(", sprintf("%.4f", std_error), ")$")
      ) %>%
      select(term, model, estimate_cell, se_cell)
  }
  
  coef_clean <- make_coef_clean(display_terms)
  
  get_cell <- function(data, term_name, model_name, cell_col) {
    out <- data[[cell_col]][
      as.character(data$term) == term_name &
        as.character(data$model) == model_name
    ]
    
    if (length(out) == 0) {
      return("")
    }
    
    out[1]
  }
  
  coef_lines <- c()
  
  for (term_name in display_terms) {
    
    label <- get_term_label(term_name)
    
    estimate_row <- c(label)
    se_row <- c("")
    
    for (model_name in model_order) {
      estimate_row <- c(
        estimate_row,
        get_cell(coef_clean, term_name, model_name, "estimate_cell")
      )
      
      se_row <- c(
        se_row,
        get_cell(coef_clean, term_name, model_name, "se_cell")
      )
    }
    
    coef_lines <- c(
      coef_lines,
      paste0(paste(estimate_row, collapse = " & "), " \\\\"),
      paste0(paste(se_row, collapse = " & "), " \\\\")
    )
  }
  
  # Summary rows -------------------------------------------------------------
  if (!"period_fixed_effects" %in% names(model_summary)) {
    model_summary <- model_summary %>%
      mutate(period_fixed_effects = FALSE)
  }
  
  if (!"month_fixed_effects" %in% names(model_summary)) {
    model_summary <- model_summary %>%
      mutate(month_fixed_effects = FALSE)
  }
  
  if (!"score_diff_controls" %in% names(model_summary)) {
    model_summary <- model_summary %>%
      mutate(score_diff_controls = str_detect(
        model,
        "^month_fixed_effects$|^game_fixed_effects$"
      ))
  }
  
  if (!"fight_period_controls" %in% names(model_summary)) {
    model_summary <- model_summary %>%
      mutate(fight_period_controls = str_detect(
        model,
        "period_interaction|period_fixed_effects|month_fixed_effects|^game_fixed_effects$"
      ))
  }
  
  if (!"game_fixed_effects" %in% names(model_summary)) {
    model_summary <- model_summary %>%
      mutate(game_fixed_effects = FALSE)
  }
  
  summary_clean <- model_summary %>%
    filter(model %in% model_order) %>%
    mutate(
      model = factor(model, levels = model_order),
      # Controls absorbed by game fixed effects are dropped from the model
      # and not identified, so they are reported as "No" in that column.
      season_controls = ifelse(
        season_controls & !game_fixed_effects, "Yes", "No"
      ),
      month_fixed_effects_flag = month_fixed_effects,
      period_fixed_effects = ifelse(period_fixed_effects, "Yes", "No"),
      month_fixed_effects = ifelse(
        month_fixed_effects_flag & !game_fixed_effects, "Yes", "No"
      ),
      game_fixed_effects = ifelse(game_fixed_effects, "Yes", "No"),
      fight_period_controls = ifelse(fight_period_controls, "Yes", "No"),
      score_diff_controls = ifelse(score_diff_controls, "Yes", "No"),
      n_games = format(n_games, big.mark = ",", trim = TRUE),
      n_iats = format(n_iats, big.mark = ",", trim = TRUE),
      log_lik = format(round(log_lik, 2), big.mark = ",", nsmall = 2)
    )
  
  get_summary <- function(model_name, var_name) {
    out <- summary_clean[[var_name]][
      as.character(summary_clean$model) == model_name
    ]
    
    if (length(out) == 0) {
      return("")
    }
    
    out[1]
  }
  
  make_summary_row <- function(label, var_name) {
    row <- c(label)
    
    for (model_name in model_order) {
      row <- c(row, get_summary(model_name, var_name))
    }
    
    paste0(paste(row, collapse = " & "), " \\\\")
  }
  
  show_score_diff_note <- any(str_detect(display_terms, "^fight_score_diff_cat|:fight_score_diff_cat"))
  show_month_row <- any(summary_clean$month_fixed_effects == "Yes")
  show_game_fe_row <- any(summary_clean$game_fixed_effects == "Yes")
  show_fight_period_row <- n_distinct(summary_clean$fight_period_controls) > 1
  
  summary_lines <- c(
    make_summary_row("Season fixed effects", "season_controls"),
    if (show_fight_period_row) make_summary_row("Fight-period fixed effects", "fight_period_controls"),
    if (show_month_row) make_summary_row("Month-of-season fixed effects", "month_fixed_effects"),
    if (show_game_fe_row) make_summary_row("Game fixed effects", "game_fixed_effects")
  )
  
  # Sample sizes for the table note (identical across displayed models)
  note_n_games <- get_summary(model_order[1], "n_games")
  note_n_iats <- get_summary(model_order[1], "n_iats")
  
  # Assemble main regression table -------------------------------------------
  n_models <- length(model_order)
  col_spec <- paste0("l", strrep("c", n_models))
  header_row <- paste0(
    " & ",
    paste(model_labels[model_order], collapse = " & "),
    " \\\\"
  )
  
  regression_latex_lines <- c(
    "\\begin{table}[!h]",
    "\\centering",
    paste0(
      "\\caption{Coefficient estimates from exponential model of interarrival times in one-fight games, ",
      season_pretty(first_season), " to ", season_pretty(last_season), "}"
    ),
    paste0("\\label{tab:iat_regression_", season_folder_name, "}"),
    "\\scriptsize",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{5pt}",
    "",
    paste0("\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}", col_spec, "}"),
    "\\toprule",
    header_row,
    "\\midrule",
    coef_lines,
    "\\midrule",
    summary_lines,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.95\\textwidth}{",
    "\\footnotesize",
    paste0(
      "\\textit{Note:} entries are coefficient estimates with game-clustered standard errors in parentheses; ",
      "all models are estimated on ", note_n_iats, " IATs from ", note_n_games, " games; ",
      "significance codes are $^{***}p<0.001$, $^{**}p<0.01$, $^{*}p<0.05$."
    ),
    "}",
    "\\end{table}"
  )
  
  # Save files -------------------------------------------------------------
  
  out_dir <- file.path(working.dir, "generated", "tables", season_folder_name)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  regression_file <- paste0("IAT_regression_table_", season_folder_name, ".tex")
  
  writeLines(
    regression_latex_lines,
    file.path(out_dir, regression_file)
  )
  
  
  message("Saved: generated/tables/", season_folder_name, "/", regression_file)
  message("In Overleaf, use:")
  message("\\input{tables/", season_folder_name, "/", regression_file, "}")
  
  # Return/show main regression table in R output --------------------------
  
  regression_preview <- coef_results %>%
    filter(term %in% display_terms, model %in% model_order) %>%
    mutate(
      model = factor(model, levels = model_order),
      term = factor(term, levels = display_terms),
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      cell = paste0(sprintf("%.4f", estimate), stars, " (", sprintf("%.4f", std_error), ")")
    ) %>%
    select(term, model, cell) %>%
    pivot_wider(
      names_from = model,
      values_from = cell,
      values_fill = ""
    ) %>%
    arrange(term) %>%
    mutate(term = sapply(as.character(term), get_term_label)) %>%
    rename(variable = term)
  
  names(regression_preview)[names(regression_preview) %in% model_order] <-
    model_labels[names(regression_preview)[names(regression_preview) %in% model_order]]
  
  return(regression_preview)
}
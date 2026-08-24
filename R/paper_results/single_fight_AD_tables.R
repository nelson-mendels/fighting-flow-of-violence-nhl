single_fight_AD_tables <- function(
    working.dir,
    pbp_master_used,
    event_name = "VIOLENT_CONTACT",
    min_events_range = 15:25,
    alpha = 0.05,
    game_level_rds = file.path(
      working.dir,
      "generated/fights/game_level_with_single_fight_lambdas.rds"
    ),
    single_fight_rds = file.path(
      working.dir,
      "generated/fights/single_fight_games.rds"
    )
) {
  
  # -------------------------------------------------------------------------
  # This function uses the same AD-test logic as calculate_IAT().
  #
  # Original game-level AD pipeline:
  #   IAT = c(first event time / 60, diff(event times) / 60)
  #   lambda = total events / 60
  #   ad.test(IAT, null = "pexp", rate = lambda)
  #
  # For pre/post fight windows, we copy that same logic, but reset the window:
  #   pre window:  0 to fight_time
  #   post window: fight_time to 3600
  # -------------------------------------------------------------------------
  
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
  
  message("Working on season range: ", season_folder_name)
  
  # Match the identifier types used in build_game_level_IAT_data(). The master
  # dataset is already restricted to regular-season regulation periods.
  pbp_clean <- pbp_master_used %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      game_id_full = as.character(game_id_full)
    )
  
  if (event_name == "VIOLENT_CONTACT") {
    pbp_clean <- filter_penalties(pbp_clean)
  }
  
  # Load official whole-game AD results, restricted to the seasons used
  game_and_fight <- readRDS(game_level_rds) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id)
    ) %>%
    filter(season %in% seasons_used)
  
  single_fight <- readRDS(single_fight_rds) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id)
    ) %>%
    filter(season %in% seasons_used)
  
  required_game_cols <- c(
    "game_id",
    "season",
    "event",
    "n",
    "ad_p_value",
    "n_fights"
  )
  
  missing_game_cols <- setdiff(required_game_cols, names(game_and_fight))
  
  if (length(missing_game_cols) > 0) {
    stop(
      "Missing required columns in game-level file: ",
      paste(missing_game_cols, collapse = ", "),
      "\nYou may need to run merge_game_and_fight_data(working.dir) before this function."
    )
  }
  
  required_single_cols <- c("game_id", "season", "fight_time")
  missing_single_cols <- setdiff(required_single_cols, names(single_fight))
  
  if (length(missing_single_cols) > 0) {
    stop(
      "Missing required columns in single-fight file: ",
      paste(missing_single_cols, collapse = ", ")
    )
  }
  
  # Official whole-game AD results
  # These are the same p-values used in the fight/rejection bar plot.
  overall_game_tests <- game_and_fight %>%
    filter(
      event == event_name,
      n_fights == 1
    ) %>%
    select(
      game_id,
      season,
      overall_n_events = n,
      overall_ad_p_value = ad_p_value
    ) %>%
    mutate(
      overall_ad_result = case_when(
        is.na(overall_ad_p_value) ~ NA_character_,
        overall_ad_p_value <= alpha ~ "Reject",
        TRUE ~ "Do not reject"
      )
    )
  
  # Project-style AD test, copied from calculate_IAT() logic
  run_project_style_ad <- function(event_times, window_start, window_end) {
    
    event_times <- event_times[
      is.finite(event_times) &
        !is.na(event_times) &
        event_times > window_start &
        event_times < window_end
    ]
    
    event_times <- sort(event_times)
    
    n_events <- length(event_times)
    window_minutes <- (window_end - window_start) / 60
    
    if (n_events < 2 || window_minutes <= 0) {
      return(tibble(
        n_events = n_events,
        lambda_hat = NA_real_,
        ad_statistic = NA_real_,
        ad_p_value = NA_real_
      ))
    }
    
    rel_times_min <- (event_times - window_start) / 60
    
    IAT_values <- c(
      rel_times_min[1],
      diff(rel_times_min)
    )
    
    lambda_hat <- n_events / window_minutes
    
    ad_test <- tryCatch(
      ad.test(
        IAT_values,
        null = "pexp",
        rate = lambda_hat
      ),
      error = function(e) NULL
    )
    
    if (is.null(ad_test)) {
      return(tibble(
        n_events = n_events,
        lambda_hat = lambda_hat,
        ad_statistic = NA_real_,
        ad_p_value = NA_real_
      ))
    }
    
    tibble(
      n_events = n_events,
      lambda_hat = lambda_hat,
      ad_statistic = as.numeric(ad_test$statistic),
      ad_p_value = as.numeric(ad_test$p.value)
    )
  }
  
  # Event times for violent contact
  event_data <- pbp_clean %>%
    filter(event_type == event_name) %>%
    select(game_id, season, game_seconds)
  
  message("Running pre/post fight AD tests...")
  
  # Calculate pre/post AD results (computed once, reused for every cutoff)
  game_side_tests <- single_fight %>%
    select(game_id, season, fight_time) %>%
    inner_join(event_data, by = c("game_id", "season")) %>%
    group_by(game_id, season, fight_time) %>%
    summarise(
      event_times = list(game_seconds),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      pre_test = list(
        run_project_style_ad(
          event_times = event_times,
          window_start = 0,
          window_end = fight_time
        )
      ),
      post_test = list(
        run_project_style_ad(
          event_times = event_times,
          window_start = fight_time,
          window_end = 3600
        )
      )
    ) %>%
    ungroup()
  
  pre_results <- game_side_tests %>%
    select(game_id, season, pre_test) %>%
    unnest(pre_test) %>%
    rename(
      pre_n_events = n_events,
      pre_lambda_hat = lambda_hat,
      pre_ad_statistic = ad_statistic,
      pre_ad_p_value = ad_p_value
    ) %>%
    mutate(
      pre_ad_result = case_when(
        is.na(pre_ad_p_value) ~ NA_character_,
        pre_ad_p_value <= alpha ~ "Reject",
        TRUE ~ "Do not reject"
      )
    )
  
  post_results <- game_side_tests %>%
    select(game_id, season, post_test) %>%
    unnest(post_test) %>%
    rename(
      post_n_events = n_events,
      post_lambda_hat = lambda_hat,
      post_ad_statistic = ad_statistic,
      post_ad_p_value = ad_p_value
    ) %>%
    mutate(
      post_ad_result = case_when(
        is.na(post_ad_p_value) ~ NA_character_,
        post_ad_p_value <= alpha ~ "Reject",
        TRUE ~ "Do not reject"
      )
    )
  
  game_level_results <- pre_results %>%
    inner_join(post_results, by = c("game_id", "season")) %>%
    left_join(overall_game_tests, by = c("game_id", "season"))
  
  # Output folder: subfolder of the season-range tables folder ---------------
  out_dir <- file.path(
    working.dir,
    "generated",
    "tables",
    season_folder_name,
    "one_fight_AD_rejection"
  )
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  fmt_int <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  output_list <- list()
  summary_rows <- list()
  
  for (min_events in min_events_range) {
    
    eligible_games <- game_level_results %>%
      filter(
        pre_n_events >= min_events,
        post_n_events >= min_events,
        !is.na(pre_ad_result),
        !is.na(post_ad_result),
        !is.na(overall_ad_result)
      ) %>%
      mutate(
        pre_ad_result = factor(
          pre_ad_result,
          levels = c("Do not reject", "Reject")
        ),
        post_ad_result = factor(
          post_ad_result,
          levels = c("Do not reject", "Reject")
        )
      )
    
    n_games <- nrow(eligible_games)
    
    table_2x2 <- table(
      eligible_games$pre_ad_result,
      eligible_games$post_ad_result
    )
    
    n_overall_reject <- sum(
      eligible_games$overall_ad_p_value <= alpha,
      na.rm = TRUE
    )
    
    overall_rate <- ifelse(
      n_games > 0,
      n_overall_reject / n_games,
      NA_real_
    )
    
    message(
      season_folder_name, " | min events >= ", min_events, ": ",
      fmt_int(n_games), " games."
    )
    
    # LaTeX 2x2 table with margins ------------------------------------------
    dnr_dnr <- table_2x2["Do not reject", "Do not reject"]
    dnr_rej <- table_2x2["Do not reject", "Reject"]
    rej_dnr <- table_2x2["Reject", "Do not reject"]
    rej_rej <- table_2x2["Reject", "Reject"]
    
    latex_lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0(
        "\\caption{Pre- Versus Post-Fight Anderson--Darling Outcomes in One-Fight Games (Minimum ",
        min_events, " Events per Window), ",
        season_pretty(first_season), " to ", season_pretty(last_season), "}"
      ),
      paste0(
        "\\label{tab:one_fight_AD_rejection_min_", min_events,
        "_", season_folder_name, "}"
      ),
      "\\small",
      "\\renewcommand{\\arraystretch}{1.15}",
      "\\setlength{\\tabcolsep}{6pt}",
      "",
      "\\begin{tabular*}{0.78\\textwidth}{@{\\extracolsep{\\fill}}lccc}",
      "\\toprule",
      " & \\multicolumn{2}{c}{Post-fight AD result} & \\\\",
      "\\cmidrule(lr){2-3}",
      "Pre-fight AD result & Do not reject & Reject & Total \\\\",
      "\\midrule",
      paste0(
        "Do not reject & ",
        fmt_int(dnr_dnr), " & ", fmt_int(dnr_rej), " & ",
        fmt_int(dnr_dnr + dnr_rej), " \\\\"
      ),
      paste0(
        "Reject & ",
        fmt_int(rej_dnr), " & ", fmt_int(rej_rej), " & ",
        fmt_int(rej_dnr + rej_rej), " \\\\"
      ),
      "\\midrule",
      paste0(
        "Total & ",
        fmt_int(dnr_dnr + rej_dnr), " & ", fmt_int(dnr_rej + rej_rej), " & ",
        fmt_int(n_games), " \\\\"
      ),
      "\\bottomrule",
      "\\end{tabular*}",
      "",
      "\\vspace{0.4em}",
      "\\parbox{0.78\\textwidth}{",
      "\\footnotesize",
      paste0(
        "\\textit{Note:} Entries are counts of one-fight games during ", season_range_text, ". ",
        "Pre-fight and post-fight windows run from the opening faceoff to the fight and from the fight to the end of regulation, respectively. ",
        "Each window's Anderson--Darling test evaluates exponentiality of the interarrival times of violent contact events within that window at the ",
        sprintf("%.2f", alpha), " level, using the window-specific event rate. ",
        "The sample is restricted to games with at least ", min_events,
        " observed events in each window. ",
        "For comparison, the whole-game AD test rejects in ",
        fmt_int(n_overall_reject), " of these ", fmt_int(n_games),
        " games (", sprintf("%.1f", 100 * overall_rate), "\\%)."
      ),
      "}",
      "\\end{table}"
    )
    
    table_file <- paste0(
      "one_fight_AD_rejection_min_", min_events,
      "_", season_folder_name, ".tex"
    )
    
    writeLines(latex_lines, file.path(out_dir, table_file))
    
    summary_rows[[as.character(min_events)]] <- tibble(
      min_events = min_events,
      n_games = n_games,
      pre_reject = rej_dnr + rej_rej,
      post_reject = dnr_rej + rej_rej,
      both_reject = rej_rej,
      neither_reject = dnr_dnr,
      n_overall_reject = n_overall_reject,
      overall_reject_rate = overall_rate
    )
    
    output_list[[paste0("min_", min_events)]] <- list(
      min_events = min_events,
      table_2x2 = table_2x2,
      n_games = n_games,
      n_overall_reject = n_overall_reject,
      overall_reject_rate = overall_rate
    )
  }
  
  message("")
  message(
    "Saved ", length(min_events_range), " tables to: generated/tables/",
    season_folder_name, "/one_fight_AD_rejection/"
  )
  message("In Overleaf, use:")
  message(
    "\\input{tables/", season_folder_name,
    "/one_fight_AD_rejection/one_fight_AD_rejection_min_{m}_",
    season_folder_name, ".tex}"
  )
  
  output_list$summary_by_min_events <- bind_rows(summary_rows)
  
  invisible(output_list)
}

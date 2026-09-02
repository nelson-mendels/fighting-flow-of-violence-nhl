make_one_fight_sample <- function(working.dir, pbp_master_used) {
  
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
  
  # Load the ESTIMATION sample saved by fit_IAT_log_model(), so this table
  # describes exactly the games and IATs the models are fit on. Counting
  # from the raw one-fight files instead would include games the model
  # drops (e.g., missing covariates), and the counts would not match the
  # regression table notes.
  sample_path <- file.path(
    working.dir,
    "generated",
    "fights",
    season_folder_name,
    paste0("IAT_estimation_sample_", season_folder_name, ".rds")
  )
  
  if (!file.exists(sample_path)) {
    stop(
      "Estimation sample file not found:\n  ", sample_path,
      "\nRe-run fit_IAT_log_model() for this season range to generate it."
    )
  }
  
  iat_data <- readRDS(sample_path) %>%
    mutate(
      season = as.character(season),
      game_id = as.character(game_id),
      RC = as.integer(RC),
      post_fight = as.integer(post_fight)
    )
  
  single_fight <- iat_data %>%
    distinct(season, game_id, .keep_all = TRUE)
  
  fmt_int <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  fmt_num <- function(x, digits = 1) {
    sprintf(paste0("%.", digits, "f"), x)
  }
  
  n_games <- n_distinct(paste(single_fight$season, single_fight$game_id))
  n_iats <- nrow(iat_data)
  
  n_pre <- sum(iat_data$post_fight == 0, na.rm = TRUE)
  n_post <- sum(iat_data$post_fight == 1, na.rm = TRUE)
  
  n_rc <- sum(iat_data$RC == 1, na.rm = TRUE)
  n_intradivision <- sum(single_fight$intradivision == 1, na.rm = TRUE)
  
  mean_iat <- mean(iat_data$IAT, na.rm = TRUE)
  mean_pre_iat <- mean(iat_data$IAT[iat_data$post_fight == 0], na.rm = TRUE)
  mean_post_iat <- mean(iat_data$IAT[iat_data$post_fight == 1], na.rm = TRUE)
  
  table_data <- tibble(
    characteristic = c(
      "One-fight games",
      "IAT observations",
      "Pre-fight IAT observations",
      "Post-fight IAT observations",
      "Right-censored IAT observations",
      "Intradivision games",
      "Mean IAT",
      "Mean pre-fight IAT",
      "Mean post-fight IAT"
    ),
    value = c(
      fmt_int(n_games),
      fmt_int(n_iats),
      fmt_int(n_pre),
      fmt_int(n_post),
      fmt_int(n_rc),
      fmt_int(n_intradivision),
      paste0(fmt_num(mean_iat, 1), " sec"),
      paste0(fmt_num(mean_pre_iat, 1), " sec"),
      paste0(fmt_num(mean_post_iat, 1), " sec")
    )
  )
  
  table_rows <- apply(
    table_data,
    1,
    function(x) {
      paste0(x[["characteristic"]], " & ", x[["value"]], " \\\\")
    }
  )
  
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0(
      "\\caption{NHL games with one fight in ",
      if (first_season == last_season) {
        paste0("the ", season_pretty(first_season), " season")
      } else {
        paste0(
          "seasons ", season_pretty(first_season),
          " through ", season_pretty(last_season)
        )
      },
      "}"
    ),
    paste0("\\label{tab:one_fight_sample_", season_folder_name, "}"),
    "\\scriptsize",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{5pt}",
    "",
    "\\begin{tabular*}{0.78\\textwidth}{@{\\extracolsep{\\fill}}lr}",
    "\\toprule",
    "Sample characteristic & Value \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "\\end{table}"
  )
  
  out_dir <- file.path(working.dir, "generated", "tables", season_folder_name)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  sample_file <- paste0("one_fight_sample_", season_folder_name, ".tex")
  
  writeLines(
    latex_lines,
    file.path(out_dir, sample_file)
  )
  
  message("Saved: generated/tables/", season_folder_name, "/", sample_file)
  message("In Overleaf, use:")
  message("\\input{tables/", season_folder_name, "/", sample_file, "}")
  
  return(table_data)
}

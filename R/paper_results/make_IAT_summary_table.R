make_IAT_summary_table <- function(working.dir) {
  
  game_level <- readRDS(file.path(
    working.dir,
    "generated/IAT_data/IAT_game_level_data.rds"
  )) %>%
    mutate(
      season       = as.character(season),
      game_id      = as.character(game_id),
      game_id_full = as.character(game_id_full)
    )
  
  iat_data <- readRDS(file.path(
    working.dir,
    "generated/fights/single_fight_games_IAT_table.rds"
  )) %>%
    mutate(
      season  = as.character(season),
      game_id = as.character(game_id)
    )
  
  single_fight_games <- readRDS(file.path(
    working.dir,
    "generated/fights/single_fight_games.rds"
  )) %>%
    mutate(
      season  = as.character(season),
      game_id = as.character(game_id)
    )
  
  fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_num <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), x)
  
  table_data <- tibble(
    metric = c(
      "Seasons",
      "Games analyzed",
      "Violent contact events",
      "Mean violent contact events per game",
      "Median violent contact events per game",
      "Total fights",
      "Games with at least one fight",
      "One-fight games",
      "IAT observations in one-fight games",
      "Mean IAT in one-fight games",
      "Median IAT in one-fight games",
      "Right-censored IAT observations",
      "Post-fight IAT observations",
      "A--D rejection rate"
    ),
    value = c(
      fmt_int(n_distinct(game_level$season)),
      fmt_int(n_distinct(paste(game_level$season, game_level$game_id_full))),
      fmt_int(sum(game_level$n, na.rm = TRUE)),
      fmt_num(mean(game_level$n, na.rm = TRUE), 1),
      fmt_num(median(game_level$n, na.rm = TRUE), 1),
      fmt_int(sum(game_level$n_fights, na.rm = TRUE)),
      fmt_int(sum(game_level$n_fights > 0, na.rm = TRUE)),
      fmt_int(nrow(single_fight_games)),
      fmt_int(nrow(iat_data)),
      paste0(fmt_num(mean(iat_data$IAT,   na.rm = TRUE), 1), " sec"),
      paste0(fmt_num(median(iat_data$IAT, na.rm = TRUE), 1), " sec"),
      fmt_int(sum(iat_data$RC         == 1, na.rm = TRUE)),
      fmt_int(sum(iat_data$post_fight == 1, na.rm = TRUE)),
      paste0(fmt_num(mean(game_level$ad_p_value < 0.05, na.rm = TRUE) * 100, 1), "\\%")
    )
  )
  
  table_rows <- apply(table_data, 1, function(x) {
    paste0(x[["metric"]], " & ", x[["value"]], " \\\\")
  })
  
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Summary of Violent Contact IAT Data}",
    "\\label{tab:iat_summary}",
    "\\small",
    "\\renewcommand{\\arraystretch}{1.15}",
    "\\setlength{\\tabcolsep}{6pt}",
    "",
    "\\begin{tabular*}{0.78\\textwidth}{@{\\extracolsep{\\fill}}lr}",
    "\\toprule",
    "Metric & Value \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.78\\textwidth}{",
    "\\footnotesize",
    "\\textit{Note:} Regular-season NHL games, 2010--11 through 2025--26. Violent contact events are counted from the game-level IAT data. A--D rejection rate is the share of game-level Anderson--Darling tests rejecting exponential interarrival times at the 5\\% level.",
    "}",
    "\\end{table}"
  )
  
  out_dir <- file.path(working.dir, "generated", "tables")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  writeLines(latex_lines, file.path(out_dir, "IAT_summary_table.tex"))
  
  message("Saved: generated/tables/IAT_summary_table.tex")
  message("In Overleaf, use:")
  message("\\input{tables/IAT_summary_table.tex}")
  
  return(table_data)
}

make_fight_desc_table <- function(working.dir) {
  
  # Load paper-ready game-level data
  game_level <- readRDS(
    file.path(working.dir, "generated/IAT_data/IAT_game_level_data.rds")
  ) %>%
    mutate(season = as.character(season))
  
  # Keep one row per game
  game_fights <- game_level %>%
    filter(event == "VIOLENT_CONTACT") %>%
    distinct(season, game_id_full, .keep_all = TRUE) %>%
    mutate(
      season_label = paste0(substr(season, 1, 4), "--", substr(season, 7, 8)),
      fight_cat = case_when(
        n_fights == 0 ~ "pct_0",
        n_fights == 1 ~ "pct_1",
        n_fights == 2 ~ "pct_2",
        n_fights >= 3 ~ "pct_3plus",
        TRUE ~ NA_character_
      )
    )
  
  # Season-level fight summary
  tbl_data <- game_fights %>%
    group_by(season_label, season) %>%
    summarise(
      n_games = n(),
      total_fights = sum(n_fights, na.rm = TRUE),
      fights_per_game = total_fights / n_games,
      pct_0 = mean(fight_cat == "pct_0", na.rm = TRUE) * 100,
      pct_1 = mean(fight_cat == "pct_1", na.rm = TRUE) * 100,
      pct_2 = mean(fight_cat == "pct_2", na.rm = TRUE) * 100,
      pct_3plus = mean(fight_cat == "pct_3plus", na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    arrange(season) %>%
    select(
      season_label,
      n_games,
      total_fights,
      fights_per_game,
      pct_0,
      pct_1,
      pct_2,
      pct_3plus
    )
  
  print(tbl_data)
  
  # Season range for the caption and note, taken from the data
  first_season_label <- tbl_data$season_label[1]
  last_season_label <- tbl_data$season_label[nrow(tbl_data)]
  
  # Format values for LaTeX
  latex_data <- tbl_data %>%
    mutate(
      n_games = format(n_games, big.mark = ",", scientific = FALSE),
      total_fights = format(total_fights, big.mark = ",", scientific = FALSE),
      fights_per_game = sprintf("%.2f", fights_per_game),
      pct_0 = sprintf("%.1f", pct_0),
      pct_1 = sprintf("%.1f", pct_1),
      pct_2 = sprintf("%.1f", pct_2),
      pct_3plus = sprintf("%.1f", pct_3plus)
    )
  
  table_rows <- apply(
    latex_data,
    1,
    function(x) {
      paste0(
        x[["season_label"]], " & ",
        x[["n_games"]], " & ",
        x[["total_fights"]], " & ",
        x[["fights_per_game"]], " & ",
        x[["pct_0"]], " & ",
        x[["pct_1"]], " & ",
        x[["pct_2"]], " & ",
        x[["pct_3plus"]],
        " \\\\"
      )
    }
  )
  
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0(
      "\\caption{Decomposition of fights per game in NHL seasons ",
      first_season_label, " through ", last_season_label, "}"
    ),
    "\\label{tab:fight_activity_by_season}",
    "\\scriptsize",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{5pt}",
    "",
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrrrr}",
    "\\toprule",
    "Season & Games & Fights & Fights per game & \\multicolumn{4}{c}{Percent of games by number of fights} \\\\",
    "\\cmidrule(lr){5-8}",
    " & & & & Zero & One & Two & Three or more \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.95\\textwidth}{",
    "\\footnotesize",
    "\\textit{Note:} Restricted to regulation periods in regular-season games; counts of fights represent unique game times as opposed to number of players involved.",
    "}",
    "\\end{table}"
  )
  
  # Save LaTeX
  out_dir <- file.path(working.dir, "generated", "tables")
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  writeLines(
    latex_lines,
    file.path(out_dir, "fight_desc_table.tex")
  )
  
  message("Saved: generated/tables/fight_desc_table.tex")
  message("In Overleaf, use:")
  message("\\input{tables/fight_desc_table.tex}")
  
  return(tbl_data)
}
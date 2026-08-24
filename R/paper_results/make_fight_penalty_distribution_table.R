# Renders the fighting-penalties-per-fight-moment distribution (built by
# build_fight_penalty_distribution) as the paper table.

make_fight_penalty_distribution_table <- function(working.dir) {
  
  rds_path <- file.path(
    working.dir,
    "generated/fights/fight_penalty_distribution.rds"
  )
  
  if (!file.exists(rds_path)) {
    stop(
      "fight_penalty_distribution.rds not found:\n  ", rds_path,
      "\nRun build_fight_penalty_distribution() first."
    )
  }
  
  dist_data <- readRDS(rds_path)
  distribution <- dist_data$distribution
  
  season_pretty <- function(x) {
    x <- as.character(x)
    ifelse(
      str_detect(x, "^\\d{8}$"),
      paste0(substr(x, 1, 4), "--", substr(x, 7, 8)),
      x
    )
  }
  
  fmt_int <- function(x) {
    format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  table_rows <- distribution %>%
    mutate(
      row = paste0(
        n_penalties, " & ",
        fmt_int(n_fights), " & ",
        sprintf("%.1f", percent_of_fights),
        " \\\\"
      )
    ) %>%
    pull(row)
  
  latex_lines <- c(
    "\\begin{table}[!h]",
    "\\centering",
    paste0(
      "\\caption{Fighting penalties per fight in NHL seasons ",
      season_pretty(dist_data$first_season),
      " through ",
      season_pretty(dist_data$last_season),
      "}"
    ),
    "\\label{tab:fight_penalty_distribution}",
    "\\small",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\setlength{\\tabcolsep}{6pt}",
    "",
    "\\begin{tabular*}{0.62\\textwidth}{@{\\extracolsep{\\fill}}ccc}",
    "\\toprule",
    "Fighting penalties & Fights & Percent of fights \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{0.62\\textwidth}{",
    "\\footnotesize",
    "\\textit{Note:} Fights are identified by timestamp; fighting penalties assessed at the same moment in a game are treated as belonging to a single fight.",
    "}",
    "\\end{table}"
  )
  
  out_dir <- file.path(working.dir, "generated", "tables")
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  writeLines(
    latex_lines,
    file.path(out_dir, "fight_penalty_distribution.tex")
  )
  
  message("Saved: generated/tables/fight_penalty_distribution.tex")
  message("In Overleaf, use:")
  message("\\input{tables/fight_penalty_distribution.tex}")
  
  return(distribution)
}
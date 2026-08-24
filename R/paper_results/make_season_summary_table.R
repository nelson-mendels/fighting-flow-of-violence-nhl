make_season_summary_table <- function(working.dir, min_n = 0) {
  
  # ── Load ──────────────────────────────────────────────────────────────────
  game_level <- readRDS(file.path(working.dir,
                                  "generated/IAT_data/IAT_game_level_data.rds"))
  
  # ── Helpers ───────────────────────────────────────────────────────────────
  fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_num <- function(x, digits = 1) sprintf(paste0("%.", digits, "f"), x)
  
  # ── Wrangle ───────────────────────────────────────────────────────────────
  game_level_filtered <- game_level %>%
    filter(n >= min_n) %>%
    mutate(
      season = as.character(season),
      season_label = paste0(
        substr(season, 1, 4), "--",
        substr(season, 7, 8)
      )
    )
  
  season_stats <- game_level_filtered %>%
    group_by(season_label, season) %>%
    summarise(
      n_games       = n(),
      mean_events   = mean(n,  na.rm = TRUE),
      median_events = median(n, na.rm = TRUE),
      sd_events     = sd(n,    na.rm = TRUE),
      p05           = quantile(n, 0.05, na.rm = TRUE),
      p95           = quantile(n, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(season) %>%
    select(-season)
  
  overall_stats <- game_level_filtered %>%
    summarise(
      season_label  = "Overall",
      n_games       = n(),
      mean_events   = mean(n,  na.rm = TRUE),
      median_events = median(n, na.rm = TRUE),
      sd_events     = sd(n,    na.rm = TRUE),
      p05           = quantile(n, 0.05, na.rm = TRUE),
      p95           = quantile(n, 0.95, na.rm = TRUE)
    )
  
  all_stats <- bind_rows(season_stats, overall_stats)
  
  # ── Build rows ────────────────────────────────────────────────────────────
  table_rows <- all_stats %>%
    mutate(
      row = paste0(
        season_label, " & ",
        fmt_int(n_games), " & ",
        fmt_num(mean_events, 1), " & ",
        fmt_num(median_events, 1), " & ",
        fmt_num(sd_events, 1), " & ",
        fmt_int(p05), " & ",
        fmt_int(p95),
        " \\\\"
      ),
      # Add midrule before Overall row
      row = if_else(season_label == "Overall", paste0("\\midrule\n", row), row)
    ) %>%
    pull(row)
  
  # ── Build LaTeX ───────────────────────────────────────────────────────────
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{NHL games and violent events by season}",
    "\\label{tab:season_summary}",
    "\\small",
    "\\renewcommand{\\arraystretch}{1.15}",
    "\\setlength{\\tabcolsep}{5pt}",
    "",
    "\\begin{tabular*}{\\textwidth}{@{\\extracolsep{\\fill}}lrrrrrr}",
    "\\toprule",
    "& & \\multicolumn{5}{c}{Violent events per game} \\\\",
    "\\cmidrule(lr){3-7}",
    "Season & Games & Mean & Median & Standard deviation & 5th percentile & 95th percentile \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular*}",
    "",
    "\\vspace{0.4em}",
    "\\parbox{\\textwidth}{",
    "\\footnotesize",
    paste0(
      "\\textit{Note:} Each row summarizes the distribution of violent events per game ",
      "within a single season. Means, medians, and standard deviations are rounded to ",
      "the nearest tenth; percentiles are rounded to the nearest whole number."
    ),
    "}",
    "\\end{table}"
  )
  
  # ── Save ──────────────────────────────────────────────────────────────────
  out_dir <- file.path(working.dir, "generated", "tables")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  writeLines(latex_lines, file.path(out_dir, "season_summary_table.tex"))
  
  message("Saved: generated/tables/season_summary_table.tex")
  message("In Overleaf, use:")
  message("\\input{tables/season_summary_table.tex}")
  
  return(all_stats)
}
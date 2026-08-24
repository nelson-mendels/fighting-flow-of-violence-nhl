# Script Guide

## Fighting and the Flow of Violence in the NHL

This guide describes every R script in the current project. Scripts are grouped by their role in the research workflow: master-data construction, game-level interarrival-time analysis, fight analysis and modeling, figure production, and paper-ready tables. All paths below are relative to the repository root.

The project has two entry-point scripts. `R/create_dataset.R` optionally rebuilds the master play-by-play dataset from public NHL data. `R/build_database.R` is the main analysis driver and reproduces the downstream datasets, models, tables, and figures from the frozen master dataset included in the repository.

## Top-level drivers

### `R/create_dataset.R`

Orchestrates construction of the master play-by-play dataset. It loads the data-acquisition packages, establishes the repository root, sources the five scripts in `R/get_data_scripts/`, downloads and cleans the 2010–11 through 2025–26 regular seasons, combines them, and saves `generated/pbp_data/master_data.rds`. Rebuilding is optional because the frozen master file is already included.

### `R/build_database.R`

Runs the complete analysis in dependency order. It loads the frozen master dataset, sources all analysis functions, builds game-level IAT statistics, produces the paper’s exponential and non-exponential examples, constructs the one-fight datasets, fits the pre-audit, post-audit, and pooled models, and creates the LaTeX tables and figures. The roughly 19,000 individual game PDFs remain available as an explicitly commented optional step because they are not inputs to later results.

## Master-data construction

These scripts are sourced by `R/create_dataset.R`. They are needed only when rebuilding the included master dataset from the public NHL source.

### `R/get_data_scripts/get_schedule_info.R`

Retrieves the official regular-season schedule for a requested NHL season from the public NHL API. It queries the franchises active during the study period, removes duplicate games, standardizes game IDs and dates, and converts team abbreviations into season-appropriate full names, including relocations and franchise-name changes. An in-session cache prevents the same season from being requested repeatedly during one pipeline run.

### `R/get_data_scripts/download_data.R`

Downloads regular-season play-by-play data for 2010–11 through 2025–26 with `nhlscraper`. It standardizes column types across seasons, compares the downloaded game IDs with the official schedule, reports missing or extra games, and saves one raw season file under `data/game_logs/`. Existing valid season files can be reused instead of downloaded again.

### `R/get_data_scripts/cleaning_function.R`

Cleans one raw season file into the common structure used by the analysis. It validates the expected source schema, joins schedule dates and team names, standardizes event, timing, score, and penalty variables, classifies penalty severity, flags fights, counts unique fight moments, assigns historically correct divisions, constructs the intradivision indicator, and retains regular-season regulation play.

### `R/get_data_scripts/clean_data.R`

Applies `cleaning_function()` to every season file in `data/game_logs/`. Valid, nonempty results are saved with matching filenames under `data/game_logs_cleaned/`; invalid or empty seasons are reported rather than silently included.

### `R/get_data_scripts/stack_seasons.R`

Reads all cleaned season files from `data/game_logs_cleaned/`, reports any unreadable files, and row-binds the successfully loaded seasons into one play-by-play dataset. The combined object is returned to `R/create_dataset.R`, which writes the final frozen master file.

## Game-level interarrival-time analysis

These scripts define violent-contact events, calculate interarrival-time statistics, and create the game-level diagnostic and paper figures.

### `R/IAT/filter_penalties.R`

Constructs the `VIOLENT_CONTACT` event definition. It combines officially recorded hits with qualifying contact-related penalties, excludes fighting-related infractions so fights can serve as intervention times, handles major, match, double-minor, and penalty-shot classifications, and removes duplicate violent-contact records occurring for the same team at the same game second.

### `R/IAT/calculate_IAT.R`

Calculates interarrival times in minutes for an ordered set of events, estimates the average event rate per game-minute, and evaluates exponential fit with the Anderson–Darling test. It returns the event count, interarrival-time values, estimated rate, and Anderson–Darling statistic and p-value, with explicit missing values when the input is too small or otherwise invalid.

### `R/IAT/build_game_level_IAT_data.R`

Applies the interarrival calculation to every game for the requested event type. It standardizes identifiers, constructs violent-contact events when requested, counts each game’s unique fight moments, and records the event rate, Anderson–Darling p-value, minimum and maximum IAT, event count, and fight count. The resulting game-level dataset is saved as `generated/IAT_data/IAT_game_level_data.rds`.

### `R/IAT/plots/make_game_plot.R`

Builds an empirical cumulative distribution plot for the interarrival times in one game and overlays the fitted exponential distribution. The annotation reports the event count, fitted rate, and Anderson–Darling p-value. The returned object contains both the plot and the underlying statistics so other scripts can reuse the same calculation.

### `R/IAT/plots/make_IAT_example_plots.R`

Creates the exponential and non-exponential game examples used in the paper. It selects representative games from a typical event-count range unless IDs are supplied, builds both figures with `game_plot()`, harmonizes their axes, and saves the PNG files under `plots/VIOLENT_CONTACT/paper/`. It also writes `IAT_example_note.tex` from the selected games’ metadata and statistics so the LaTeX note stays synchronized with the figures.

### `R/IAT/plots/game_level_plots.R`

Generates an individual IAT-distribution PDF for every selected game and event type, with optional season and game filters. Existing files can be skipped or overwritten, and progress is reported during large runs. Because the complete archive contains roughly one file per game and does not feed later analysis, its call remains commented out in the normal driver.

## Fight datasets and statistical modeling

These scripts isolate one-fight games, construct the censored event-level dataset, fit the exponential-rate models, and summarize goodness-of-fit results.

### `R/fights/single_fight_games.R`

Identifies regular-season games with exactly one unique fight time. It records the fight time, available fighter names, score differential, period, and intradivision status; excludes violent-contact events involving the fighters when names are available; and calculates pre- and post-fight event counts, rates, rate changes, and two-sided binomial-test p-values. The output is saved as `generated/fights/single_fight_games.rds`.

### `R/fights/merge_game_and_fight_data.R`

Left-joins the single-fight measures onto the complete game-level IAT dataset using game, season, and date. Games without a qualifying single fight remain in the file, and fight counts are reconciled into one field. The merged result is saved as `generated/fights/game_level_with_single_fight_lambdas.rds`.

### `R/fights/build_fight_penalty_distribution.R`

Counts the number of individual fighting penalties assigned at each unique fight moment, defined by season, game, and game second. It creates the distribution of penalties per fight, records the total number of fight moments and fighting penalties, and saves the summary as `generated/fights/fight_penalty_distribution.rds` for the corresponding paper table.

### `R/fights/build_IAT_table.R`

Constructs the event-level interarrival table for one single-fight game. It inserts the game start and end boundaries, splits the event sequence at the fight, marks the intervals censored by the fight or the end of regulation, identifies pre- versus post-fight observations, records the current period, and flags intervals ending within the specified pre-fight window.

### `R/fights/build_all_IAT_tables.R`

Calls `build_IAT_table()` for every game in the one-fight sample and combines the results. It joins game characteristics, including intradivision status, fight-period score differential, and relative month within the season. The complete event-level modeling file is saved as `generated/fights/single_fight_games_IAT_table.rds`.

### `R/fights/fit_IAT_log_model.R`

Fits the censored exponential interarrival models through their equivalent Poisson likelihood, using log IAT as the exposure offset and game-clustered standard errors. The specifications progressively add period and pre-fight-window terms, season and intradivision controls, month and score-differential interactions, and game fixed effects. For each season range, it saves coefficient estimates, model summaries, average pre- and post-fight rates, delta-method rate ratios, the exact estimation sample, and game fixed effects under `generated/fights/<season_range>/`.

### `R/fights/make_AD_rejection_table.R`

Summarizes the share of whole-game Anderson–Darling tests rejecting exponential interarrival times. It reports all games and games meeting a selected minimum event count for the pooled and component season ranges, then saves `generated/tables/AD_rejection_rates.tex` and prints the corresponding LaTeX input command.

## Fight-analysis figures

These scripts create the season-specific and comparative visual outputs under `plots/fights/`.

### `R/fights/fight_plots/plot_fights_per_game_by_season.R`

Aggregates the game-level data by season and plots the mean number of fights per game from 2010–11 through 2025–26. The line-and-point figure is saved as `plots/fights/fights_per_season.jpg`.

### `R/fights/fight_plots/make_fight_time_distribution_plot.R`

Creates a regulation-time histogram of the unique fight times in one-fight games for a supplied season range. It marks the period boundaries, reports the largest time bins and median fight time, and saves a season-range-specific JPEG under `plots/fights/<season_range>/`.

### `R/fights/fight_plots/make_pre_post_lambda_plots.R`

Creates scatterplots comparing each one-fight game’s estimated pre-fight and post-fight violent-contact rates. For each minimum-event floor, it saves a full-range plot and a common-axis zoomed plot, allowing games above and below the equality line to be compared within each season range.

### `R/fights/fight_plots/make_pre_post_lambda_facet_plot.R`

Combines the pre/post rate scatterplots from multiple non-overlapping season ranges into side-by-side panels with common visual scales. It produces one comparative figure for each event-count floor and view type and saves the combined JPEGs directly under `plots/fights/`.

### `R/fights/fight_plots/make_fight_vs_AD_rejection_plots.R`

Examines whether rejection of exponential violent-event timing varies with the number of fights in a game. Across a sequence of minimum event-count cutoffs, it groups games into zero, one, two, and three-or-more fights, plots the Anderson–Darling rejection rate, and performs a Pearson chi-squared test. The function saves the cutoff-specific plots and a season-range-specific LaTeX table of test results.

### `R/fights/fight_plots/make_fight_vs_AD_rejection_facet_plot.R`

Combines the fight-count versus Anderson–Darling rejection plots from multiple season ranges. For each event-count cutoff, it aligns the panels on a common y-axis and saves a comparative PNG directly under `plots/fights/`.

### `R/fights/fight_plots/make_one_fight_timeline_plot.R`

Creates the paper’s side-by-side timelines for selected one-fight games. It plots violent-contact events across regulation, marks the fight and period boundaries, reports diagnostic event counts, and saves `plots/fights/single_fight_timeline_comparison.png`.

### `R/fights/fight_plots/make_game_fixed_effects_plots.R`

Reads the game fixed effects produced by `fit_IAT_log_model()` for a specified season range. It saves a distribution plot, a fixed-effect-versus-interval-count plot, and a grouped comparison using the selected game characteristic, with intradivision status as the default, under `plots/fights/<season_range>/game_FE/`.

## Paper-ready tables

These scripts convert regenerated datasets and model objects into LaTeX tables under `generated/tables/`. Each function prints the `\input{}` command after saving its output.

### `R/paper_results/make_season_summary_table.R`

Summarizes the number of games and the distribution of violent-contact event counts by season, including the mean, median, standard deviation, fifth percentile, and ninety-fifth percentile. It adds an overall row and saves `generated/tables/season_summary_table.tex`.

### `R/paper_results/make_IAT_summary_table.R`

Creates a compact full-sample summary of the game-level and one-fight IAT datasets. It reports season and game counts, violent-contact totals, fight counts, one-fight observations, IAT summaries, censoring, post-fight observations, and the overall Anderson–Darling rejection rate in `generated/tables/IAT_summary_table.tex`.

### `R/paper_results/make_fight_desc_table.R`

Decomposes fight activity by season. It reports games, total fights, fights per game, and the percentages of games containing zero, one, two, or at least three fights, then saves `generated/tables/fight_desc_table.tex`.

### `R/paper_results/make_fight_penalty_distribution_table.R`

Reads the fight-moment distribution constructed by `build_fight_penalty_distribution.R` and formats it as a LaTeX table showing how many fighting penalties are assigned per fight moment. The output is `generated/tables/fight_penalty_distribution.tex`.

### `R/paper_results/make_one_fight_sample.R`

Describes the exact estimation sample saved by `fit_IAT_log_model()` rather than the broader raw one-fight file. For each season range, it reports game and IAT counts, pre- and post-fight observations, censoring, divisional matchups, and IAT means, then saves `one_fight_sample_<season_range>.tex`.

### `R/paper_results/make_IAT_regression_table.R`

Reads the season-range-specific coefficient and model-summary files and formats the four displayed exponential-rate specifications. It creates the main IAT regression table and separate period, season, and month effect tables, preserving the paper’s labels, notes, and model ordering.

### `R/paper_results/make_post_fight_rate_ratio_tables.R`

Formats the modeled pre-fight rates, post-fight rates, and post-to-pre rate ratios for the displayed specifications. It also creates the season-control results table using the model outputs and season-level one-fight game counts. Both outputs are written to the applicable season-range folder under `generated/tables/`.

### `R/paper_results/single_fight_AD_tables.R`

Recomputes Anderson–Darling tests separately before and after the fight in one-fight games, using the same exponential-test logic as the whole-game analysis. For each minimum-event threshold from 15 through 25, it summarizes rejection patterns and saves a threshold-specific LaTeX table under `generated/tables/<season_range>/one_fight_AD_rejection/`.

## Normal execution order

For exact reproduction of the current analysis, open `flow_of_violence_nhl.Rproj` and run `R/build_database.R` from the repository root. The included frozen master dataset is the only committed data input required by this step.

Run `R/create_dataset.R` only when intentionally rebuilding the master dataset from the public NHL source. Because upstream records and package behavior can change, a newly downloaded master dataset may not be byte-for-byte identical to the frozen file used for the current results.

# Builds the analysis datasets, figures, models, and LaTeX tables.
# Run this script from the repository root.

# Packages ----------------------------------------------------------------
library(tidyverse)
library(goftest)
library(grid)
library(scales)
library(fixest)
library(patchwork)

# Project setup -----------------------------------------------------------
# Use the cloned repository's root on any computer. The trailing slash is
# retained for compatibility with the existing analysis functions.
working.dir <- paste0(normalizePath(".", winslash = "/", mustWork = TRUE), "/")
scripts_dir <- file.path(working.dir, "R")
master_file <- file.path(working.dir, "generated", "pbp_data", "master_data.rds")

if (!dir.exists(file.path(scripts_dir, "IAT")) ||
    !dir.exists(file.path(scripts_dir, "fights")) ||
    !dir.exists(file.path(scripts_dir, "paper_results"))) {
  stop("Could not find the analysis scripts. Run R/build_database.R from the repository root.")
}

if (!file.exists(master_file)) {
  stop("Could not find generated/pbp_data/master_data.rds. Run R/create_dataset.R first.")
}

# Analysis functions -----------------------------------------------------
# Game-level interarrival-time analysis
source(file.path(scripts_dir, "IAT", "filter_penalties.R"))
source(file.path(scripts_dir, "IAT", "calculate_IAT.R"))
source(file.path(scripts_dir, "IAT", "build_game_level_IAT_data.R"))
source(file.path(scripts_dir, "IAT", "plots", "game_level_plots.R"))
source(file.path(scripts_dir, "IAT", "plots", "make_game_plot.R"))
source(file.path(scripts_dir, "IAT", "plots", "make_IAT_example_plots.R"))

# Fight datasets, models, and summaries
source(file.path(scripts_dir, "fights", "single_fight_games.R"))
source(file.path(scripts_dir, "fights", "merge_game_and_fight_data.R"))
source(file.path(scripts_dir, "fights", "build_fight_penalty_distribution.R"))
source(file.path(scripts_dir, "fights", "build_IAT_table.R"))
source(file.path(scripts_dir, "fights", "build_all_IAT_tables.R"))
source(file.path(scripts_dir, "fights", "fit_IAT_log_model.R"))
source(file.path(scripts_dir, "fights", "make_AD_rejection_table.R"))

# Fight figures
source(file.path(scripts_dir, "fights", "fight_plots", "make_one_fight_timeline_plot.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "plot_fights_per_game_by_season.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_fight_vs_AD_rejection_plots.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_pre_post_lambda_plots.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_fight_time_distribution_plot.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_pre_post_lambda_facet_plot.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_fight_vs_AD_rejection_facet_plot.R"))
source(file.path(scripts_dir, "fights", "fight_plots", "make_game_fixed_effects_plots.R"))

# Paper tables
source(file.path(scripts_dir, "paper_results", "make_IAT_regression_table.R"))
source(file.path(scripts_dir, "paper_results", "make_post_fight_rate_ratio_tables.R"))
source(file.path(scripts_dir, "paper_results", "make_one_fight_sample.R"))
source(file.path(scripts_dir, "paper_results", "make_season_summary_table.R"))
source(file.path(scripts_dir, "paper_results", "make_IAT_summary_table.R"))
source(file.path(scripts_dir, "paper_results", "make_fight_desc_table.R"))
source(file.path(scripts_dir, "paper_results", "single_fight_AD_tables.R"))
source(file.path(scripts_dir, "paper_results", "make_fight_penalty_distribution_table.R"))

# Load master data --------------------------------------------------------
pbp_master <- readRDS(master_file)
events <- "VIOLENT_CONTACT"

# Step 1: game-level IAT analysis ----------------------------------------
build_game_level_IAT_data(working.dir, pbp_master, events)

# Optional: generate a PDF for every game (about 19,000 files). These plots
# are not inputs to later analysis, so leave this commented out for a normal
# reproducibility run. Uncomment it when the full game-level plot archive is wanted.
# game_level_plots(working.dir, pbp_master, events)

# Generate the exponential and non-exponential IAT examples used in the paper.
iat_examples <- make_IAT_example_plots(
  working.dir = working.dir,
  pbp_master = pbp_master,
  overwrite = TRUE
)

# Step 2: fight analysis --------------------------------------------------
single_fight_games(working.dir, pbp_master, events)
merge_game_and_fight_data(working.dir)
build_fight_penalty_distribution(working.dir, pbp_master)
build_all_IAT_tables(working.dir, pbp_master, events)

# Load generated analysis data -------------------------------------------
game_data <- readRDS(file.path(working.dir, "generated", "IAT_data", "IAT_game_level_data.rds"))

game_and_fight <- readRDS(file.path(
  working.dir, "generated", "fights", "game_level_with_single_fight_lambdas.rds"
)) %>%
  mutate(lambda_difference_VIOLENT_CONTACT = lambda_change_after_minus_before_VIOLENT_CONTACT)

single_fight <- readRDS(file.path(
  working.dir, "generated", "fights", "single_fight_games.rds"
))

# Analysis samples --------------------------------------------------------
# NHL hit reporting changed after the league's 2024 audit. Keep the two eras
# separate for comparisons and retain the pooled sample for full-period results.
pbp_10_24 <- pbp_master %>% filter(!season %in% c(20242025, 20252026))
pbp_24_26 <- pbp_master %>% filter(season %in% c(20242025, 20252026))

analysis_samples <- list(
  pre_audit = pbp_10_24,
  post_audit = pbp_24_26,
  pooled = pbp_master
)

facet_sample_names <- c("pre_audit", "post_audit")
range_results <- vector("list", length(analysis_samples))
names(range_results) <- names(analysis_samples)

# Run the same analysis for each season range ----------------------------
for (sample_name in names(analysis_samples)) {
  pbp_master_used <- analysis_samples[[sample_name]]

  fight_vs_AD_rejection <- make_fight_vs_AD_rejection_plots(
    working.dir = working.dir,
    game_and_fight = game_and_fight,
    pbp_master_used = pbp_master_used,
    event_used = "VIOLENT_CONTACT",
    cutoffs = c(0, 10, 20, 30, 40, 50, 60, 70),
    alpha = 0.05
  )

  make_fight_time_distribution_plot(working.dir, pbp_master_used)
  pre_post_lambda <- make_pre_post_lambda_plots(working.dir, single_fight, pbp_master_used)
  model_results <- fit_IAT_log_model(working.dir, pbp_master_used)
  make_one_fight_sample(working.dir, pbp_master_used)
  make_IAT_regression_table(working.dir, pbp_master_used)
  make_post_fight_rate_ratio_tables(working.dir, pbp_master_used)

  one_fight_AD <- single_fight_AD_tables(
    working.dir = working.dir,
    pbp_master_used = pbp_master_used,
    event_name = "VIOLENT_CONTACT",
    min_events_range = 15:25,
    alpha = 0.05
  )

  make_game_fixed_effects_plots(working.dir, pbp_master_used)

  range_results[[sample_name]] <- list(
    fight_vs_AD_rejection = fight_vs_AD_rejection,
    pre_post_lambda = pre_post_lambda,
    model_results = model_results,
    one_fight_AD = one_fight_AD
  )
}

# Full-sample paper tables and figures -----------------------------------
make_season_summary_table(working.dir)
make_IAT_summary_table(working.dir)
make_fight_desc_table(working.dir)
make_fight_penalty_distribution_table(working.dir)
plot_fights_per_game_by_season(working.dir, game_data)

fight_timeline_plot <- make_one_fight_timeline_plot(
  working.dir = working.dir,
  pbp_master = pbp_master,
  game_ids = c("2014020425", "2017020983"),
  panel_labels = c(
    "Significant post-fight decrease",
    "No significant post-fight rate change"
  )
)

make_AD_rejection_table(
  working.dir = working.dir,
  game_data = game_data,
  data_coverage = analysis_samples,
  cutoff = 50,
  alpha = 0.05
)

# Comparative figures use the two non-pooled samples.
if (length(facet_sample_names) >= 2) {
  lambda_results <- lapply(
    range_results[facet_sample_names],
    function(x) x$pre_post_lambda
  )
  ad_results <- lapply(
    range_results[facet_sample_names],
    function(x) x$fight_vs_AD_rejection
  )
  data_to_facet <- analysis_samples[facet_sample_names]

  make_pre_post_lambda_facet_plot(working.dir, lambda_results, data_to_facet)
  make_fight_vs_AD_rejection_facet_plot(working.dir, ad_results, data_to_facet)
}

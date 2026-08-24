# Fighting and the Flow of Violence in the NHL

This repository contains the data-construction, analysis, modeling, table, and figure code for a study of how fighting relates to the timing of violent contact in National Hockey League games. The project uses regular-season play-by-play data from 2010–11 through 2025–26 and models the interarrival times between violent-contact events before and after fights.

The repository reflects the full research workflow rather than a paper-specific rewrite. It includes the analyses used in the paper as well as additional work developed during the broader project.

## Reproducibility at a glance

The repository includes the frozen master dataset used by the current analysis:

`generated/pbp_data/master_data.rds`

That file is the only precomputed dataset required by the main analysis. Running `R/build_database.R` reconstructs the downstream datasets, statistical models, LaTeX tables, and analysis figures.

The master dataset can also be rebuilt from public NHL data by running `R/create_dataset.R`. Because public APIs and source packages can change, use the included frozen master dataset when the goal is to reproduce the current results exactly.

## Repository structure

```text
.
├── flow_of_violence_nhl.Rproj
├── README.md
├── SCRIPT_GUIDE.md
├── R/
│   ├── create_dataset.R
│   ├── build_database.R
│   ├── get_data_scripts/
│   ├── IAT/
│   ├── fights/
│   └── paper_results/
├── generated/
│   ├── pbp_data/master_data.rds
│   ├── IAT_data/
│   ├── fights/
│   └── tables/
└── plots/
    ├── VIOLENT_CONTACT/paper/
    └── fights/
```

- `R/get_data_scripts/` downloads, validates, cleans, and stacks season-level NHL play-by-play data.
- `R/IAT/` defines violent-contact events and constructs game-level interarrival-time statistics.
- `R/fights/` creates the fight samples, fits the statistical models, and produces fight-related figures.
- `R/paper_results/` converts analysis results into paper-ready LaTeX tables.
- `generated/pbp_data/` contains the frozen analysis input.
- `generated/IAT_data/` and `generated/fights/` receive reproducible intermediate and model outputs.
- `generated/tables/` contains the generated LaTeX tables.
- `plots/VIOLENT_CONTACT/paper/` and `plots/fights/` contain the paper and contextual analysis figures.

For a file-by-file explanation of all 36 R scripts, see [`SCRIPT_GUIDE.md`](SCRIPT_GUIDE.md).

## Requirements

The code is written in R and is designed to run from the repository root on macOS, Windows, or Linux. Opening `flow_of_violence_nhl.Rproj` in RStudio sets the correct project root.

Install the required packages once:

```r
install.packages(c(
  "tidyverse",
  "nhlscraper",
  "jsonlite",
  "goftest",
  "scales",
  "fixest",
  "patchwork"
))
```

The current repository was checked with R 4.6.1 and these directly used package versions:

```text
tidyverse  2.0.0
nhlscraper 0.7.0
jsonlite   2.0.0
goftest    1.2-3
scales     1.4.0
fixest     0.14.2
patchwork  1.3.2
```

`grid`, which is also loaded by the analysis driver, is included with R.

## Reproduce the current analysis

### 1. Clone or download the repository

Open `flow_of_violence_nhl.Rproj`, or set the command-line working directory to the repository root. The scripts use project-relative paths and do not depend on a particular user name or computer.

### 2. Use the included frozen master dataset

Confirm that this file is present:

```text
generated/pbp_data/master_data.rds
```

The included file contains 5,886,581 play-by-play rows and 26 variables covering 16 regular seasons from 2010–11 through 2025–26. Its SHA-256 checksum is:

```text
6a7f67dac3ab5b6070c6d465a22a2efbbf2733e3f483fc124e5fb73159aafb9c
```

### 3. Run the analysis driver

From the repository root, run either:

```r
source("R/build_database.R")
```

or:

```bash
Rscript R/build_database.R
```

The driver performs the workflow in dependency order:

1. constructs game-level interarrival-time statistics;
2. creates the paper’s exponential and non-exponential IAT examples;
3. identifies one-fight games and calculates pre- and post-fight measures;
4. builds the event-level censored IAT dataset;
5. runs the pre-audit, post-audit, and pooled statistical models;
6. saves model results and fixed effects;
7. creates LaTeX tables and analysis figures.

The pre-audit sample covers 2010–11 through 2023–24, the post-audit sample covers 2024–25 through 2025–26, and the pooled sample covers the full study period.

## Rebuild the master dataset from public data

Rebuilding the master dataset is optional because the frozen file is included. To reconstruct it from the public source, run:

```r
source("R/create_dataset.R")
```

or:

```bash
Rscript R/create_dataset.R
```

This pipeline:

1. downloads regular-season play-by-play data with `nhlscraper`;
2. validates downloaded games against the public NHL schedule API;
3. saves raw season files under `data/game_logs/`;
4. cleans and standardizes them under `data/game_logs_cleaned/`;
5. combines the seasons and replaces `generated/pbp_data/master_data.rds`.

The raw and cleaned season files are intentionally not versioned because they are large, reproducible intermediates. A newly downloaded master dataset may differ from the frozen file if the NHL source data or package behavior has been revised.

## Data construction and analysis

Violent-contact events combine officially recorded hits with qualifying contact-related penalties. Fighting penalties are identified separately so that fights define the intervention times rather than being counted as violent-contact outcomes.

`R/IAT/calculate_IAT.R` converts ordered event times into interarrival times, estimates the event rate, and applies the Anderson–Darling exponential goodness-of-fit test. `R/IAT/build_game_level_IAT_data.R` repeats this process for each game.

The primary fight analysis focuses on games with exactly one unique fight time. `R/fights/build_all_IAT_tables.R` constructs censored pre- and post-fight intervals, and `R/fights/fit_IAT_log_model.R` estimates the exponential-rate models using the equivalent Poisson likelihood with game-clustered standard errors and progressively richer controls.

## Included and excluded outputs

The repository includes:

- all current fight-analysis plots under `plots/fights/`;
- the paper’s two IAT example figures and accompanying LaTeX note under `plots/VIOLENT_CONTACT/paper/`;
- the generated LaTeX tables under `generated/tables/`.

The full game-by-game plot archive is intentionally excluded. The historical working archive contains more than 68,000 PDFs and exceeds 700 MB because it includes earlier event types and legacy runs; the current optional generator would still create roughly one PDF per analyzed game. These PDFs do not feed any dataset, model, table, or paper figure, so including them would make the repository much slower to clone without improving reproducibility.

The generator remains available in `R/IAT/plots/game_level_plots.R`. To recreate the current game-level archive, uncomment this line in `R/build_database.R` before running the driver:

```r
game_level_plots(working.dir, pbp_master, events)
```

Intermediate RDS files under `generated/IAT_data/` and `generated/fights/` are likewise excluded from version control because `R/build_database.R` recreates them from the included master dataset. The directories are retained so their role in the workflow is visible.

## Main outputs

Running `R/build_database.R` produces:

- `generated/IAT_data/IAT_game_level_data.rds`;
- `generated/fights/single_fight_games.rds`;
- `generated/fights/game_level_with_single_fight_lambdas.rds`;
- `generated/fights/single_fight_games_IAT_table.rds`;
- season-range-specific coefficient, model-summary, rate-ratio, estimation-sample, and fixed-effect RDS files;
- LaTeX tables under `generated/tables/`;
- fight and IAT figures under `plots/`.

Each table-producing function prints the corresponding LaTeX `\input{}` path after saving its output.

## Public data and provenance

Play-by-play data were retrieved from publicly available NHL sources using the `nhlscraper` R package. Schedule information was retrieved separately from the NHL’s public schedule API. The data-construction code documents the transformations from these sources to the frozen analysis dataset, including regular-season and regulation-period restrictions, event standardization, fight identification, penalty classification, historical team names, and division membership.

The frozen master dataset is included to make the current results directly reproducible even if the upstream NHL data or nhlscraper package behavior is later corrected or reorganized. 

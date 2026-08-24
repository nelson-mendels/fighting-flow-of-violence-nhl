# Cleans one season of raw nhlscraper play-by-play data and converts it into
# the standardized format used throughout the downstream analysis.

cleaning_function <- function(rds_file) {
  # 1. Parse season from the expected filename: pbp_YYYY-YYYY.rds.
  file_name <- basename(rds_file)
  file_season <- str_extract(file_name, "(?<=pbp_)[0-9]{4}-[0-9]{4}")

  if (is.na(file_season)) {
    stop("Could not parse season from file name: ", file_name)
  }

  season_std <- gsub("-", "", file_season)

  # 2. Read raw play-by-play data and validate its required fields.
  pbp <- readRDS(rds_file)

  required_columns <- c(
    "gameId", "eventTypeDescKey", "penaltyTypeDescKey", "isHome",
    "periodNumber", "secondsElapsedInPeriod", "periodType",
    "secondsElapsedInGame", "homeGoals", "awayGoals", "penaltyDuration"
  )
  missing_columns <- setdiff(required_columns, names(pbp))

  if (length(missing_columns) > 0) {
    stop(
      "Unexpected schema in ", file_name, "; missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # 3. Retrieve official game dates and standardized team names.
  schedule_info <- get_schedule_info(season_std)

  # 4. Standardize variables.
  # Joins schedule metadata and creates the event, timing, score, and penalty
  # variables used in the analysis.

  pbp <- pbp %>%
    mutate(gameId = as.character(gameId)) %>%
    left_join(schedule_info, by = c("gameId" = "game_id_full")) %>%
    mutate(
      game_id = gameId,
      game_date = as.Date(game_date),

      # Standardize event names
      event_type = case_when(
        eventTypeDescKey == "shot-on-goal" ~ "SHOT",
        TRUE ~ toupper(gsub("-", "_", coalesce(eventTypeDescKey, "")))
      ),
      event = event_type,

      # Use the specific infraction as the description for penalties
      description = if_else(
        eventTypeDescKey == "penalty",
        as.character(penaltyTypeDescKey),
        as.character(eventTypeDescKey)
      ),

      # Identify the team responsible for each event
      event_team = if_else(as.logical(isHome), home_name, away_name),
      event_team_type = if_else(as.logical(isHome), "home", "away"),

      # Construct elapsed and remaining time variables
      period = as.integer(periodNumber),
      period_seconds = as.integer(secondsElapsedInPeriod),
      period_seconds_remaining = case_when(
        toupper(periodType) == "REG" ~
          1200L - as.integer(secondsElapsedInPeriod),
        toupper(periodType) == "OT" ~
          300L - as.integer(secondsElapsedInPeriod),
        TRUE ~ NA_integer_
      ),
      game_seconds = as.integer(secondsElapsedInGame),
      game_seconds_remaining = case_when(
        as.integer(periodNumber) <= 3L ~
          3600L - as.integer(secondsElapsedInGame),
        toupper(periodType) == "OT" ~
          3900L - as.integer(secondsElapsedInGame),
        TRUE ~ NA_integer_
      ),

      # Standardize score variables
      home_score = as.integer(homeGoals),
      away_score = as.integer(awayGoals),

      # penaltyDuration is measured in minutes
      penalty_minutes = if_else(
        !is.na(penaltyDuration),
        as.integer(round(as.numeric(penaltyDuration))),
        NA_integer_
      ),

      # Classify penalties by severity
      penalty_severity = case_when(
        str_detect(coalesce(penaltyTypeDescKey, ""),
                   regex("^ps-", ignore_case = TRUE)) ~ "Penalty Shot",
        str_detect(coalesce(penaltyTypeDescKey, ""),
                   regex("game-misconduct", ignore_case = TRUE)) ~ "Game Misconduct",
        str_detect(coalesce(penaltyTypeDescKey, ""),
                   regex("match-penalty", ignore_case = TRUE)) ~ "Match",
        penaltyTypeDescKey %in% c(
          "too-many-men-on-the-ice",
          "delaying-game-unsuccessful-challenge",
          "unsportsmanlike-conduct-bench",
          "bench",
          "delaying-game-bench",
          "delaying-game-bench-face-off-violation",
          "interference-bench"
        ) ~ "Bench Minor",
        penalty_minutes == 5 ~ "Major",
        penalty_minutes == 10 ~ "Misconduct",
        penalty_minutes %in% c(2, 4) ~ "Minor",
        TRUE ~ NA_character_
      ),

      season = season_std,
      game_id_full = game_id
    )

  # 5. Build historical division lookup
  # Division membership changed several times during the sample, including
  # the 2013 realignment, expansion to Vegas and Seattle, the temporary
  # 2020–21 divisions, and the relocation of Arizona to Utah.

  make_div <- function(seasons, division, teams) {
    crossing(season = seasons, team_name = teams) %>%
      mutate(division_name = division)
  }

  # Season groupings based on league alignment
  era1 <- c("20102011", "20112012", "20122013")
  era2 <- c("20132014", "20142015", "20152016", "20162017")
  era3 <- c("20172018", "20182019", "20192020")
  era4 <- "20202021"
  era5 <- c("20212022", "20222023", "20232024")
  era6a <- "20242025"
  era6b <- "20252026"

  # Divisions that remain unchanged following the 2013 realignment
  atlantic8 <- c(
    "Boston Bruins",
    "Buffalo Sabres",
    "Detroit Red Wings",
    "Florida Panthers",
    "Montreal Canadiens",
    "Ottawa Senators",
    "Tampa Bay Lightning",
    "Toronto Maple Leafs"
  )

  metro8 <- c(
    "Carolina Hurricanes",
    "Columbus Blue Jackets",
    "New Jersey Devils",
    "New York Islanders",
    "New York Rangers",
    "Philadelphia Flyers",
    "Pittsburgh Penguins",
    "Washington Capitals"
  )

  division_lookup <- bind_rows(

    # 2010–11 through 2012–13: six-division alignment
    make_div(
      era1,
      "Northeast",
      c(
        "Boston Bruins",
        "Buffalo Sabres",
        "Montreal Canadiens",
        "Ottawa Senators",
        "Toronto Maple Leafs"
      )
    ),
    make_div(
      era1,
      "Atlantic",
      c(
        "New Jersey Devils",
        "New York Islanders",
        "New York Rangers",
        "Philadelphia Flyers",
        "Pittsburgh Penguins"
      )
    ),
    make_div(
      era1,
      "Southeast",
      c(
        "Winnipeg Jets",
        "Carolina Hurricanes",
        "Florida Panthers",
        "Tampa Bay Lightning",
        "Washington Capitals"
      )
    ),
    make_div(
      era1,
      "Central",
      c(
        "Chicago Blackhawks",
        "Columbus Blue Jackets",
        "Detroit Red Wings",
        "Nashville Predators",
        "St. Louis Blues"
      )
    ),
    make_div(
      era1,
      "Northwest",
      c(
        "Calgary Flames",
        "Colorado Avalanche",
        "Edmonton Oilers",
        "Minnesota Wild",
        "Vancouver Canucks"
      )
    ),
    make_div(
      era1,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Dallas Stars",
        "Los Angeles Kings",
        "Arizona Coyotes",
        "San Jose Sharks"
      )
    ),

    # 2013–14 through 2016–17: four-division realignment before Vegas
    make_div(era2, "Atlantic", atlantic8),
    make_div(era2, "Metropolitan", metro8),
    make_div(
      era2,
      "Central",
      c(
        "Chicago Blackhawks",
        "Colorado Avalanche",
        "Dallas Stars",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era2,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Arizona Coyotes",
        "Calgary Flames",
        "Edmonton Oilers",
        "Los Angeles Kings",
        "San Jose Sharks",
        "Vancouver Canucks"
      )
    ),

    # 2017–18 through 2019–20: Vegas added to the Pacific Division
    make_div(era3, "Atlantic", atlantic8),
    make_div(era3, "Metropolitan", metro8),
    make_div(
      era3,
      "Central",
      c(
        "Chicago Blackhawks",
        "Colorado Avalanche",
        "Dallas Stars",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era3,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Arizona Coyotes",
        "Calgary Flames",
        "Edmonton Oilers",
        "Los Angeles Kings",
        "San Jose Sharks",
        "Vancouver Canucks",
        "Vegas Golden Knights"
      )
    ),

    # 2020–21: temporary COVID-era North, East, Central, and West divisions
    make_div(
      era4,
      "North",
      c(
        "Calgary Flames",
        "Edmonton Oilers",
        "Montreal Canadiens",
        "Ottawa Senators",
        "Toronto Maple Leafs",
        "Vancouver Canucks",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era4,
      "East",
      c(
        "Boston Bruins",
        "Buffalo Sabres",
        "New Jersey Devils",
        "New York Islanders",
        "New York Rangers",
        "Philadelphia Flyers",
        "Pittsburgh Penguins",
        "Washington Capitals"
      )
    ),
    make_div(
      era4,
      "Central",
      c(
        "Carolina Hurricanes",
        "Chicago Blackhawks",
        "Columbus Blue Jackets",
        "Dallas Stars",
        "Detroit Red Wings",
        "Florida Panthers",
        "Nashville Predators",
        "Tampa Bay Lightning"
      )
    ),
    make_div(
      era4,
      "West",
      c(
        "Anaheim Ducks",
        "Arizona Coyotes",
        "Colorado Avalanche",
        "Los Angeles Kings",
        "Minnesota Wild",
        "San Jose Sharks",
        "St. Louis Blues",
        "Vegas Golden Knights"
      )
    ),

    # 2021–22 through 2023–24: Seattle added to the Pacific Division
    make_div(era5, "Atlantic", atlantic8),
    make_div(era5, "Metropolitan", metro8),
    make_div(
      era5,
      "Central",
      c(
        "Arizona Coyotes",
        "Chicago Blackhawks",
        "Colorado Avalanche",
        "Dallas Stars",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era5,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Calgary Flames",
        "Edmonton Oilers",
        "Los Angeles Kings",
        "San Jose Sharks",
        "Seattle Kraken",
        "Vancouver Canucks",
        "Vegas Golden Knights"
      )
    ),

    # 2024–25: Utah Hockey Club replaces the Arizona Coyotes
    make_div(era6a, "Atlantic", atlantic8),
    make_div(era6a, "Metropolitan", metro8),
    make_div(
      era6a,
      "Central",
      c(
        "Utah Hockey Club",
        "Chicago Blackhawks",
        "Colorado Avalanche",
        "Dallas Stars",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era6a,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Calgary Flames",
        "Edmonton Oilers",
        "Los Angeles Kings",
        "San Jose Sharks",
        "Seattle Kraken",
        "Vancouver Canucks",
        "Vegas Golden Knights"
      )
    ),

    # 2025–26: Utah Hockey Club renamed the Utah Mammoth
    make_div(era6b, "Atlantic", atlantic8),
    make_div(era6b, "Metropolitan", metro8),
    make_div(
      era6b,
      "Central",
      c(
        "Utah Mammoth",
        "Chicago Blackhawks",
        "Colorado Avalanche",
        "Dallas Stars",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Winnipeg Jets"
      )
    ),
    make_div(
      era6b,
      "Pacific",
      c(
        "Anaheim Ducks",
        "Calgary Flames",
        "Edmonton Oilers",
        "Los Angeles Kings",
        "San Jose Sharks",
        "Seattle Kraken",
        "Vancouver Canucks",
        "Vegas Golden Knights"
      )
    )
  )

  # 6. Assign home and away divisions
  # Creates one division record for each game before joining the information
  # back onto the event-level dataset.

  home_lookup <- division_lookup %>%
    rename(home_name = team_name, home_division_name = division_name)

  away_lookup <- division_lookup %>%
    rename(away_name = team_name, away_division_name = division_name)

  game_divisions <- pbp %>%
    distinct(season, game_id_full, home_name, away_name) %>%
    left_join(home_lookup, by = c("season", "home_name")) %>%
    left_join(away_lookup, by = c("season", "away_name")) %>%
    select(game_id_full, home_division_name, away_division_name)

  # 7. Retain analysis variables and identify fights
  # Restricts the data to regular-season regulation events and creates the
  # game-level identifier used throughout the analysis.

  pbp_clean <- pbp %>%
    select(
      season,
      game_id_full,
      game_id,
      game_date,
      event_type,
      event,
      description,
      event_team,
      event_team_type,
      period,
      period_seconds,
      period_seconds_remaining,
      game_seconds,
      game_seconds_remaining,
      home_score,
      away_score,
      home_name,
      away_name,
      penalty_minutes,
      penalty_severity
    ) %>%
    filter(str_sub(game_id_full, 5, 6) == "02", period <= 3) %>%
    mutate(
      fight_flag = as.integer(
        str_detect(
          coalesce(description, ""),
          regex("fighting", ignore_case = TRUE)
        )
      ),
      game_date = as.Date(game_date),
      game_id_short = as.numeric(str_sub(game_id_full, -4))
    )

  # 8. Count fights per game
  # Multiple fighting penalties recorded at the same game second are treated
  # as one fight.

  fights_per_game <- pbp_clean %>%
    filter(fight_flag == 1) %>%
    distinct(season, game_id_full, game_seconds) %>%
    count(season, game_id_full, name = "n_fights")

  pbp_clean <- pbp_clean %>%
    left_join(fights_per_game, by = c("season", "game_id_full")) %>%
    mutate(n_fights = coalesce(n_fights, 0L))

  # 9. Add division information and intradivision indicator
  # intradivision equals 1 when the home and away teams belong to the same
  # division and 0 otherwise.

  pbp_clean <- pbp_clean %>%
    left_join(game_divisions, by = "game_id_full") %>%
    mutate(intradivision = as.integer(home_division_name == away_division_name))

  # 10. Sort final data and run validation checks

  pbp_clean <- pbp_clean %>%
    arrange(season, game_id_full, game_seconds)

  if (anyNA(pbp_clean$season)) {
    warning("NA values in season column.")
  }

  if (anyNA(pbp_clean$home_name) || anyNA(pbp_clean$away_name)) {
    warning("NA team names - check schedule join for ", file_name)
  }

  if (anyNA(pbp_clean$home_division_name) ||
      anyNA(pbp_clean$away_division_name)) {
    warning("NA division values - check division lookup for ", file_name)
  }

  pbp_clean
}

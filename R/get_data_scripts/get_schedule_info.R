# Retrieves regular-season schedules from the NHL public API and standardizes
# game identifiers, dates, abbreviations, and season-aware team names.

# Franchise tricodes appearing during the study period
all_tricodes <- c(
  "ANA", "ARI", "ATL", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI",
  "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NJD",
  "NSH", "NYI", "NYR", "OTT", "PHI", "PHX", "PIT", "SEA", "SJS",
  "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WPG", "WSH"
)

# Avoid requesting the same season twice during one pipeline run.
.schedule_cache <- new.env(parent = emptyenv())

# Convert team abbreviations to standardized, season-aware names
abbr_to_name <- function(abbr, season_std) {
  base <- c(
    ANA = "Anaheim Ducks",
    BOS = "Boston Bruins",
    BUF = "Buffalo Sabres",
    CAR = "Carolina Hurricanes",
    CBJ = "Columbus Blue Jackets",
    CGY = "Calgary Flames",
    CHI = "Chicago Blackhawks",
    COL = "Colorado Avalanche",
    DAL = "Dallas Stars",
    DET = "Detroit Red Wings",
    EDM = "Edmonton Oilers",
    FLA = "Florida Panthers",
    LAK = "Los Angeles Kings",
    MIN = "Minnesota Wild",
    MTL = "Montreal Canadiens",
    NJD = "New Jersey Devils",
    NSH = "Nashville Predators",
    NYI = "New York Islanders",
    NYR = "New York Rangers",
    OTT = "Ottawa Senators",
    PHI = "Philadelphia Flyers",
    PIT = "Pittsburgh Penguins",
    SEA = "Seattle Kraken",
    SJS = "San Jose Sharks",
    STL = "St. Louis Blues",
    TBL = "Tampa Bay Lightning",
    TOR = "Toronto Maple Leafs",
    VAN = "Vancouver Canucks",
    VGK = "Vegas Golden Knights",
    WSH = "Washington Capitals",
    WPG = "Winnipeg Jets",

    # Standardize relocated and renamed franchises
    ATL = "Winnipeg Jets",
    ARI = "Arizona Coyotes",
    PHX = "Arizona Coyotes"
  )

  utah <- if (season_std >= "20252026") {
    "Utah Mammoth"
  } else {
    "Utah Hockey Club"
  }

  out <- unname(base[abbr])
  out[abbr == "UTA"] <- utah

  out
}

# Retrieve and standardize one season of schedule data
get_schedule_info <- function(season_std) {
  season_std <- as.character(season_std)

  if (exists(season_std, envir = .schedule_cache, inherits = FALSE)) {
    return(get(season_std, envir = .schedule_cache, inherits = FALSE))
  }

  fetch_one <- function(tricode) {
    url <- paste0(
      "https://api-web.nhle.com/v1/club-schedule-season/",
      tricode,
      "/",
      season_std
    )

    out <- tryCatch(
      fromJSON(url),
      error = function(e) NULL
    )

    if (is.null(out) || is.null(out$games) || NROW(out$games) == 0) {
      return(NULL)
    }

    g <- out$games

    tibble(
      game_id_full = as.character(g$id),
      game_type = as.integer(g$gameType),
      game_date = as.Date(g$gameDate),
      home_abbr = as.character(g$homeTeam$abbrev),
      away_abbr = as.character(g$awayTeam$abbrev)
    )
  }

  sched <- map(all_tricodes, fetch_one) %>%
    compact() %>%
    bind_rows() %>%
    distinct(game_id_full, .keep_all = TRUE) %>%
    filter(game_type == 2L) %>%
    mutate(
      home_name = abbr_to_name(home_abbr, season_std),
      away_name = abbr_to_name(away_abbr, season_std)
    ) %>%
    select(
      game_id_full,
      game_date,
      home_abbr,
      away_abbr,
      home_name,
      away_name
    )

  if (nrow(sched) == 0) {
    stop("Schedule fetch returned no games for season ", season_std)
  }

  unknown_abbr <- unique(c(
    sched$home_abbr[is.na(sched$home_name)],
    sched$away_abbr[is.na(sched$away_name)]
  ))

  if (length(unknown_abbr) > 0) {
    stop(
      "No standardized team name for: ",
      paste(unknown_abbr, collapse = ", ")
    )
  }

  assign(season_std, sched, envir = .schedule_cache)
  sched
}

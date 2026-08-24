# Calculates interarrival times and estimates the event rate for a supplied dataset.
# It also evaluates exponential fit using the Anderson–Darling statistic.

calculate_IAT <- function(filtered_data) {
  
  # Calculate inter-arrival times in minutes
  df <- filtered_data %>%
    mutate(
      IAT = c(
        season_seconds[1] / 60,
        diff(season_seconds) / 60
      )
    )
  
  total_events <- nrow(df)
  
  total_games <- filtered_data %>%
    summarise(n = n_distinct(game_num)) %>%
    pull(n)
  
  # Estimate the average number of events per game-minute
  lambda <- total_events / (total_games * 60)
  
  # Confirm that the input can support the goodness-of-fit tests
  valid_data <- length(df$IAT) > 1 &&
    is.finite(lambda) &&
    lambda > 0
  
  if (!valid_data) {
    n <- NA
    IAT_values <- df$IAT
    ad_raw_p <- NA
  } else {
    n <- total_events
    
    # Anderson–Darling test against the fitted exponential distribution
    ad_test <- ad.test(
      df$IAT,
      null = "pexp",
      rate = lambda
    )
    
    ad_raw_p <- ad_test$p.value
    
    IAT_values <- df$IAT
  }
  
  # Return statistics used in downstream datasets and tables
  return(
    list(
      n = n,
      IAT_values = IAT_values,
      lambda = lambda,
      ad_raw_p_value = ad_raw_p
    )
  )
}

# Classifies hits and qualifying contact-related penalties as violent-contact events.
# Fighting-related penalties are excluded, and duplicate violent events at the same game time are removed.

filter_penalties <- function(pbp_master) {
  
  # Penalty types classified as violent contact
  violent_penalty_keys <- c(
    "hooking",
    "tripping",
    "slashing",
    "roughing",
    "roughing-removing-opponents-helmet",
    "holding",
    "holding-the-stick",
    "interference",
    "interference-goalkeeper",
    "cross-checking",
    "boarding",
    "elbowing",
    "high-sticking",
    "charging",
    "kneeing",
    "clipping",
    "checking-from-behind",
    "illegal-check-to-head",
    "spearing",
    "butt-ending",
    "head-butting",
    
    # Contact-related penalty shots
    "ps-hooking-on-breakaway",
    "ps-tripping-on-breakaway",
    "ps-holding-on-breakaway",
    "ps-holding-stick-on-breakaway",
    "ps-slash-on-breakaway",
    
    # Contact-related double minors
    "high-sticking-double-minor",
    "cross-checking-double-minor",
    "spearing-double-minor",
    "butt-ending-double-minor",
    "head-butting-double-minor"
  )
  
  # Fighting-related penalties excluded from violent contact
  fighting_related_keys <- c(
    "fighting",
    "instigator",
    "instigator-face-shield",
    "instigator-misconduct",
    "aggressor"
  )
  
  # Text descriptions used when a standardized penalty key is unavailable
  contact_description_regex <- paste(
    c(
      "\\bhooking\\b",
      "\\btripping\\b",
      "\\bslashing\\b",
      "\\broughing\\b",
      "\\bholding\\b",
      "\\bholding the stick\\b",
      "\\binterference\\b",
      "\\bgoalkeeper interference\\b",
      "\\bcross[- ]checking\\b",
      "\\bboarding\\b",
      "\\belbowing\\b",
      "\\bhigh[- ]sticking\\b",
      "\\bcharging\\b",
      "\\bkneeing\\b",
      "\\bclipping\\b",
      "\\bchecking from behind\\b",
      "\\billegal check to the head\\b",
      "\\billegal check to head\\b",
      "\\bspearing\\b",
      "\\bbutt[- ]ending\\b",
      "\\bhead[- ]butting\\b"
    ),
    collapse = "|"
  )
  
  # Identify any available penalty-description columns
  key_cols <- intersect(
    c(
      "penalty_key",
      "penalty_type",
      "penaltyType",
      "penalty_desc_key",
      "penaltyDescKey",
      "eventTypeDescKey",
      "typeDescKey",
      "secondaryType",
      "secondary_type"
    ),
    names(pbp_master)
  )
  
  out <- pbp_master %>%
    mutate(row_id = row_number())
  
  # Create required columns when absent
  if (!"description" %in% names(out)) {
    out$description <- NA_character_
  }
  
  if (!"penalty_severity" %in% names(out)) {
    out$penalty_severity <- NA_character_
  }
  
  if (!"penalty_minutes" %in% names(out)) {
    out$penalty_minutes <- NA_real_
  }
  
  # Combine available penalty fields into one classification key
  if (length(key_cols) > 0) {
    out <- out %>%
      mutate(across(all_of(key_cols), as.character))
    
    out$penalty_key_raw <- do.call(coalesce, out[key_cols])
  } else {
    out$penalty_key_raw <- NA_character_
  }
  
  out %>%
    mutate(
      # Standardize penalty keys and descriptions
      penalty_key_clean = penalty_key_raw %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "-") %>%
        str_replace_all("^-+|-+$", ""),
      
      description_clean = coalesce(as.character(description), "") %>%
        str_to_lower(),
      
      # Fill missing severity labels using assessed penalty minutes
      penalty_severity = case_when(
        is.na(penalty_severity) & penalty_minutes == 5 ~ "Major",
        is.na(penalty_severity) & penalty_minutes == 4 ~ "Double Minor",
        is.na(penalty_severity) & penalty_minutes == 15 ~ "Match",
        is.na(penalty_severity) & penalty_minutes == 10 ~ "Misconduct",
        is.na(penalty_severity) & penalty_minutes == 0 ~ "Penalty Shot",
        TRUE ~ penalty_severity
      ),
      
      severity_clean = coalesce(as.character(penalty_severity), "") %>%
        str_to_lower() %>%
        str_squish(),
      
      # Identify fighting-related penalties
      is_fighting_related_penalty =
        penalty_key_clean %in% fighting_related_keys |
        str_detect(
          description_clean,
          regex(
            "\\bfight(ing)?\\b|\\binstigator\\b|\\baggressor\\b",
            ignore_case = TRUE
          )
        ),
      
      # Identify contact-related penalties
      description_has_contact = str_detect(
        description_clean,
        regex(contact_description_regex, ignore_case = TRUE)
      ),
      
      is_contact_penalty =
        penalty_key_clean %in% violent_penalty_keys |
        (
          (is.na(penalty_key_clean) | penalty_key_clean == "") &
            description_has_contact
        ),
      
      is_non_fighting_major =
        severity_clean == "major" &
        !is_fighting_related_penalty,
      
      is_match_penalty =
        severity_clean == "match" |
        penalty_key_clean %in% c(
          "match-penalty",
          "match-penatly-10-minutes"
        ),
      
      # Apply the final violent-penalty definition
      is_violent_penalty =
        event_type == "PENALTY" &
        !is_fighting_related_penalty &
        (
          is_contact_penalty |
            is_non_fighting_major |
            is_match_penalty
        ),
      
      penalty_severity = case_when(
        is_violent_penalty ~ "Violent Contact",
        TRUE ~ penalty_severity
      ),
      
      # Combine qualifying penalties and officially recorded hits
      event_type = case_when(
        event_type == "HIT" ~ "VIOLENT_CONTACT",
        event_type == "PENALTY" &
          penalty_severity == "Violent Contact" ~ "VIOLENT_CONTACT",
        TRUE ~ event_type
      )
    ) %>%
    
    # Retain only one violent event per team at a given game second
    group_by(season, game_id, game_date, game_seconds, event_team) %>%
    arrange(
      desc(event_type == "VIOLENT_CONTACT"),
      desc(coalesce(penalty_minutes, 0)),
      row_id,
      .by_group = TRUE
    ) %>%
    mutate(vc_seen = cumsum(event_type == "VIOLENT_CONTACT")) %>%
    filter(!(event_type == "VIOLENT_CONTACT" & vc_seen > 1)) %>%
    ungroup() %>%
    
    # Remove temporary classification fields
    select(
      -row_id,
      -penalty_key_raw,
      -penalty_key_clean,
      -description_clean,
      -severity_clean,
      -is_fighting_related_penalty,
      -description_has_contact,
      -is_contact_penalty,
      -is_non_fighting_major,
      -is_match_penalty,
      -is_violent_penalty,
      -vc_seen
    )
}
# ---- check empty characters ------

check_empty_character <- function(x) {
  is.null(x) || length(x) == 0 || all(x == "")
}

# ----- check numeic ----
check_empty_numeric <- function(x) {
  is.numeric(x) | all(is.na(x))
}

# ----- check tax  -----
check_taxonomy_match <- function(input_values, db_values) {
  # Normalize input value
  input_norm <- stringr::str_to_sentence(input_values)

  # Check exact matches
  matches <- input_norm %in% db_values

  # For non-matches, find closest suggestions
  suggestions <- sapply(which(!matches), function(i) {
    if (is.na(input_values[i])) {
      return(NA)
    }

    # Calculate string distances using Jaro-Winkler distance
    distances <- stringdist::stringdist(
      input_norm[i],
      db_values,
      method = "jw"
    )

    # Get the closest match
    closest_idx <- which.min(distances)
    closest_dist <- distances[closest_idx]

    # Only suggest if reasonably close (distance < 0.3)
    if (closest_dist < 0.3) {
      return(db_values[closest_idx])
    } else {
      return(paste0("No close match found for '", input_values[i], "'"))
    }
  })

  # Create result with suggestions
  result <- list(
    valid = matches,
    suggestions = rep(NA, length(input_values))
  )
  result$suggestions[!matches] <- suggestions

  return(result)
}
check_empty_character <- function(x) {
  is.null(x) || length(x) == 0 || all(x == "")
}

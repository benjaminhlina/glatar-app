# ---- check empty characters ------

check_empty_character <- function(x) {
  is.null(x) || length(x) == 0 || all(x == "")
}


# ----- Credential checker -----
# Works with the same `credentials` data frame shinymanager uses.
check_tab_credentials <- function(user, pass, credentials) {
  row <- credentials[
    tolower(trimws(credentials$user)) == tolower(trimws(user)),
  ]
  if (nrow(row) == 0) {
    return(FALSE)
  }
  # Plain-text comparison — replace with hash check if needed
  isTRUE(row$password[1] == pass)
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

# ---- check sheets ----
check_sheets <- function(file_path, output) {
  # get sheets
  sheets <- readxl::excel_sheets(file_path)

  # check if all sheets are there
  required_sheets <- c("tbl_submission", "tbl_sources", "tbl_samples")
  missing_sheets <- setdiff(required_sheets, sheets)

  if (length(missing_sheets) > 0) {
    output$upload_status <- shiny::renderUI({
      shiny::p(
        paste0(
          "✖ Error: Missing required sheet(s): ",
          paste(missing_sheets, collapse = ", ")
        ),
        style = "color:red; font-weight:600;"
      )
    })

    return()
  }
}

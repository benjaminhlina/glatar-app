# ---- check empty characters ------

#' Check functions
#'
#' These functions check different conditions
#'
#' @param x a `vector
#'
#' @name check_functions
#' @export`

check_empty_character <- function(x) {
  is.null(x) || length(x) == 0 || all(x == "")
}


# ---- check sheets ----
#' @param file_path the file path supplied by reactive handler
#' @param output shiny object to output too
#'
#' @name check_functions
#'
#' @export
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

# ----- Credential checker -----
# Works with the same `credentials` data frame shinymanager uses.

#' @param user a `vector` containing the username
#' @param pass a `vector` containing the password
#' @param credentials a `dataframe` containing username and password
#'
#' @name check_functions
#' @export

check_tab_credentials <- function(user, pass, credentials) {
  cd <- credentials[
    tolower(trimws(credentials$user)) == tolower(trimws(user)),
  ]
  if (nrow(cd) == 0) {
    return(FALSE)
  }
  # Plain-text comparison — replace with hash check if needed
  isTRUE(cd$password[1] == pass)
}


# ----- check tax  ----
#' @param input_values a `vector` of common and species names in incoming
#' excel sheet.
#' @param db_values a `vector` of common and species naems that are
#' in `tbl_taxonomy`
#'
#' @name check_functions
#'
#' @export

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

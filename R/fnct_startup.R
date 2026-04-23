#' Start up GLATAR App
#'
#' This function, which has no arguments, starts the database
#' connection, gets `valid_values`` for validation, gets
#' `nice_names` for properly displaying tables, creates
#' credientials, and and file location for resources.
#'
#' @export
start_up <- function() {
  con <- start_db_con()
  # ---- get _valid_values from db -------
  valid_values <- get_valid_values(con)

  app_version <- "0.1.0"

  # create named vectors
  nice_name_lookup <- stats::setNames(
    naming_conventions$nice_names,
    naming_conventions$raw_names
  )
  # ----- load everything ------
  gtag_path <- system.file("www", package = "glatar")
  if (gtag_path == "") {
    cli::cli_alert_warning(
      "gtag.js not found in inst/www/ - Google Analytics will not load"
    )
  } else {
    cli::cli_alert_success("gtag.js found at: {gtag_path}")
  }
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "glatar")
  )

  credentials <- data.frame(
    user = Sys.getenv("SHINY_USER"),
    password = Sys.getenv("SHINY_PASSWORD"),
    stringsAsFactors = FALSE
  )
  # ---- create ui ----
  cli::cli_alert_info("Starting the App")

  startup <- list(
    con = con,
    valid_values = valid_values,
    app_version = app_version,
    naming_conventions = naming_conventions,
    nice_name_lookup = nice_name_lookup,
    rule_map = rule_map,
    credentials = credentials
  )
  return(startup)
}

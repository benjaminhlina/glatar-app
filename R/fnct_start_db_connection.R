# ----- create connection to database -----

#' Start Database Connection
#'
#' This function uses environment supplied variables, `{DBI}`,
#' and `{RPostgres}` to connect to a PostgreSQL database.
#'
#' @param username The username for user specific connections.
#' Default is `NULL` and will use default user.
#' @param password The passward for user specific connections.
#' Default is `NULL` and will use default user.
#'
#' @return Returns a connection object.
#'
#' @name db_connections
#' @export

start_db_con <- function(username = NULL, password = NULL) {
  cli::cli_alert_info("The SSL mode is {.val {Sys.getenv('POSTGRES_SSLMODE')}}")
  ssl_mode <- Sys.getenv("POSTGRES_SSLMODE", unset = "disable")

  cli::cli_alert_info("The SSL mode is {.val {ssl_mode}}")
  cli::cli_alert("Starting db connection")

  current_user <- if (!is.null(username)) {
    username
  } else {
    Sys.getenv("POSTGRES_USER")
  }
  current_pw <- if (!is.null(password)) {
    password
  } else {
    Sys.getenv("POSTGRES_PASSWORD")
  }

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRES_DB"),
    host = Sys.getenv("POSTGRES_HOST"),
    port = Sys.getenv("POSTGRES_PORT"),
    user = current_user,
    password = current_pw,
    sslmode = ssl_mode,
    gssencmode = "disable"
  )

  cli::cli_alert_success(
    "db successfully connected to {.val {Sys.getenv('POSTGRES_DB')}} on {.val {Sys.getenv('POSTGRES_HOST')}}"
  )
  cli::cli_alert_success(
    "db successfully connected (is_valid: {.val {DBI::dbIsValid(con)}})"
  )
  return(con)
}

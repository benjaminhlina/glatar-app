# ----- create connection to database -----
cli::cli_alert_info("The SSL mode is {.val {Sys.getenv('POSTGRES_SSLMODE')}}")
ssl_mode <- Sys.getenv("POSTGRES_SSLMODE", unset = "disable")

cli::cli_alert_info("The SSL mode is {.val {ssl_mode}}")
cli::cli_alert("Starting db connection")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRES_DB"),
  host = Sys.getenv("POSTGRES_HOST"),
  port = Sys.getenv("POSTGRES_PORT"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  sslmode = ssl_mode
)

cli::cli_alert_success(
  "db successfully connected to {.val {Sys.getenv('POSTGRES_DB')}} on {.val {Sys.getenv('POSTGRES_HOST')}}"
)
cli::cli_alert_success(
  "db successfully connected (is_valid: {.val {DBI::dbIsValid(con)}})"
)

{
  library(DBI)
  library(RPostgres)
  library(RPostgreSQL)
}

# ----- create connection to database -----
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = Sys.getenv("POSTGRES_DB"),
                      host = Sys.getenv("POSTGRES_HOST"),
                      port = Sys.getenv("POSTGRES_PORT"),
                      user = Sys.getenv("POSTGRES_USER"),
                      password = Sys.getenv("POSTGRES_PASSWORD")
                      # sslmode = "require"
                      )

{
  library(DBI)
  library(RPostgres)
  library(RPostgreSQL)
}

# ----- create connection to database -----
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "NAAEDdb",
                      host = "bh-2.cluster-c1mqmq4qms1u.us-east-2.rds.amazonaws.com",
                      port = 5432,
                      user = Sys.getenv("POSTGRES_USER"),
                      password = Sys.getenv("POSTGRES_PASSWORD"),
                      sslmode = "require"
                      )

# ----- startup -----
con <- start_db_con()
valid_values <- get_valid_values(con)

app_version <- "0.1.0"

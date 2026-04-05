# ----- startup -----
# ----- start db conections
con <- start_db_con()
# ---- get _valid_values from db -------
valid_values <- get_valid_values(con)

app_version <- "0.1.0"

# ----- bring in naming convetions ------
naming_conventions <- readr::read_csv(
  here::here(
    "data",
    "app-data",
    "app_naming_conventions.csv"
  ),
) |>
  dplyr::mutate(
    nice_names = dplyr::case_when(
      raw_names %in% "d13c" ~ "\U03B4<sup>13</sup>C",
      raw_names %in% "d15n" ~ "\U03B4<sup>15</sup>N",
      raw_names %in% "d34s" ~ "\U03B4<sup>34</sup>S",
      raw_names %in% "d18o" ~ "\U03B4<sup>18</sup>O",
      raw_names %in% "d2h" ~ "\U03B4<sup>2</sup>H",
      .default = nice_names
    )
  )

# print(naming_conventions, n = 50)

# create named vectors
nice_name_lookup <- stats::setNames(
  naming_conventions$nice_names,
  naming_conventions$raw_names
)

# ----- bring in rule map ----
rule_map <- readr::read_csv(here::here(
  "data",
  "app-data",
  "validation_rule_mapping.csv"
))

credentials <- data.frame(
  user = Sys.getenv("SHINY_USER"),
  password = Sys.getenv("SHINY_PASSWORD"),
  stringsAsFactors = FALSE
)
# ---- create ui ----
cli::cli_alert_info("Starting the App")

{
  library(dplyr)
  library(here)
  library(readr)
}

# ---- create renaming table ----

naming_conventions <- read_csv(here::here(
  "data",
  "app_naming_conventions.csv"
)) |>
  mutate(
    nice_names = case_when(
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
nice_name_lookup <- setNames(
  naming_conventions$nice_names,
  naming_conventions$raw_names
)

# ---- create renaming table ----

get_nice_name_lookup <- get_nice_name_lookup <- function(con) { 
  tbl(con, "tbl_naming_conventions") |>
  collect() |> 
  mutate(
    nice_names = case_when(
      raw_names %in% "d13c" ~ "\U03B4<sup>13</sup>C",
      raw_names %in% "d15n" ~ "\U03B4<sup>15</sup>N",
      raw_names %in% "d34s" ~ "\U03B4<sup>34</sup>S",
      .default = nice_names
    )
  )

# print(naming_conventions, n = 50)

# create named vectors
 setNames(naming_conventions$nice_names,
                             naming_conventions$raw_names)
}

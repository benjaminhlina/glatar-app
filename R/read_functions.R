# ----- read col_types------
read_col_types <- function(file_path, tbl_name, skip, n_max) {
  col_count <- readxl::read_excel(
    file_path,
    sheet = tbl_name,
    skip = skip,
    n_max = n_max,
    guess_max = 100000
  ) |>
    ncol()

  # ---- then dynamically create col_types
  col_types <- c(
    rep("guess", 3),
    "date",
    rep("guess", col_count - 4)
  )
  return(col_types)
}

# ----- read xl file ------

read_xl <- function(
  file_path,
  tbl_name,
  con = NULL,
  col_types = NULL,
  skip,
  rename = TRUE,
  rename_twice = NULL
) {
  df <- tryCatch(
    {
      readxl::read_excel(
        file_path,
        sheet = tbl_name,
        col_types = col_types,
        skip = skip,
        guess_max = 100000
      ) |>
        janitor::clean_names() |>
        (\(x) {
          if (!rename) {
            return(x)
          }
          x <- rename_to_db_col(x, con, tbl_name)
          if (!is.null(rename_twice)) {
            x <- rename_to_db_col(x, con, rename_twice)
          }
          x
        })()
    },
    error = function(e) {
      msg <- conditionMessage(e)

      if (grepl("col_types", msg, fixed = TRUE)) {
        cli::cli_abort(
          c(
            "Could not read {.val {tbl_name}}: sheet has more columns than expected.",
            "i" = "Check that the template has not been modified (extra columns may have been added).",
            "x" = "{msg}"
          ),
          call = NULL
        )
      }

      cli::cli_abort(
        c(
          "Could not read {.val {tbl_name}}.",
          "x" = "{msg}"
        ),
        call = NULL
      )
    }
  )

  return(df)
}

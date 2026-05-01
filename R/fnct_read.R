# ----- read col_types------
#' Read functions
#'
#' Read functions allow for `.xlsx` files to be read into the the app
#' to be able to then manipulate and eventually validate and import into
#' the database.
#'
#' @param file_path the path to the file provided by UI upload observer.
#' @param tbl_name the table name to be imported
#' @param skip the number of rows to skip given the header is not the first row.
#' @param n_max n_max is number of the maximum number of data rows to read.
#'
#' @details `read_col_types()`` reads in a file and determiens the number of
#' columns and the column types
#' @return  `read_col_types()` returns a vector that has each column type
#' for a given excel file.
#'
#' @name read_functions
#' @export
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
#'
#' @param file_path the path to the file provided by UI upload observer.
#' @param tbl_name the table name to be imported
#' @param skip the number of rows to skip given the header is not the first row.
#' @param col_types a vector contianing the column types supplied by `read_col_types()`.
#' @param con a `DBI` conection to, in this case PostgreSQL database.
#' @param rename a logical that strips excesive information and renames column names
#' to snake case and follows the database shcema. Defaults to `TRUE`.
#' @param rename_twice a logical that strips excesive information and renames column names
#' to snake case and follows the database shcema. Defaults to  `NULL``.
#'
#'
#' @details`read_xl()`` reads in a file and cleans up the columns names before any other
#' processes occure such as validation and spliting to the database.
#' @return  `read_xl()`returns a `data.frame` with the a given database table.
#'
#' @name read_functions
#' @export

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

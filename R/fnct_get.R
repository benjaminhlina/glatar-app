# ----- get_column_amp ------

#' Get functions
#'
#' These functions are very important as they get infomration
#' from a PostgreSQL database, whther that is
#' schemas, raw values or other information about the database.
#' Each function creates a SQL string that is excuted on a
#' given table in the database.
#'
#' @param con a valid `DBI` connection to a PostgreSQL database.
#'
#' @details
#' `get_column_map()` gets the data dictonary from the database.
#'
#' @name get_functions
#' @export

get_column_map <- function(con) {
  df <- dplyr::tbl(con, "tbl_data_dictionary")
  return(df)
}

# ----- get_data -----

#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param debug_sql a logical value that will provide SQL for
#' debugging. Defualt is `FALSE`.
#'
#' @details
#' `get_data()` retrives base template for all sample data in the
#' the database. It gets `tbl_samples`, `tbl_loc`, and `tbl_length`
#' and joins them to make a base for all other opperations.
#'
#' @name get_functions
#' @export
get_data <- function(con, debug_sql = FALSE) {
  shiny::req(con)

  # Always start from samples
  # --grab location
  tbl_loc <- dplyr::tbl(con, "tbl_location")
  # ---- grab sampels
  df <- dplyr::tbl(con, "tbl_samples")

  # grab_+length
  tbl_length <- dplyr::tbl(con, "tbl_length")

  df <- df |>
    dplyr::left_join(
      tbl_loc,
      by = "sample_id"
    ) |>
    dplyr::left_join(
      tbl_length,
      by = "sample_id"
    )
  if (debug_sql == TRUE) {
    cli::cli_alert_info(dbplyr::sql_render(df))
  }
  return(df)
}


# ----- get data types -----
#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param df a `tbl_lazy`.
#' @param data_tables  a vector produced by `data_tables()`.
#' @param flag_cols a vector that has replaced `tbl_` to `has_`
#' if table is present in `data_tables()`.
#' @param var a variable of interest.
#'
#' @details
#' `get_data_tables()` this retrieves and identifes which data belongs to each sample
#' id. For example, id `1` has calorimetry and proximent composition data. This will
#' start the base dataframe to gather this information.
#' @name get_functions
#' @export

get_data_tables <- function(con, df, data_tables, flag_cols, var) {
  for (i in seq_along(data_tables)) {
    df <- df |>
      dplyr::left_join(
        dplyr::tbl(con, names(data_tables)[i]) |>
          dplyr::distinct(.data[[var]]) |>
          dplyr::mutate(!!flag_cols[i] := 1L),
        by = var
      )
  }
  return(df)
}

# ----- get dropdown hoices -----

#' @param df a `tbl_lazy`.
#' @param type variable of interest for a given dropdown.
#' @name get_functions
#' @export
get_dropdown_choices <- function(df, type) {
  df <- df |>
    dplyr::distinct(.data[[type]]) |>
    dplyr::arrange(.data[[type]]) |>
    dplyr::pull(.data[[type]])
  return(df)
}

# ---- get good groups -----
#' @param df a `tbl_lazy`.
#' @name get_functions
#' @export
get_good_groups <- function(df) {
  shiny::req(df)
  good_groups <- good_groups()

  # get column names
  cols <- dplyr::tbl_vars(df) |>
    as.character()

  # # Return only those that are in good_groups
  groups <- sort(intersect(cols, good_groups))

  cli::cli_alert_info("Converted names: {.val {cols}}")
  bullet <- cli::symbol$bullet
  cli::cli_inform(c(
    "v" = "Selecting groups.",
    bullet = "Groups selected: {.val {groups}}"
  ))

  return(groups)
}

# ---- get table ids -----

#' @param con a valid `DBI` connection to a PostgreSQL database.
#'
#' @details
#' `get_id_col()`use a SQL query where we select all of the ID columns
#' throughout the entire database and then select only the columns that
#' we want to add new ID values to. This information is then fed to
#' `get_id_max()`.
#'
#' @name get_functions
#' @seealso [get_id_max()]
#' @export
get_id_col <- function(con) {
  tables_ids <- DBI::dbGetQuery(
    con,
    "
        SELECT table_name, column_name
        FROM information_schema.columns
        WHERE table_schema = 'public'
        AND column_name LIKE '%_id'
        AND table_name <> 'tbl_submission'"
  ) |>
    dplyr::filter(
      !column_name %in%
        c("submission_id", "percent_lipid", "user_sample_id") &
        !(column_name %in% "sample_id" & table_name != "tbl_samples"),
      !(column_name %in% "source_id" & table_name != "tbl_sources")
    )

  return(tables_ids)
}

# ----- get max id -----

#' @param table_name the name of the database table to get the maxiumum
#' id column value.
#' @param id_col name of the corresponding id column in a given `table_name`
#' that we want the maximum id value.
#'
#' @name get_functions
#' @seealso [get_id_col()]
#' @export

get_id_max <- function(table_name, id_col) {
  result <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT COALESCE(MAX({id_col}), 0) AS max_id FROM {table_name}")
  )
  selected_id_max <- result$max_id

  return(selected_id_max)
}


# ----- simple function to get a tb use dbplyr -----
#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param df a `tbl_lazy`.
#' @param table table name to get from the database.
#'
#' @name get_functions
#' @export

get_join_table <- function(df, table, con) {
  jt <- df |>
    dplyr::left_join(dplyr::tbl(con, table))

  return(jt)
}


# ---- get numeric vars -----
#' @param con a valid `DBI` connection to a PostgreSQL database.
#'
#' @name get_functions
#' @export

get_numeric_vars <- function(con) {
  df <- get_column_map(con) |>
    dplyr::filter(
      field_class %in% c("integer", "numeric", "double")
    ) |>
    dplyr::distinct(field_name) |>
    dplyr::arrange(field_name) |>
    dplyr::pull(field_name)
  return(df)
}

# ----- get ranges -----

#' @param df a `tbl_lazy`.
#' @param x_var the `x` variable of interest.
#' @param y_var the `y` variable of interest.
#'
#' @name get_functions
#' @export

get_ranges <- function(df, x_var, y_var) {
  ranges <- df |>
    dplyr::summarise(
      x_min = min(.data[[x_var]], na.rm = TRUE),
      x_max = max(.data[[x_var]], na.rm = TRUE),
      y_min = min(.data[[y_var]], na.rm = TRUE),
      y_max = max(.data[[y_var]], na.rm = TRUE)
    ) |>
    dplyr::collect()

  if (
    is.na(ranges$y_min) ||
      is.na(ranges$y_max) ||
      is.na(ranges$x_min) ||
      is.na(ranges$x_max)
  ) {
    return(NULL)
  } else {
    range_tot <- list(
      x_range_vec = c(ranges$x_min, ranges$x_max),
      y_range_vec = c(ranges$y_min, ranges$y_max)
    )
    return(range_tot)
  }
}

# ------ gret raw data ------

#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param selected_vars a `reactive` object that is the column name of interest
#' usually generated from sidebar.
#' @param grouping_vars a `reactive` object that is the column name of grouping
#' variables usually generated from sidebar.
#' @param debug_sql a logical value that will provide SQL for
#' debugging. Defualt is `FALSE`.
#'
#' @details
#' `get_raw_data()` retrives base template for all sample data in the
#' the database and joins variables and grouping variables of interest.
#'
#' @name get_functions
#' @export
#'
get_raw_data <- function(
  con,
  selected_vars = NULL,
  grouping_vars = NULL,
  debug_sql = FALSE
) {
  shiny::req(con)

  if (is.null(selected_vars)) {
    selected_vars <- NULL
  }

  cli::cli_inform(c(
    "v" = "Starting summary data query.",
    bullet = "Variables selected: {.val {selected_vars}}"
  ))

  # Always start from samples
  # --grab location
  df <- get_data(con)
  # |>
  #   left_join(
  #     tbl(con, "tbl_calorimetry")
  #   )

  base_col <- c(
    "submission_id",
    "sample_id",
    "user_sample_id",
    "organism_type",
    "common_name",
    "scientific_name",
    "data_type",
    "waterbody"
  )

  # base_col <- get_data(con) |>
  #   colnames() |>
  #   sort()

  # ----- grab seelected vars ----

  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    # --- get selected vars -----
    vars_for_select <- as.character(selected_vars)

    vars_for_select <- dplyr::case_when(
      grepl(
        "^length_mm__(fork|total|standard)$",
        vars_for_select
      ) ~ "length_mm",
      grepl("^energy_units__", vars_for_select) ~ "energy_measurement",
      # grepl("amino_acid_type__", vars_for_select) ~ "amino_acid_measurement",
      .default = vars_for_select
    ) |>
      unique(vars_for_select)

    cli::cli_inform("vars_for_select after remap: {.val {vars_for_select}}")
    needed_tables <- setdiff(
      get_tables_needed(con = con, var = vars_for_select),
      "tbl_samples"
    )
    cli::cli_inform("needed_tables: {.val {needed_tables}}")

    if (!is.null(needed_tables)) {
      df <- needed_tables |>
        purrr::reduce(.init = df, ~ get_join_table(.x, .y, con))
    }

    # check what columns are in the the table after joining
    cli::cli_inform("cols in df: {.val {colnames(df)}}")
  } else {
    vars_for_select <- NULL # no extra cols
  }

  df <- df |>
    dplyr::select(
      dplyr::all_of(base_col),
      dplyr::any_of(c(grouping_vars, vars_for_select))
    )

  if (debug_sql) {
    cli::cli_alert_info(dbplyr::sql_render(df))
  }
  cli::cli_alert_success("selected qery completed: df is {.val {class(df)}}")
  return(df)
}
# ---- Helper: determine which tab is selected ----
#' @param input input from UI
#'
#' @details
#' `get_selected_tabs()` is an exception within `get_*` functions as
#' it doesn't interact with the database and instead interacts with
#' the `shiny` UI.
#'
#' @name get_functions
#' @export
#'

get_selected_tab <- function(input) {
  current_tab <- input$tabs
  if (is.null(current_tab) || length(current_tab) != 1) {
    cli::cli_alert_info("current_tab is NULL or invalid")
    return(NULL)
  }
  out <- switch(
    current_tab,
    "summary_info" = input[["summary_sidebar-summary_table"]],
    "scatter_plot" = input[["scatter_sidebar-scatter_plot"]],
    NULL
  )
  cli::cli_alert_info("Active tab: {current_tab}")
  cli::cli_alert_info("Selected table from sidebar: {out}")
  if (is.null(out) || is.na(out) || out == "") {
    return(NULL)
  }
  return(out)
}

# ---- get summary data frame -----
#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param selected_vars a `reactive` object that is the column name of interest
#' usually generated from sidebar.
#' @param grouping_vars a `reactive` object that is the column name of grouping
#' variables usually generated from sidebar.
#' @param debug_sql a logical value that will provide SQL for
#' debugging. Defualt is `FALSE`.
#'
#' @details
#' `get_summary_data()` retrives base template for all sample data in the
#' the database and joins variables and grouping variables of interest for
#' summarizing.
#'
#' @name get_functions
#' @export
#'
get_summary_data <- function(
  con,
  selected_vars = NULL,
  grouping_vars = NULL,
  debug_sql = FALSE
) {
  shiny::req(con)

  if (is.null(selected_vars)) {
    selected_vars <- NULL
  }

  cli::cli_inform(c(
    "v" = "Starting summary data query.",
    bullet = "Variables selected: {.val {selected_vars}}"
  ))

  # Always start from samples
  # --grab location
  df <- get_data(con) |>
    dplyr::left_join(
      dplyr::tbl(con, "tbl_calorimetry")
    )

  # ----- grab seelected vars ----

  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    needed_tables <- setdiff(
      get_tables_needed(con = con, var = selected_vars),
      "tbl_samples"
    )

    if (!is.null(needed_tables)) {
      df <- needed_tables |>
        purrr::reduce(.init = df, ~ get_join_table(.x, .y, con))
    }

    # --- get selected vars -----
    vars_for_select <- as.character(selected_vars)

    vars_for_select <- dplyr::case_when(
      grepl(
        "^length_mm__(fork|total|standard)$",
        vars_for_select
      ) ~ "length_mm",
      grepl("^energy_units__", vars_for_select) ~ "energy_measurement",
      # grepl("amino_acid_type__", vars_for_select) ~ "amino_acid_measurement",
      .default = vars_for_select
    )

    vars_for_select <- unique(vars_for_select)

    if (is.null(grouping_vars)) {
      # Select only requested columns (plus keys if needed)
      df <- df |>
        dplyr::select(
          organism_type,
          data_type,
          waterbody,
          scientific_name,
          length_type,
          energy_units,
          dplyr::any_of(vars_for_select)
        )
      # }
    } else {
      df <- df |>
        dplyr::select(
          organism_type,
          data_type,
          waterbody,
          scientific_name,
          length_type,
          energy_units,
          dplyr::any_of(grouping_vars),
          dplyr::any_of(vars_for_select)
        )
    }
  } else {
    df
  }

  if (debug_sql) {
    cli::cli_alert_info(dbplyr::sql_render(df))
  }

  cli::cli_alert_success("selected qery completed: df is {.val {class(df)}}")
  return(df)
}
# ----- get submision id ------
#' @param con a valid `DBI` connection to a PostgreSQL database.
#'
#' @name get_functions
#' @export
get_submission_id <- function(con) {
  sub_id <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT gen_random_uuid() AS next_id")
  )
  return(sub_id)
}


# ---- get teh tables we need to filter by based on what the user selects -----

#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param var a variable of interest.
#'
#' @details
#' `get_tables_needed()` this gets the tables of interest
#'
#' @seealso [get_column_map()]
#' @name get_functions
#' @export

get_tables_needed <- function(con, var) {
  shiny::req(con)

  if (is.null(var) || length(var) == 0) {
    return(character(0))
  }

  get_column_map(con) |>
    dplyr::filter(field_name %in% var) |>
    dplyr::distinct(table_name) |>
    dplyr::pull(table_name)
}

# ----- get taxa columns ------
#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @name get_functions
#' @export
get_taxa_col <- function(con) {
  taxa_col <- tbl(con, "tbl_taxonomy") |>
    colnames()

  return(taxa_col)
}


# ---- get theme selection -----

#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param theme a `vector` containing different data themes
#' @param numeric_choices a `reactive` value that has numerical choices.
#' @param numeric_names a `reactive` value that has selected numerical column names.
#' @param length_vars a `reactive` value that has length variables.
#' @param energy_vars a `reactive` value that has energy variables.
#' @name get_functions
#' @export

get_theme_choices <- function(
  con,
  theme,
  numeric_choices,
  numeric_names,
  length_vars,
  energy_vars
) {
  cli::cli_alert_info("Available numeric_choices: {.val {numeric_choices}}")
  # Always included regardless of theme
  shared_vars <- c(
    "sample_weight",
    "weight_g"
  )

  theme_vars <- switch(
    theme,
    "Energy Density" = c(
      energy_vars,
      "percent_water",
      "percent_ash"
    ),
    "Body Composition" = dplyr::tbl(con, "tbl_proxcomp") |>
      colnames(),
    "Stable Isotopes" = dplyr::tbl(con, "tbl_isotope") |>
      colnames(),
    "Amino Acids" = dplyr::tbl(con, "tbl_amino_acid") |>
      colnames(),
    "Fatty Acids" = dplyr::tbl(con, "tbl_fatty_acid") |>
      colnames(),
    "Contaminates" = dplyr::tbl(con, "tbl_contaminants") |>
      colnames(),
    "Thiamine" = dplyr::tbl(con, "tbl_thiamine") |>
      colnames(),
    character(0)
  )
  cli::cli_alert_info("Theme vars resolved to: {.val {theme_vars}}") # add after switch()

  # Combine shared + theme-specific, filter to only valid choices
  relevant_vars <- c(shared_vars, theme_vars)

  valid_numeric <- numeric_choices[numeric_choices %in% relevant_vars]
  valid_names <- numeric_names[numeric_choices %in% relevant_vars]
  cli::cli_alert_info("Relevant vars matched: {.val {relevant_vars}}")

  choices <- sort(c(
    stats::setNames(valid_numeric, valid_names),
    length_vars,
    if (theme %in% c("Energy Density")) energy_vars else NULL
  ))

  return(choices)
}


# ---- get valid values
#' @param con a valid `DBI` connection to a PostgreSQL database.
#'
#' @details
#' `get_valid_values()` gets all the schemas in the database and
#' grabs all the constraints and cleans them up so we can use the
#' constraints to validate data.
#'
#' @name get_functions
#' @export
get_valid_values <- function(con) {
  raw_constraints <- DBI::dbGetQuery(
    con,
    "
    SELECT cc.table_name, cc.column_name, chk.check_clause
    FROM information_schema.table_constraints tc
    JOIN information_schema.check_constraints chk
      ON tc.constraint_name = chk.constraint_name
     AND tc.constraint_schema = chk.constraint_schema
    JOIN information_schema.constraint_column_usage cc
      ON tc.constraint_name = cc.constraint_name
     AND tc.table_schema = cc.table_schema
    WHERE tc.constraint_type = 'CHECK'
      AND tc.table_schema = 'public'
      AND cc.table_name LIKE 'tbl_%'
    ORDER BY cc.table_name, cc.column_name
  "
  )

  cleaned_constrants <- raw_constraints |>
    dplyr::rowwise() |>
    dplyr::mutate(values = list(clean_db_constraints(check_clause))) |>
    dplyr::filter(!is.null(values)) |>
    dplyr::ungroup() |>
    dplyr::select(column_name, values) |>
    tibble::deframe()

  return(cleaned_constrants)
}

# ---- get vart types ----
#' @param con a valid `DBI` connection to a PostgreSQL database.
#' @param df a `tbl_lazy`.
#' @param var a variable of interst to get unique values.
#'
#' @name get_functions
#' @export
get_var_types <- function(df, var) {
  var_types <- get_dropdown_choices(df = df, type = var)
  # df |>
  # dplyr::distinct(.data[[var]]) |>
  # dplyr::arrange(.data[[var]]) |>
  # dplyr::pull(.data[[var]])
  # Only keep non-NA length types
  var_types <- var_types[!is.na(var_types)]

  # Create synthetic variable names and labels
  if (any(var_types %in% c("fork", "standard", "total", "carapace"))) {
    vars <- paste0("length_mm__", var_types)
    labels <- paste0(stringr::str_to_title(var_types), " Length (mm)")
  }
  if (
    any(
      var_types %in%
        c(
          "Joules/g dry weight",
          "Joules/g wet weight"
        )
    )
  ) {
    # cleaned_var_types <- gsub("/", " ", var_types)
    # cleaned_var_types <- gsub("\\s+", "_", cleaned_var_types)

    vars <- paste0("energy_units__", var_types)
    labels <- paste0("Energy Density (", var_types, ")")
  }
  # if (
  #   any(
  #     var_types %in%
  #       c(
  #         "Alanine",
  #         "Arginine",
  #         "Aspartic acid",
  #         "Cysteine",
  #         "Cystine",
  #         "Glutamic acid",
  #         "Glycine",
  #         "Histidine",
  #         "Isoleucine",
  #         "Leucine",
  #         "Lysine",
  #         "Methionine",
  #         "Phenylalanine",
  #         "Proline",
  #         "Serine",
  #         "Threonine",
  #         "Tyrosine",
  #         "Valine"
  #       )
  #   )
  # ) {
  #   vars <- paste0("amino_acid_type__", var_types)
  #   labels <- paste0(
  #     stringr::str_to_sentence(var_types)
  #     # " (",
  #     # amino_acid_unit,
  #     # ")"
  #   )
  # }
  # c("ug/mg sample weigh"t, "% total protein")

  stats::setNames(vars, labels) # names = labels, values = synthetic variable codes
}

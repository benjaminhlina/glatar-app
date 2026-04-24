# ------- add_valid_cols -----

#' Add functions
#'
#' These functions add new columns to the ingested
#' data prior to being validated or imported
#' into the database
#'
#' @param df a `data.frame`
#' @param id_name the id column name to add new id to in the database
#' @param max_ids the max id value
#'
#' @name add_functions
#' @export
add_new_id <- function(df, id_name, max_ids) {
  col_name <- paste0(".", id_name)

  df <- df |>
    dplyr::mutate(
      !!col_name := seq(
        from = max_ids[[id_name]] + 1,
        length.out = dplyr::n()
      )
    )
}


# ------ add_source_id -----

#' @param tbl_samples `data.frame` that contains sample data
#' @param tbl_sources `data.frame` that contains source data
#' @name add_functions
#' @export
add_source_id <- function(tbl_samples, tbl_sources) {
  source_select <- tbl_sources |>
    dplyr::select(source_id, .source_id)

  tbl_samples <- tbl_samples |>
    dplyr::left_join(
      source_select
    ) |>
    dplyr::select(-source_id) |>
    dplyr::rename(source_id = .source_id)

  return(tbl_samples)
}

# ------ add_sub_sor_tbl -------

#' @param split_tables a `list` cotaining `tbl_samples` split into
#' database tables prior to submission.
#' @param sub_tbl the `data.frame` that will be submitted to `tbl_submission`
#' in the database.
#' @param sor_tbl the `data.frame` that will be submitted to `tbl_sources`
#' in the database.
#'
#' @name add_functions
#' @export

# ---- source and submission tables once split samples -----
add_sub_sor_tbl <- function(split_tables, sub_tbl, sor_tbl) {
  # addd --- source and submsision
  split_tables$tbl_submission <- sub_tbl
  split_tables$tbl_sources <- sor_tbl
  return(split_tables)
}
# ------ add_table_ids -----
#' @param tables_split a `list` cotaining `tbl_samples` split into
#' database tables prior to submission.
#' @param tables_ids column name that ids are assigned
#' @param max_ids the max id value
#'
#' @name add_functions
#' @export

add_table_ids <- function(tables_split, tables_ids, max_ids) {
  purrr::imap(tables_split, function(df, tbl_name) {
    # --- get candidate id columns for this table
    id_col <- tables_ids |>
      dplyr::filter(table_name == tbl_name) |>
      dplyr::pull(column_name)

    # --- drop foreign keys you never want to generate
    id_col <- setdiff(
      id_col,
      c("sample_id", "source_id", "submission_id")
    )

    if (length(id_col) == 0) {
      cli::cli_alert_info("No primary ID to assign for {tbl_name}")
      return(df)
    }

    # --- use first remaining (should be only one)
    id_col <- id_col[1]

    # --- current max from DB (0 if none)
    start_id <- max_ids[[id_col]]
    if (is.null(start_id) || is.na(start_id)) {
      start_id <- 0
    }

    # --- do we need to create IDs?
    needs_id <- !id_col %in% names(df) || all(is.na(df[[id_col]]))

    if (needs_id) {
      n <- nrow(df)

      df <- df |>
        dplyr::mutate(
          !!rlang::sym(id_col) := seq.int(start_id + 1, length.out = n)
        )

      cli::cli_alert_success(
        "Assigned {id_col} for {tbl_name} starting at {start_id + 1}"
      )
    } else {
      cli::cli_alert_info("{tbl_name} already has {id_col}")
    }

    df
  })
}

# ------- add_taxonomic_groups -----

#' @param df a `data.frame`
#' @param species_list a `tbl_lazy` object of the `tbl_taxonomy` from the
#' database.
#'
#' @name add_functions
#' @export

add_taxonomic_groups <- function(df, species_list) {
  species_list <- species_list |>
    dplyr::collect()

  df <- df |>
    dplyr::rename(
      user_genus = genus,
      user_family = family
    )
  has_sci <- !is.na(df$scientific_name)
  # df_joined <- df |>
  #   dplyr::left_join(species_list, by = c("common_name", "scientific_name"))

  species_by_sci <- species_list |>
    dplyr::select(-common_name) |>
    dplyr::distinct(scientific_name, .keep_all = TRUE)

  species_by_com <- species_list |>
    dplyr::select(-scientific_name) |>
    dplyr::distinct(common_name, .keep_all = TRUE)

  df_sci <- df[has_sci, ] |>
    dplyr::left_join(
      species_by_sci,
      by = c("scientific_name")
    )

  df_com <- df[!has_sci, ] |>
    dplyr::left_join(
      species_by_com,
      by = "common_name"
    )

  df_joined <- dplyr::bind_rows(df_sci, df_com)
  # return  species list back to lazy -----

  # ----- cli out put ------
  tax_cols <- setdiff(names(species_list), c("common_name", "scientific_name"))
  # CLI report
  matched <- df_joined |>
    dplyr::filter(!is.na(.data[[tax_cols[1]]]))

  unmatched <- df_joined |>
    dplyr::filter(is.na(.data[[tax_cols[1]]]))

  pct <- round(nrow(matched) / nrow(df) * 100, 1)

  cli::cli_h1("Taxonomic Join Report")
  cli::cli_inform(c(
    "i" = "{nrow(df)} row{?s} processed ({pct}% match rate)",
    "v" = "{nrow(matched)} matched",
    if (nrow(unmatched) > 0) c("!" = "{nrow(unmatched)} unmatched")
  ))
  if (nrow(matched) > 0) {
    matched_pairs <- matched |>
      dplyr::distinct(common_name, scientific_name) |>
      dplyr::arrange(common_name)

    cli::cli_ul(glue::glue(
      "{matched_pairs$common_name} ({matched_pairs$scientific_name})"
    ))
  }
  if (nrow(unmatched) > 0) {
    unmatched_pairs <- unmatched |>
      dplyr::distinct(common_name, scientific_name) |>
      dplyr::arrange(common_name)

    cli::cli_h2("Unmatched")
    cli::cli_ul(glue::glue(
      "{unmatched_pairs$common_name} ({unmatched_pairs$scientific_name})"
    ))
    cli::cli_rule()
  }
  return(df_joined)
}


# ----- add_taxa_ui -----
#' @param con a `DBI` conection to, in this case PostgreSQL database.
#' @param ns shiny namespcae object.
#' @name add_functions
#' @export
add_taxa_ui <- function(con, ns) {
  ns_names <- tbl(con, "tbl_taxonomy") |>
    colnames()

  nice_names <- convert_nice_name(ns_names)

  # Generate every input from the two vectors
  inputs <- purrr::map2(ns_names, nice_names, function(ns_name, nice_name) {
    if (ns_name == "tsn") {
      shiny::column(3, shiny::numericInput(ns(ns_name), nice_name, value = NA))
    } else {
      shiny::column(3, shiny::textInput(ns(ns_name), nice_name))
    }
  })

  # Chunk into groups of 4 → one fluidRow each
  input_rows <- inputs |>
    split(ceiling(seq_along(inputs) / 4)) |>
    purrr::map(~ shiny::fluidRow(!!!.x))

  shiny::wellPanel(
    input_rows,
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(
          ns("add_row"),
          "Add Row",
          icon = shiny::icon("plus"),
          class = "btn-primary"
        ),
        shiny::actionButton(
          ns("clear_row"),
          "Clear Fields",
          icon = shiny::icon("eraser"),
          class = "btn-warning ms-2"
        ),
        shiny::actionButton(
          ns("clear_all"),
          "Clear All Rows",
          icon = shiny::icon("trash"),
          class = "btn-danger ms-2"
        ),
        shiny::downloadButton(
          ns("submit"),
          "Submit to Manager",
          icon = shiny::icon("file-excel"),
          class = "btn-success ms-2"
        )
      )
    )
  )
}

# ------ add_valid_cols ------

#' @param df a `data.frame`
#' @param valid_values a `data.frame` containing valid values from database
#' schema derived from `get_valid_values()`.
#'
#' @name add_functions
#' @seealso [get_valid_values()]
#' @export
add_valid_cols <- function(df, valid_values) {
  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character) &
        !dplyr::any_of(c(
          "energy_units",
          "pi_name",
          "amino_acid_type",
          "calorimetry_method",
          "fatty_acid_type",
          "thiamine_type",
          "energy_measurment_units"
        )),
      tolower
    ))

  constraint_exprs <- purrr::imap(valid_values, function(vals, col) {
    if (!col %in% names(df)) {
      return(NULL)
    }
    sym <- rlang::sym(col)
    rlang::expr(is.na(!!sym) | !!sym %in% !!vals)
  }) |>
    purrr::compact()

  names(constraint_exprs) <- paste0(".", names(constraint_exprs))

  df <- df |>
    dplyr::mutate(!!!constraint_exprs)

  # 3. Bespoke checks that can't be expressed as simple %in% -------------
  df <- df |>
    dplyr::mutate(
      .date = is.na(date) | grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
      .month = is.na(month) | (month >= 1 & month <= 12),
      .energy_measurement = dplyr::case_when(
        is.na(sample_weight_type) ~ NA,
        sample_weight_type == "wet" ~ energy_measurement >= 0 &
          energy_measurement <= 13000,
        sample_weight_type == "dry" ~ energy_measurement >= 1000 &
          energy_measurement <= 43000,
        .default = NA
      )
    )

  return(df)
}


# ------ add_valid_taxonomy ------

#' @param df a `data.frame`
#' @param species_list a `tbl_lazy` object of the `tbl_taxonomy` from the
#' database
#'
#' @details `add_valid_taxonomy()` adds columns whether or not `valid_taxonomy()`
#' returns species that are present in `tbl_taxononomy`'s `common_name` and
#' `scientific_name`
#' @name add_functions
#' @export

add_valid_taxonomy <- function(df, species_list) {
  # ---- get tax from db ----
  valid_taxonomy <- valid_taxonomy(species_list)

  valid_common_sentence <- stringr::str_to_sentence(valid_taxonomy$valid_common)
  valid_sci_sentence <- stringr::str_to_sentence(
    valid_taxonomy$valid_scientific
  )

  df <- df |>
    dplyr::mutate(
      common_name = stringr::str_to_sentence(common_name),
      scientific_name = stringr::str_to_sentence(scientific_name)
    )

  # Store validation results in df attributes for later use
  common_check <- check_taxonomy_match(df$common_name, valid_common_sentence)
  sci_check <- check_taxonomy_match(df$scientific_name, valid_sci_sentence)

  print_common <- data.frame(
    valid = common_check$valid,
    suggestions = common_check$suggestions
  ) |>
    dplyr::distinct()

  print_sci <- data.frame(
    valid = sci_check$valid,
    suggestions = sci_check$suggestions
  ) |>
    dplyr::distinct()

  cli::cli_alert_info("comon_check {.val {print_common}}")
  cli::cli_alert_info("sci_check {.val {print_sci}}")

  attr(df, "common_name_suggestions") <- common_check$suggestions
  attr(df, "scientific_name_suggestions") <- sci_check$suggestions

  df <- df |>
    dplyr::mutate(
      .valid_common_name = common_check$valid,
      .valid_scientific_name = sci_check$valid
    )

  return(df)
}

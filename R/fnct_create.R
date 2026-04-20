# ---- create fileted summary ----

#' Create functions
#'
#' These functions are all use other functions to create
#' `shiny::reactive()` objects usually used in modules.
#' Any function that uses a `reactive()` call is considered
#' to be creating an object that is then displayed in the UI.
#'
#'
#' @param data a `reactive()` data frame like object usually
#' created from another `create_function`. Considering
#' raw data is gathered through a PostgresSQL connection
#' these reactive objects tend to be `tbl_lazy`.
#' @param input_source usually an object created by a sidebar
#' function. These objects tend to be `reactive()` outcomes from
#' `observe()` or `observeEvent()` calls within a module.
#' @param pane the sidepane that is bein selected e.g.,
#' `"scatter_plot". `
#'
#' @name create_functions
#' @export

create_filtered_data <- function(data, input_source, pane) {
  shiny::reactive({
    df <- data()

    # add pane to have this switch from req to null depending on pane
    if (pane == "scatter_plot") {
      if (is.null(df)) {
        return(NULL)
      }
    }

    if (pane == "summary_info") {
      shiny::req(df)
    }
    if (pane == "view_data") {
      req(df)
    }

    # ----- create filters -----
    data_type_f <- input_source$data_types()
    waterbody_f <- input_source$waterbody_filter()
    species_f <- input_source$species_filter()
    organism_type_f <- input_source$organism_type()

    cli::cli_inform(c(
      "data_type_f: {paste(data_type_f, collapse = ', ')}",
      "waterbody_f: {paste(waterbody_f, collapse = ', ')}",
      "species_f: {paste(species_f, collapse = ', ')}",
      "organism_type_f: {paste(organism_type_f, collapse = ', ')}"
    ))
    # ----- filters -----
    if (!is.null(data_type_f) && !"All" %in% data_type_f) {
      df <- df |>
        dplyr::filter(data_type %in% data_type_f)
    }
    if (!is.null(waterbody_f) && !"All" %in% waterbody_f) {
      df <- df |>
        dplyr::filter(waterbody %in% waterbody_f)
    }

    if (!is.null(organism_type_f) && !"All" %in% organism_type_f) {
      df <- df |>
        dplyr::filter(organism_type %in% organism_type_f)
    }
    if (!is.null(species_f) && !"All" %in% species_f) {
      df <- df |>
        dplyr::filter(scientific_name %in% species_f)
    }

    if ((pane == "view_data")) {
      df <- df |>
        dplyr::collect()
      return(df)
    } else {
      return(df)
    }
  })
}


# ----- mean summarized table -----
#' @param data a `reactive()` data frame like object usually
#' created from another `create_function`. Considering
#' raw data is gathered through a PostgresSQL connection
#' these reactive objects tend to be `tbl_lazy`.
#' @param input_source usually an object created by a sidebar
#' function. These objects tend to be `reactive()` outcomes from
#' `observe()` or `observeEvent()` calls within a module.
#'
#' @name create_functions
#' @export
create_mean_data <- function(data, input_source) {
  shiny::reactive({
    df <- data()

    summary_grouping_vars <- input_source$grouping_vars()
    y_vals <- input_source$y_variable()

    # cli check
    error_mean_data(
      df = df,
      summary_grouping_vars = summary_grouping_vars,
      y_vals = y_vals
    )

    # can create base_df
    base_df <- df |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c(
        "organism_type",
        "data_type",
        summary_grouping_vars
      )))) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::ungroup()

    # if selected_vars is null just produce base query -----
    if (is.null(y_vals) || length(y_vals) == 0) {
      # Return just the grouped counts
      cli::cli_inform("No y_variable selected → returning grouped n only")
      grouped_summary_df <- base_df |>
        dplyr::collect() |>
        dplyr::arrange(dplyr::across(dplyr::any_of(summary_grouping_vars)))

      cli::cli_ul(c(
        "collected class: {paste(class(grouped_summary_df), collapse = ', ')}",
        "rows: {nrow(grouped_summary_df)}"
      ))
      return(grouped_summary_df)
    }

    summary_list <- lapply(y_vals, function(v) {
      mapped_var <- fix_var_generic(df = df, var_raw = v)

      df_filtered <- mapped_var$df
      var_to_summarise <- mapped_var$var
      var_label <- mapped_var$var_label

      cli::cli_inform("var_to_summarise: {.field {var_to_summarise}}")
      cli::cli_inform("Available columns: {.val {colnames(df_filtered)}}")

      # Check if variable exists after filtering
      if (!var_to_summarise %in% colnames(df_filtered)) {
        cli::cli_warn(
          "Skipping {.field {v}} — column not present after mapping"
        )
        return(NULL)
      }

      grouped_summary_df <- df_filtered |>
        dplyr::group_by(dplyr::across(dplyr::all_of(summary_grouping_vars))) |>
        dplyr::summarise(
          !!paste0(var_label, " (mean)") := mean(
            .data[[var_to_summarise]],
            na.rm = TRUE
          ),
          !!paste0(var_label, " (sd)") := sd(
            .data[[var_to_summarise]],
            na.rm = TRUE
          ),
        ) |>
        dplyr::ungroup()
    })
    # Remove NULL results
    summary_list <- summary_list[!sapply(summary_list, is.null)]
    shiny::req(length(summary_list) > 0)

    # Combine all summaries by joining on grouping vars
    #  # can use  init = base_df
    grouped_summary_df <- Reduce(
      function(x, y) {
        dplyr::full_join(x, y, by = summary_grouping_vars)
      },
      summary_list
      # init = base_df
    )
    # run query x
    grouped_summary_df <- grouped_summary_df |>
      dplyr::left_join(
        base_df,
        by = summary_grouping_vars,
        na_matches = "na"
      ) |>
      dplyr::relocate(
        "organism_type",
        "data_type",
        .before = dplyr::everything()
      ) |>
      dplyr::relocate(
        n,
        .after = dplyr::all_of(utils::tail(summary_grouping_vars, 1))
      ) |>
      dplyr::filter(
        dplyr::if_any(contains("(mean)"), ~ !is.na(.x))
      ) |>
      dplyr::collect() |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c(
        "organism_type",
        "data_type",
        summary_grouping_vars
      )))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          \(x) {
            vals <- x[!is.na(x)]
            if (length(vals) == 0L) NA else vals[[1L]]
          }
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(
        "organism_type",
        "data_type",
        summary_grouping_vars
      )))) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))

    return(grouped_summary_df)
  })
}

# ----- create raw data -----
#' @param activated is a `shiny` reactive value which gets triggered
#' when the tab is activated. This makes it so that the tab stays the
#' same when clicking away but also that it doesn't run
#' the tab upon start up nor when clicking away.
#' @param con a `DBI` conection to, in this case PostgreSQL database
#' @param input_source usually an object created by a sidebar
#' function. These objects tend to be `reactive()` outcomes from
#' `observe()` or `observeEvent()` calls within a module.
#' @param main_input the `main_input` from the shiny server.
#' @param tab the selected tab that is being displayed e.g.,
#' `"scatter_plot". `
#' @param var_field a `character` string that identifies the object
#' in the `list` that is supplied to `input_source`. This object
#' name is the variable field that is of interest for the particular
#' use for a given function.
#'
#' @name create_functions
#' @export
create_raw_data <- function(
  activated = NULL,
  con,
  input_source,
  main_input,
  tab = NULL,
  var_field
) {
  shiny::reactive({
    # use for other tabs ---
    if (!is.null(tab)) {
      error_tab_name(tab)

      shiny::req(main_input$tabs == tab)
    }

    if (!is.null(activated)) {
      shiny::req(activated)
    }

    # get connection
    con_db <- if (inherits(con, "reactive")) con() else con

    # get selected vars
    # Handle multiple var_fields
    selected_vars <- c()

    for (field in var_field) {
      vars <- input_source[[field]]
      vars_val <- if (inherits(vars, "reactive")) vars() else vars
      cli::cli_alert_info(
        "Field: {field}, Value: {vars_val}, Length: {length(vars_val)}"
      )
      selected_vars <- c(selected_vars, vars_val)
    }

    # remove null selected_vars
    selected_vars <- unique(selected_vars[!is.null(selected_vars)])

    # alert
    cli::cli_alert("selected vars is: {.var {selected_vars}}")

    # check slected _vars
    error_selected_vars(selected_vars = selected_vars)

    # get groups
    gv <- input_source$grouping_vars

    group_vars <- if (inherits(gv, "reactive")) gv() else gv

    shiny::req(con_db)

    # ----- if grouping_vars is null or length is 0 return a null object all
    # together

    if (is.null(group_vars) || length(group_vars) == 0) {
      return(NULL)
    }

    shiny::req(con_db)

    # ----- if grouping_vars is null or length is 0 return a null object all
    # together

    # ---- actually get data when group_vars is valid ----
    df <- get_raw_data(
      con = con_db,
      grouping_vars = group_vars,
      selected_vars = selected_vars,
    )

    return(df)
  })
}
# ------- create filtered souce ------
#' @param collect a `logical` that determines whether to `collect()`
#' the data from the PostgreSQL database and turn into a `tibble`
#' object in a given session. Default is `TRUE`.`
#' @param con a `DBI` conection to, in this case PostgreSQL database
#' @param input a given shiny module and server input.
#' @param main_input the `main_input` from the shiny server.
#' @param source_data a reactive object that is `tbl_source`
#' a `tbl_lazy` object from a PostgreSQL database.
#' @param tbl_name a `character` string of the database name
#' to create searching data from e.g., `tbl_taxonomy`.
#'
#' @name create_functions
#' @export
create_searching_data <- function(
  collect = TRUE,
  con = NULL,
  input,
  source_data = NULL,
  tbl_name = NULL
) {
  shiny::reactive({
    if (!is.null(source_data)) {
      shiny::req(source_data)

      df <- if (shiny::is.reactive(source_data)) source_data() else source_data

      search_cols <- colnames(df)
    } else {
      if (is.null(con) || is.null(tbl_name)) {
        cli::cli_abort(
          "Must provide both {.arg con} and {.arg tbl_name} 
          when {.arg source_data} is {.val NULL}."
        )
      }

      df <- dplyr::tbl(con, tbl_name)
      search_cols <- df |>
        colnames()
    }

    cli::cli_alert_info("df is the following type: {.val {class(df)}}")

    search_term <- input$search_bar

    cli::cli_alert_success("Search term is: {.field {search_term}}")
    # Only filter if the user has typed something
    if (!is.null(search_term) && nzchar(trimws(search_term))) {
      df <- search_table(
        df = df,
        search_term = search_term,
        search_cols = search_cols
      )
    }
    if (isTRUE(collect) && !is.data.frame(df)) {
      df <- df |>
        dplyr::collect()
      df <- df |>
        dplyr::arrange(common_name)
    }
    if (isFALSE(collect)) {
      df <- df |>
        dplyr::select(-c(genus:class_sci, phylum, kingdom, organism_type, tsn))
    }

    current_names <- names(df)

    names(df) <- dplyr::coalesce(
      nice_name_lookup[current_names],
      current_names
    )

    return(df)
  })
}
# ----- create source data ------

#' @param activated is a `shiny` reactive value which gets triggered
#' when the tab is activated. This makes it so that the tab stays the
#' same when clicking away but also that it doesn't run
#' the tab upon start up nor when clicking away.
#' @param con a `DBI` conection to, in this case PostgreSQL database
#' @param main_input the `main_input` from the shiny server.
#' @param tab the selected tab that is being displayed e.g.,
#' `"source_info". `
#'
#' @name create_functions
#' @export
create_source_data <- function(
  activated = NULL,
  con,
  main_input,
  tab = NULL
) {
  shiny::reactive({
    # use for other tabs ---
    if (!is.null(tab)) {
      error_tab_name(tab)

      shiny::req(main_input$tabs == tab)
    }

    if (!is.null(activated)) {
      shiny::req(activated)
    }

    # get connection
    con_db <- if (inherits(con, "reactive")) con() else con

    source_data <- dplyr::tbl(con, "tbl_sources") |>
      dplyr::select(-submission_id, -email, -affiliation)

    data_tables <- data_tables()

    flag_cols <- paste0("has_", sub("tbl_", "", names(data_tables)))

    samples_data <- dplyr::tbl(con, "tbl_samples") |>
      dplyr::select(
        source_id,
        sample_id,
        common_name:class_sci,
        phylum,
        kingdom,
        organism_type,
        tsn
      )

    samples_data <- get_data_tables(
      con = con,
      df = samples_data,
      data_tables = data_tables,
      flag_cols = flag_cols,
      var = "sample_id"
    )

    # ── 3. Aggregate to unique locations ───────
    samples_data <- clean_data_tables(
      df = samples_data,
      flag_cols = flag_cols,
      type = data_tables,
      group_cols = c(
        source_id,
        common_name:class_sci,
        phylum,
        kingdom,
        organism_type,
        tsn
      ),
      filter_coords = FALSE
    ) |>
      dplyr::select(source_id:tsn, data_tables, n_samples)

    samples_data <- samples_data |>
      dplyr::group_by(source_id, data_tables) |>
      dplyr::summarise(
        dplyr::across(
          common_name:tsn,
          ~ paste(unique(.x), collapse = ", ")
        ),
        n_total = sum(n_samples),
        .groups = "drop"
      )

    source_data_joined <- source_data |>
      dplyr::collect() |>
      dplyr::left_join(samples_data) |>
      dplyr::select(-source_id)

    return(source_data_joined)
  })
}

# ---- sumary data -----
#' @param activated is a `shiny` reactive value which gets triggered
#' when the tab is activated. This makes it so that the tab stays the
#' same when clicking away but also that it doesn't run
#' the tab upon start up nor when clicking away.
#' @param con a `DBI` conection to, in this case PostgreSQL database
#' @param input_source usually an object created by a sidebar
#' function. These objects tend to be `reactive()` outcomes from
#' `observe()` or `observeEvent()` calls within a module.
#' @param main_input the `main_input` from the shiny server.
#' @param tab the selected tab that is being displayed e.g.,
#' `"summary_info". `
#' @param var_field a `character` string that identifies the object
#' in the `list` that is supplied to `input_source`. This object
#' name is the variable field that is of interest for the particular
#' use for a given function.
#'
#' @name create_functions
#' @export
create_summary_data <- function(
  activated = NULL,
  con,
  input_source,
  main_input,
  tab = NULL,
  var_field
) {
  shiny::reactive({
    # use for other tabs ---
    if (!is.null(tab)) {
      error_tab_name(tab)

      shiny::req(main_input$tabs == tab)
    }

    if (!is.null(activated)) {
      shiny::req(activated)
    }

    # get connection
    con_db <- if (inherits(con, "reactive")) con() else con

    # get selected vars
    # Handle multiple var_fields
    selected_vars <- c()

    for (field in var_field) {
      vars <- input_source[[field]]
      vars_val <- if (inherits(vars, "reactive")) vars() else vars
      cli::cli_alert_info(
        "Field: {field}, Value: {vars_val}, Length: {length(vars_val)}"
      )
      selected_vars <- c(selected_vars, vars_val)
    }

    # remove null selected_vars
    selected_vars <- unique(selected_vars[!is.null(selected_vars)])

    # alert
    cli::cli_alert("selected vars is: {.var {selected_vars}}")

    # check slected _vars
    error_selected_vars(selected_vars = selected_vars)

    # get groups

    gv <- input_source$grouping_vars

    group_vars <- if (inherits(gv, "reactive")) gv() else gv

    shiny::req(con_db)

    # ----- if grouping_vars is null or length is 0 return a null object all
    # together

    if (is.null(group_vars) || length(group_vars) == 0) {
      return(NULL)
    }
    # ---- actually get data when group_vars is valid ----
    df <- get_summary_data(
      con = con_db,
      selected_vars = selected_vars,
      grouping_vars = group_vars
    )

    return(df)
  })
}

# ----- creaete zoom slider -----
#' @param data a `reactive()` data frame like object usually
#' created from another `create_function`. Considering
#' raw data is gathered through a PostgresSQL connection
#' these reactive objects tend to be `tbl_lazy`.
#' @param input_source usually an object created by a sidebar
#' function. These objects tend to be `reactive()` outcomes from
#' `observe()` or `observeEvent()` calls within a module.
#'
#' @name create_functions
#' @export

create_zoom_slider <- function(
  data,
  input_source
) {
  shiny::reactive({
    df <- data()

    x_var_raw <- input_source$x_choices()

    # get the y var
    y_var_raw <- input_source$y_choices()

    # use generic function to filter and grab the correct for length only
    fix_x <- fix_var_generic(
      df = df,
      var_raw = x_var_raw
    )

    # get the returned objects which are returned in a list
    df <- fix_x$df
    x_var <- fix_x$var
    x_label <- fix_x$var_label

    # now do the same for y
    fix_y <- fix_var_generic(
      df = df,
      var_raw = y_var_raw
    )

    df <- fix_y$df
    y_var <- fix_y$var
    y_label <- fix_y$var_label

    # filter df by x and y vars
    df <- df |>
      dplyr::filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))

    ranges <- get_ranges(df, x_var, y_var)

    x_range <- ranges[["x_range_vec"]]
    y_range <- ranges[["y_range_vec"]]

    if (!is.null(x_range)) {
      cli::cli_alert(
        "x_range is: {.val {c(min = round(x_range[1]), max = round(x_range[2]))}}"
      )
      update_zoom_slider("zoom_x", x_range)
    }
    if (!is.null(y_range)) {
      cli::cli_alert(
        "y_range is: {.val {c(min = round(y_range[1]), max = round(y_range[2]))}}"
      )

      update_zoom_slider("zoom_y", y_range)
    }
  })
}

display_hist <- function(
  data,
  input_source,
  output,
  output_id = "summary_histogram"
) {
  output[[output_id]] <- shiny::renderPlot({
    # Get raw data (not summarized)
    df <- data() |>
      dplyr::collect()
    shiny::req(df, nrow(df) > 0)
    # Ensure the selected column exists in the raw data
    var <- input_source$hist_vars()

    cli::cli_alert_info("selected var initially is: {.field {var}}")

    cli::cli_alert_info(
      "colnames is present: {.val {any(colnames(df) %in% var)}}"
    )

    # detect length-type UI choices
    is_length <- grepl("length_mm", var, ignore.case = TRUE) &&
      !var %in% colnames(df)

    is_energy <- grepl("Joules/g", var, ignore.case = TRUE) &&
      !var %in% colnames(df)

    cli::cli_alert_info("colnames are: {.val {colnames(df)}}")
    if (is_length) {
      # Convert UI label to the length_type in the data
      length_type_val <- dplyr::case_when(
        grepl("fork", var, ignore.case = TRUE) ~ "fork",
        grepl("total", var, ignore.case = TRUE) ~ "total",
        grepl("standard", var, ignore.case = TRUE) ~ "standard",
        grepl("carapace", var, ignore.case = TRUE) ~ "carapace",
        .default = NA
      )

      # error_hist_ui(df = df, var = var, type_val = length_type_val,
      #               col = "length_type")

      shiny::req(!is.na(length_type_val))
      shiny::req("length_mm" %in% colnames(df))
      shiny::req("length_type" %in% colnames(df))

      error_hist_vars(df, var = "length_mm", ba = "before")

      df <- df |>
        dplyr::filter(length_type == length_type_val) |>
        dplyr::mutate(length_mm = suppressWarnings(as.numeric(length_mm))) |>
        dplyr::filter(!is.na(length_mm))

      error_hist_vars(df, var, ba = "after")

      var <- "length_mm"
    } else if (is_energy) {
      # Convert UI label to the length_type in the data
      energy_type_val <- dplyr::case_when(
        grepl(
          "Joules/g dry weight",
          var,
          ignore.case = TRUE
        ) ~ "Joules/g dry weight",
        grepl(
          "Joules/g wet weight",
          var,
          ignore.case = TRUE
        ) ~ "Joules/g wet weight",

        .default = NA
      )

      # error_hist_ui(df, var, type_val = energy_type_val)

      shiny::req(!is.na(energy_type_val))
      shiny::req("energy_measurement" %in% colnames(df))
      shiny::req("energy_units" %in% colnames(df))

      error_hist_vars(df, var = "energy_measurement", ba = "before")

      df <- df |>
        dplyr::filter(energy_units == energy_type_val) |>
        dplyr::mutate(
          energy_measurement = suppressWarnings(
            as.numeric(energy_measurement)
          )
        ) |>
        dplyr::filter(!is.na(energy_measurement))

      error_hist_vars(df, var, ba = "after")

      var <- "energy_measurement"
    } else {
      # ---- NON-LENGTH VARIABLES ----
      cli::cli_alert_success("entered else statement")

      shiny::req(var %in% colnames(df))
      error_hist_vars(df, var, ba = "before")

      df <- df |>
        dplyr::mutate(dplyr::across(
          dplyr::all_of(var),
          ~ suppressWarnings(as.numeric(.))
        )) |>
        dplyr::filter(!is.na(.data[[var]]))

      error_hist_vars(df, var, ba = "after")
    }

    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()
    # Remove NAs from the selected column
    # df <- df |>
    #   filter(!is.na(.data[[var]]))

    nice_label <- convert_nice_name(var)[[1]]

    if (nice_label %in% "Length (mm)") {
      nice_label <- paste(
        stringr::str_to_title(length_type_val),
        convert_nice_name(var)[[1]],
        sep = " "
      )
    } else if (nice_label %in% "Energy Density") {
      nice_label <- paste(
        convert_nice_name(var)[[1]],
        " (",
        energy_type_val,
        ")",
        sep = ""
      )
    }

    title_text <- paste0(
      "Histogram of ",
      nice_label,
      "<br><b>Species:</b> ",
      fix_title_label(species_f),
      "<br><b>Waterbody:</b> ",
      fix_title_label(waterbody_f)
    )

    cli::cli_alert_info("selected var prior to plotting is: {.field {var}}")

    # Plot the histogram of the selected variable
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!rlang::sym(var))) +
      ggplot2::geom_histogram(fill = "#4DB6AC", color = "black") +
      # facet_wrap(~ common_name) +
      ggplot2::theme_bw(
        base_size = 15
      ) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5),
        axis.title.x = ggtext::element_markdown()
      ) +
      ggplot2::labs(
        x = nice_label,
        y = "Frequency",
        title = title_text
      )
    return(p)
  })
}

# ----- display scatter plot -----
display_scatter_plot <- function(
  data,
  input_source,
  output,
  output_id = "scatter_plot"
) {
  # ----- display scatter plot -----
  output[[output_id]] <- shiny::renderPlot({
    #     # Get raw data (not summarized)

    df <- data()

    x_var_raw <- input_source$x_choices()

    # get the y var
    y_var_raw <- input_source$y_choices()

    # ---- if data is null/no grouping varialbes display message -----
    if (is.null(df)) {
      p <- empty_plot(
        "Select one or more grouping variables from the sidebar.
        \n**Next** select your _x_ and _y_ variables of interest."
      )

      return(p)
    }
    if (check_empty_character(x_var_raw) | check_empty_character(y_var_raw)) {
      p <- empty_plot(
        "Grouping variables selected ✓\n\nNow 
      choose your **x** and **y** variables of interest."
      )

      return(p)
    }

    cli::cli_alert_warning("df class: {.val {class(df)}}")

    # get the basic grouping

    scatter_grouping_vars <- input_source$grouping_vars()

    # ---- get the number of groups ----
    n_groups <- length(scatter_grouping_vars)

    cli::cli_alert_info("The number of groups is {.field {n_groups}}")

    # ---- too many groups ----
    if (n_groups > 3) {
      p <- empty_plot(
        "You have selected 4 or more grouping variables.\nPlease select 3 or fewer."
      )
      return(p)
    }

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

    # ----- create nice title -----
    species_f <- input_source$species_filter()
    waterbody_f <- input_source$waterbody_filter()

    title_text <- paste0(
      "Scatter plot of ",
      y_label,
      " by ",
      x_label,
      "<br><b>Species:</b> ",
      fix_title_label(species_f),
      "<br><b>Waterbody:</b> ",
      fix_title_label(waterbody_f)
    )

    # use convert_nice_name to make title nice
    legend_title <- convert_nice_name(scatter_grouping_vars[1])
    # Show what the labels look like in console with cli formatting
    cli::cli_h2("Plot Labels")
    cli::cli_text("X: {.val {x_label}}")
    cli::cli_text("Y: {.val {y_label}}")

    # Debug with cli
    cli::cli_alert_info("x_label raw: {x_label}")
    cli::cli_alert_info("x_label no quotes: {gsub('\"', '', x_label)}")
    cli::cli_alert_info("y_label raw: {y_label}")
    cli::cli_alert_info("y_label no quotes: {gsub('\"', '', y_label)}")

    # Then strip quotes before using in labs
    x_label <- gsub('"', '', x_label)
    y_label <- gsub('"', '', y_label)

    # ----- plot -----
    p <- ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = !!rlang::sym(x_var),
        y = !!rlang::sym(y_var)
      )
    ) +
      ggplot2::scale_fill_viridis_d(
        name = legend_title,
        option = "B",
        begin = 0.1,
        end = 0.9,
        alpha = 0.5
      ) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        legend.title = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        strip.background = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = x_label,
        y = y_label,
        title = title_text
      )

    if (n_groups >= 1) {
      p <- p +
        ggplot2::geom_point(
          ggplot2::aes(fill = !!rlang::sym(scatter_grouping_vars[1])),
          alpha = 0.7,
          size = 5,
          shape = 21
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          alpha = 0.7,
          size = 3,
          shape = 21
        )
    }
    # ---- Faceting logic ----
    if (n_groups %in% 2) {
      # Second variable facet_wrap
      p <- p +
        ggplot2::facet_wrap(
          ggplot2::vars(!!rlang::sym(scatter_grouping_vars[2]))
        )
    }

    if (n_groups %in% 3) {
      # Second + third  facet_grid
      p <- p +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(scatter_grouping_vars[2])),
          cols = ggplot2::vars(!!rlang::sym(scatter_grouping_vars[3]))
        )
    }

    return(p)
  })
}
# ----- dsplay_submsiion_id -----
display_submission_map <- function(
  output,
  ns,
  output_id = "map",
  split_tables
) {
  output[[output_id]] <- leaflet::renderLeaflet({
    tbl_locs <- split_tables$tbl_location
    tbl_samp <- split_tables$tbl_samples |>
      dplyr::select(sample_id, user_sample_id)

    shiny::req(tbl_locs)
    shiny::req(tbl_samp)

    location_summary <- tbl_locs |>
      dplyr::left_join(
        tbl_samp
      ) |>
      dplyr::group_by(latitude, longitude) |>
      dplyr::summarise(
        sample_ids = paste(unique(user_sample_id), collapse = ", "),
        n_samples = dplyr::n_distinct(sample_ids),
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        latitude = as.numeric(latitude),
        longitude = as.numeric(longitude)
      )

    # Show the actual coordinates for debugging
    cli::cli_alert_info(
      "Locations validated: {nrow(location_summary)} location{?s} 
      ({min(location_summary$n_samples)}-{max(location_summary$n_samples)} 
      samples per location)"
    )
    cli::cli_alert_info(
      "Coordinates: lat range [{min(location_summary$latitude, na.rm=TRUE)}, 
      {max(location_summary$latitude, na.rm=TRUE)}], 
      lon range [{min(location_summary$longitude, na.rm=TRUE)}, 
      {max(location_summary$longitude, na.rm=TRUE)}]"
    )

    # Check for issues
    if (
      any(is.na(location_summary$latitude)) ||
        any(is.na(location_summary$longitude))
    ) {
      cli::cli_alert_warning("Some coordinates are NA!")
    }
    if (any(abs(location_summary$latitude) > 105, na.rm = TRUE)) {
      cli::cli_alert_warning(
        "Some latitudes are out of range (-105 to 105)!"
      )
    }
    if (any(abs(location_summary$longitude) > 180, na.rm = TRUE)) {
      cli::cli_alert_warning(
        "Some longitudes are out of range (-180 to 180)!"
      )
    }

    leaflet::leaflet(location_summary) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = "#0066cc",
        fillColor = "#3399ff",
        fillOpacity = 0.7,
        popup = ~ paste0(
          "<b>Number of samples:</b> ",
          n_samples,
          "<br>",
          "<b>Sample ID(s):</b> ",
          sample_ids
        ),
        label = ~ paste0(n_samples, " sample(s)")
      )
  })
}


# ----- dsiplay submssion map info ------
display_sub_map_msg <- function(
  output,
  ns,
  output_id = "location_map",
  split_tables,
  validated_submission,
  validated_sources,
  validated_samples
) {
  output[[output_id]] <- shiny::renderUI({
    shiny::req(split_tables)

    shiny::req(
      validated_submission(),
      validated_sources(),
      validated_samples()
    )
    tbl_loc <- split_tables$tbl_location

    if (all(is.na(tbl_loc$latitude)) & all(is.na(tbl_loc$longitude))) {
      shiny::tagList(
        shiny::h4(
          "No locations were detected in the longtiude and latitude
                  columns of your submitted data.
                  If this is correct, please proceed to submitting
                  the data to the database",
          style = "margin-top: 20px; margin-bottom: 10px;"
        )
      )
    } else {
      shiny::tagList(
        shiny::h4(
          "Please check that your sample locations, the number of samples,
                  and their corresponding ids are correct prior to submitting to
                  the database. To check, click on each point
                  to view the number of samples and the user submitted sample ids.",
          style = "margin-top: 20px; margin-bottom: 10px;"
        ),
        leaflet::leafletOutput(ns("map"), height = "500px")
      )
    }
  })
}

# ---- display summary_table -----

display_table <- function(data, output, output_id = "summary_table_output") {
  output[[output_id]] <- DT::renderDT({
    shiny::req(data())
    # get data
    df <- data()

    # validate data
    shiny::validate(
      shiny::need(is.data.frame(df), "Waiting for data…"),
      shiny::need(nrow(df) > 0, "No data available")
    )

    # display data
    DT::datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE
        # autoWidth = TRUE
      ),
      escape = FALSE
    )
  })
}

# ----- display upload status ------
display_validation_status <- function(
  output,
  ns,
  output_id = "upload_status",
  split_tables = NULL,
  validated = TRUE
) {
  output[[output_id]] <- shiny::renderUI({
    if (validated) {
      tbl_samp <- split_tables$tbl_samples

      shiny::tagList(
        shiny::p(
          "✔ All validations passed",
          style = "color:green; font-weight:600;"
        ),
        shiny::p(
          paste0(
            "Ready to submit ",
            nrow(tbl_samp),
            " rows to database."
          ),
          style = "color:green;"
        )
      )
    } else {
      shiny::tagList(
        shiny::p(
          "✖ Validation failed - please fix the following issues:",
          style = "color:red; font-weight:600;"
        ),
        shiny::tableOutput(ns("error_table"))
      )
    }
  })
}

# ---- dispaly upload status ------
display_upload_status <- function(
  output,
  ns,
  output_id = "upload_status",
  upload_succeeded = NULL,
  submission_results = NULL
) {
  output[[output_id]] <- shiny::renderUI({
    if (!upload_succeeded) {
      shiny::HTML(
        "<span style='color: red;'>
           ✘ Upload failed — no data was saved. Please check your data and try again.
         </span>"
      )
    } else {
      msg <- lapply(names(submission_results), function(tbl_name) {
        res <- submission_results[[tbl_name]]
        paste0(
          "✔ ",
          tbl_name,
          ": ",
          res$rows_submitted,
          " rows submitted",
          if (!is.na(res$submission_id)) {
            paste0(", submission_id = ", res$submission_id)
          } else {
            ""
          }
        )
      })

      shiny::HTML(
        paste0(
          "<span style='color: green;'>",
          paste(msg, collapse = "<br>"),
          "</span>"
        )
      )
    }
  })
}

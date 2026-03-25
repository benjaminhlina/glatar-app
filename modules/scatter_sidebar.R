# ----- scatter sidebar -----
scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("scatter_ui"),
      style = "display:none;",
      shiny::selectInput(
        ns("themes"),
        "Select a theme",
        choices = NULL,
        # multiple = TRUE
      ),
      shiny::selectInput(
        ns("scatter_grouping_vars"),
        "Select Grouping Variables",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectInput(
        ns("scatter_data_types"),
        "Select a data type",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectInput(
        ns("scatter_waterbody_filter"),
        "Select Waterbody",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectInput(
        ns("scatter_organism_type"),
        "Select Species",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectInput(
        ns("scatter_species_filter"),
        "Select Species",
        choices = NULL,
        multiple = TRUE
      ),

      shiny::selectizeInput(
        ns("x_var"),
        "Select X Variable",
        choices = NULL,
        options = list(
          render = I(
            "
                             {
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }"
          )
        )
      ),
      shiny::selectizeInput(
        ns("y_var"),
        "Select Y Variable",
        choices = NULL,
        options = list(
          render = I(
            "
          {
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }
        "
          )
        )
      ),
      shiny::div(
        id = ns("grouping_message"),
        style = "padding: 10px;
                     margin: 10px 0;
                     background-color: #000000;
                     border-left: 4px solid #2196F3;
                     border-radius: 4px;",
        shiny::icon("info-circle"),
        " Select one or more grouping variables to generate a plot"
      )
    )
  )
}
# ---- scatter_sidebar_server -----
scatter_sidebar_server <- function(id, con, main_input) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shinyjs::toggle(
        id = "scatter_ui",
        condition = main_input$tabs == "scatter_plot"
      )
    })

    # ---- intialize scatter plot -----
    initialized_scatter <- shiny::reactiveVal(FALSE)
    # Store computed values so the reactive can access them
    numeric_choices_r <- shiny::reactiveVal(NULL)
    numeric_names_r <- shiny::reactiveVal(NULL)
    length_vars_r <- shiny::reactiveVal(NULL)
    energy_vars_r <- shiny::reactiveVal(NULL)

    axis_choices <- shiny::reactive({
      shiny::req(input$themes)
      shiny::req(numeric_choices_r())
      get_theme_choices(
        theme = input$themes,
        con = con,
        numeric_choices = numeric_choices_r(),
        numeric_names = numeric_names_r(),
        length_vars = length_vars_r(),
        energy_vars = energy_vars_r()
      )
    })

    # ---- observe events -----
    shiny::observeEvent(
      main_input$tabs,
      {
        # require scatter tab and initalize scatter
        shiny::req(main_input$tabs == "scatter_plot")
        shiny::req(!initialized_scatter())

        # get sidebar df
        sidebar_df <- get_sidebar_df(con)

        # make input for filters to exclude all when it hits
        scatter_filters <- c(
          "scatter_waterbody_filter",
          "scatter_species_filter",
          "scatter_data_types",
          "scatter_organism_type"
        )

        purrr::walk(
          scatter_filters,
          ~ exclusive_all_observer(input, session, .x)
        )

        # get use the reactive
        df <- sidebar_df()
        shiny::req(df)
        theme_choices <- c(
          "Energy Density",
          "Body Composition",
          "Stable Isotopes",
          "Amino Acids",
          "Fatty Acids",
          "Contaminates",
          "Thiamine"
        )

        # ---- get data types -----
        data_types_choices <- c(
          "Individual",
          "Composite",
          "Mean",
          "SD",
          "Equation"
        )

        # get grouping
        grouping_choices <- get_groups(df) |>
          sort()

        grouping_choices <- stats::setNames(
          grouping_choices,
          convert_nice_name(grouping_choices)
        )

        # get numerical vals
        numeric_choices <- get_numeric_vars(con)

        # remove columns we don't want/need that are numerics
        numeric_choices <- setdiff(
          numeric_choices,
          c(
            "calorimeter_conversion_factor",
            "issue",
            "length_mm",
            "energy_measurement",
            "latitude",
            "longitude",
            "month",
            "publication_year",
            "site",
            "site_depth",
            "source_id",
            "user_sample_id",
            "sample_year",
            "volume"
          )
        )
        # make numeric names nice
        numeric_names <- convert_nice_name(numeric_choices)

        # get long variables that we need wide
        length_vars <- get_var_types(df, var = "length_type")
        energy_vars <- get_var_types(df, var = "energy_units")

        # create axis choices
        # axis_choices <- sort(c(
        #   setNames(numeric_choices, numeric_names),
        #   length_vars,
        #   energy_vars
        # ))
        numeric_choices_r(numeric_choices)

        numeric_names_r(numeric_names)
        length_vars_r(length_vars)
        energy_vars_r(energy_vars)

        # waterbody
        waterbody_choices <- get_dropdown_choices(df, "waterbody")
        # species
        species_choices <- get_dropdown_choices(df, "scientific_name")

        organism_choices <- get_dropdown_choices(df, "organism_type")

        # get info for check dropdown
        n_wb <- length(waterbody_choices)
        n_sp <- length(species_choices)
        grp <- paste(grouping_choices, collapse = ', ')

        # error_dropdowns()
        cli::cli_alert_success("Updating dropdowns")
        cli::cli_alert_danger("scatter_sidebar tiggered")
        cli::cli_ul(c(
          "Waterbody unique values: {.val {n_wb}}",
          "Species unique values: {.val {n_sp}}",
          "Grouping choices: {.val {grp}}"
        ))

        # # ----- create themes -----
        shiny::updateSelectInput(
          session,
          "themes",
          choices = theme_choices
        )

        # grouping choices
        shiny::updateSelectInput(
          session,
          "scatter_grouping_vars",
          choices = grouping_choices,
        )
        # ---- create data choices -----
        shiny::updateSelectInput(
          session,
          "scatter_data_types",
          choices = c("All", data_types_choices),
          selected = c("All")
        )

        # Waterbody Drop-down
        shiny::updateSelectInput(
          session,
          "scatter_waterbody_filter",
          choices = c("All", waterbody_choices),
          selected = "All"
        )

        # Species Drop-down
        shiny::updateSelectInput(
          session,
          "scatter_organism_type",
          choices = c("All", organism_choices),
          selected = "All"
        )
        # Species Drop-down
        shiny::updateSelectInput(
          session,
          "scatter_species_filter",
          choices = c("All", species_choices),
          selected = "All"
        )
        # make x_choices
        initialized_scatter(TRUE)
      },
      ignoreInit = FALSE
    )

    # ----- add in toggle for grouping var on sidebar ------
    shiny::observe({
      if (
        is.null(input$scatter_grouping_vars) ||
          length(input$scatter_grouping_vars) == 0
      ) {
        shinyjs::show("grouping_message")
      } else {
        shinyjs::hide("grouping_message")
      }
    })
    shiny::observe({
      shiny::req(axis_choices())
      nc <- paste(axis_choices(), collapse = ', ')
      cli::cli_alert_info(
        "Numeric choices: {.val {nc}}"
      )
      shiny::updateSelectizeInput(
        session,
        "x_var",
        choices = axis_choices(),
        server = TRUE,
        selected = "percent_water"
      )

      # make y_choices

      shiny::updateSelectizeInput(
        session,
        "y_var",
        choices = axis_choices(),
        server = TRUE,
        selected = "energy_units__Joules/g wet weight"
      )
    })

    # ---- export what we need from the severer ----
    # we need grouping and hist variables we also need the function

    return(list(
      data_types = shiny::reactive(input$scatter_data_types),
      organism_choices = shiny::reactive(input$scatter_organism_type),
      grouping_vars = shiny::reactive(input$scatter_grouping_vars),
      waterbody_filter = shiny::reactive(input$scatter_waterbody_filter),
      species_filter = shiny::reactive(input$scatter_species_filter),
      y_choices = shiny::reactive(input$y_var),
      x_choices = shiny::reactive(input$x_var)
    ))
  })
}

raw_data_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("raw_data_ui"),
      style = "display:none;",
      shiny::conditionalPanel(
        condition = "input.tabs == 'view_data'",
        shiny::selectInput(
          ns("themes"),
          "Select a theme",
          choices = NULL,
          # multiple = TRUE
        ),
        shiny::selectInput(
          ns("raw_selected_col"),
          "Select Columns of Interest",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("raw_data_types"),
          "Select a data type",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("raw_waterbody_filter"),
          "Select Waterbody",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("raw_organism_type"),
          "Select Organism Type",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("raw_species_filter"),
          "Select Species",
          choices = NULL,
          multiple = TRUE
        ),

        shiny::selectizeInput(
          ns("raw_y_variable"),
          "Select Data Columns of Interest",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = 'Select columns...',
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
        shiny::downloadButton(
          ns("download_raw"),
          "Download Raw Data as Excel",
          class = "btn-primary",
          style = "margin-left: 15px; margin-top: 10px;
                                width: 245px"
        )
      )
    )
  )
}


raw_data_sidebar_server <- function(id, con, main_input) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shinyjs::toggle(
        id = "raw_data_ui",
        condition = main_input$tabs == "view_data"
      )
    })

    # ---- initalize ------
    initialized <- reactiveVal(FALSE)

    # Store computed values so the reactive can access them
    numeric_choices_r <- shiny::reactiveVal(NULL)
    numeric_names_r <- shiny::reactiveVal(NULL)
    length_vars_r <- shiny::reactiveVal(NULL)
    energy_vars_r <- shiny::reactiveVal(NULL)

    raw_choices <- reactive({
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

    # --- get sidebar info -----
    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(main_input$tabs == "view_data")
        shiny::req(!initialized())

        sidebar_df <- get_sidebar_df(con)

        filters <- c(
          "raw_waterbody_filter",
          "raw_species_filter",
          "raw_data_types",
          "raw_organism_type"
        )

        purrr::walk(filters, ~ exclusive_all_observer(input, session, .x))

        # get df
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

        # get grouping snad numerical values

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

        grouping_choices <- get_groups(df) |>
          setdiff(base_col) |>
          sort()

        grouping_choices <- stats::setNames(
          grouping_choices,
          convert_nice_name(grouping_choices)
        )

        numeric_choices <- get_numeric_vars(con)

        # ---- remove grouping or non needed variblaes ----

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
        numeric_names <- convert_nice_name(numeric_choices)
        # get length variables
        length_vars <- get_var_types(df, var = "length_type")
        energy_vars <- get_var_types(df, var = "energy_units")

        # Store into reactiveVals so summary_choices reactive can use them
        numeric_choices_r(numeric_choices)

        numeric_names_r(numeric_names)
        length_vars_r(length_vars)
        energy_vars_r(energy_vars)

        # watervody
        waterbody_choices <- get_dropdown_choices(df, "waterbody")
        # species
        species_choices <- get_dropdown_choices(df, "scientific_name")

        organism_choices <- get_dropdown_choices(df, "organism_type")

        # get info to make pretty console info
        n_wb <- length(waterbody_choices)
        n_sp <- length(species_choices)
        grp <- paste(grouping_choices, collapse = ', ')
        # check_dropdowns()
        cli::cli_alert_success("Updating dropdowns")
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
        # ---- create data choices -----
        shiny::updateSelectInput(
          session,
          "raw_data_types",
          choices = c("All", data_types_choices),
          selected = c("All")
        )
        # waterbody this needs to be reactive
        shiny::updateSelectInput(
          session,
          "raw_waterbody_filter",
          choices = c("All", waterbody_choices),
          selected = "All"
        )
        # Species Drop-down

        shiny::updateSelectInput(
          session,
          "raw_organism_type",
          choices = c("All", organism_choices),
          selected = "All"
        )
        # Species Drop-down

        shiny::updateSelectInput(
          session,
          "raw_species_filter",
          choices = c("All", species_choices),
          selected = "All"
        )
        shiny::updateSelectInput(
          session,
          "raw_selected_col",
          choices = c("All", grouping_choices),
          selected = "All"
        )

        # Update y summary  variable choices

        # set inalize as true to make this trigger once it is hit
        initialized(TRUE)
      },
      ignoreInit = TRUE
    )
    # Update y summary  variable choices
    shiny::observe({
      shiny::req(raw_choices())
      nc <- paste(raw_choices(), collapse = ', ')
      cli::cli_alert_info(
        "Numeric choices: {.val {nc}}"
      )
      shiny::updateSelectizeInput(
        session,
        "raw_y_variable",
        choices = raw_choices(),
        server = TRUE
      )
    })
    # make this into a function that sidebar exports out
    register_raw <- function(input_source) {
      output$download_raw <- shiny::downloadHandler(
        filename = function() {
          paste0("glatar_raw_tbl_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          shiny::req(input_source)
          df <- input_source$raw_df()()
          req(df)
          writexl::write_xlsx(df, file)
        }
      )

      shiny::observe({
        shiny::req(input$tabs == "view_data")
        shiny::req(input_source)
        df <- input_source$raw_df()()

        # toggle button
        shinyjs::toggleState(
          session$ns("download_raw"),
          condition = !is.null(df) && nrow(df) > 0
        )
      })
    }

    # ----- export what we need from the server ----
    # we need grouping and hist variables we also need the function

    return(list(
      data_types = shiny::reactive(input$raw_data_types),
      organism_type = shiny::reactive(input$raw_organism_type),
      grouping_vars = shiny::reactive(input$raw_selected_col),
      waterbody_filter = shiny::reactive(input$raw_waterbody_filter),
      species_filter = shiny::reactive(input$raw_species_filter),
      y_variable = shiny::reactive(input$raw_y_variable),
      register_raw = register_raw
    ))
  })
}

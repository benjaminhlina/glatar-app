source_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("source_ui"),
      style = "display:none;",
      shiny::conditionalPanel(
        condition = "input.tabs == 'view_source'",
        shiny::selectInput(
          ns("themes"),
          "Select a theme",
          choices = NULL,
          # multiple = TRUE
        ),
        shiny::selectInput(
          ns("source_waterbody_filter"),
          "Select Waterbody",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("source_organism_type"),
          "Select Organism Type",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          ns("source_species_filter"),
          "Select Species",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::downloadButton(
          ns("download_source"),
          "Download Source Material as Excel",
          class = "btn-primary",
          style = "margin-left: 15px; margin-top: 10px;
                                width: 245px"
        )
      )
    )
  )
}


source_sidebar_server <- function(id, con, main_input) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shinyjs::toggle(
        id = "source_ui",
        condition = main_input$tabs == "view_source"
      )
    })

    # ---- initalize ------
    initialized <- shiny::reactiveVal(FALSE)

    # --- get sidebar info -----
    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(main_input$tabs == "view_source")
        shiny::req(!initialized())

        sidebar_df <- get_sidebar_df(con)

        filters <- c(
          "source_waterbody_filter",
          "source_species_filter",
          "source_organism_type"
        )

        purrr::walk(filters, ~ exclusive_all_observer(input, session, .x))

        # get df
        df <- sidebar_df()
        shiny::req(df)

        theme_choices <- themes()
        # ---- get data types -----
        data_types_choices <- data_types()

        # watervody
        waterbody_choices <- get_dropdown_choices(df, "waterbody")
        # species
        species_choices <- get_dropdown_choices(df, "scientific_name")

        organism_choices <- get_dropdown_choices(df, "organism_type")

        # get info to make pretty console info
        n_wb <- length(waterbody_choices)
        n_sp <- length(species_choices)
        # check_dropdowns()
        cli::cli_alert_success("Updating dropdowns")
        cli::cli_alert_danger("source_sidebar tiggered")
        cli::cli_ul(c(
          "Waterbody unique values: {.val {n_wb}}",
          "Species unique values: {.val {n_sp}}"
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
          "source_data_types",
          choices = c("All", data_types_choices),
          selected = c("All")
        )
        # waterbody this needs to be reactive
        shiny::updateSelectInput(
          session,
          "source_waterbody_filter",
          choices = c("All", waterbody_choices),
          selected = "All"
        )

        # Organsim  Drop-down

        shiny::updateSelectInput(
          session,
          "source_organism_type",
          choices = c("All", organism_choices),
          selected = "All"
        )

        # Species Drop-down

        shiny::updateSelectInput(
          session,
          "source_species_filter",
          choices = c("All", species_choices),
          selected = "All"
        )

        # Update y summary  variable choices

        # set inalize as true to make this trigger once it is hit
        initialized(TRUE)
      },
      ignoreInit = TRUE
    )
    # Update y summary  variable choices

    # make this into a function that sidebar exports out
    register_source <- function(input_source) {
      output$download_summary <- shiny::downloadHandler(
        filename = function() {
          paste0("glatar_summary_tbl_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          shiny::req(input_source)
          df <- input_source$summary_df()()
          shiny::req(df)
          writexl::write_xlsx(df, file)
        }
      )

      observe({
        shiny::req(input$tabs == "summary_info")
        shiny::req(input_source)
        df <- input_source$summary_df()()

        # toggle button
        shinyjs::toggleState(
          session$ns("download_summary"),
          condition = !is.null(df) && nrow(df) > 0
        )
      })
    }

    # ----- export what we need from the server ----
    # we need grouping and hist variables we also need the function

    return(list(
      data_types = shiny::reactive(input$source_data_types),
      organism_type = shiny::reactive(input$source_organism_type),
      waterbody_filter = shiny::reactive(input$source_waterbody_filter),
      species_filter = shiny::reactive(input$source_species_filter),
      register_source = register_source
    ))
  })
}

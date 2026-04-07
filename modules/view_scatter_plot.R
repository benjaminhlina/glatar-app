view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("scatter_ui"),
      style = "display:none;",
      shiny::h2("Scatter Plot"),
      shiny::p(
        "This panel displays scatter plots for your selected data.
                  Use the dropdowns to filter your results by either clicking and select a choice 
                  or choices from the dropdowns or by typing directly into the dropdown to search for
                  specific options. To clear selections, click on the dropdown and 
                  press Backspace which will clear a selected choice. 
                  All dropdowns can have multiple selections, except
                  the x and y variables and when the filters are set to 'All'."
      ),
      plot_ui(
        title = "Scatter Plot",
        plot_id = "scatter_plot",
        height = "600px",
        ns = ns
      ),
      shiny::hr(),
      shiny::h4("Plot Controls", style = "margin-top: 5px;"),
      shiny::fluidRow(
        shiny::column(4, pallete_selector(ns = ns)),
        shiny::column(
          4,
          zoom_slider_ui(ns = ns, label = "Zoom X Axis", id = "zoom_x")
        ),
        shiny::column(
          4,
          zoom_slider_ui(ns = ns, label = "Zoom Y Axis", id = "zoom_y")
        )
      )
    )
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  shiny::moduleServer(id, function(input, output, session) {
    # ---- namespaces -----
    ns <- session$ns

    # ----- add in ui ------
    shiny::observeEvent(
      main_input$tabs,
      {
        shinyjs::toggle(
          id = "scatter_ui",
          condition = main_input$tabs == "scatter_plot"
        )
      },
      ignoreInit = TRUE
    )

    # add activation
    scatter_activated <- shiny::reactiveVal(FALSE)

    # summary activate only if scatter_plot
    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(main_input$tabs == "scatter_plot")
        scatter_activated(TRUE)
      },
      ignoreInit = TRUE
    )

    # ---- Make scatter raw data -----
    scatter_data <- create_summary_data(
      con = con,
      main_input = main_input,
      input_source = scatter_sidebar_vals,
      tab = "scatter_plot",
      var_field = c(
        "x_choices",
        "y_choices"
      ),
      activated = scatter_activated
    )
    # ---- allow filtering -----
    filtered_scatter_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data,
      pane = "scatter_plot"
    )

    # ---- check filtered scatter  -----
    shiny::observeEvent(
      filtered_scatter_data(),
      {
        shiny::req(filtered_scatter_data())
        error_summary_data(filtered_scatter_data())
      },
      ignoreInit = TRUE
    )

    # ----- PLOT CONTROLS ------
    palette_inputs <- pallete_selector_server(input)
    zoom_inputs <- zoom_slider_server(input)

    observeEvent(
      {
        filtered_scatter_data()
        scatter_sidebar_vals$x_choices()
        scatter_sidebar_vals$y_choices()
      },
      {
        df <- filtered_scatter_data()
        req(df)

        var_map <- syn_var()

        x_choice <- scatter_sidebar_vals$x_choices()
        y_choice <- scatter_sidebar_vals$y_choices()

        x_var <- var_map[[x_choice]] %||% x_choice
        y_var <- var_map[[y_choice]] %||% y_choice

        ranges <- get_ranges(df, x_var, y_var)

        x_range <- ranges[["x_range_vec"]]
        y_range <- ranges[["y_range_vec"]]
        cli::cli_alert(
          "x_range is: {.val {c(min = round(x_range[1]), max = round(x_range[2]))}}"
        )
        cli::cli_alert(
          "y_range is: {.val {c(min = round(y_range[1]), max = round(y_range[2]))}}"
        )

        if (!is.null(x_range)) {
          update_zoom_slider("zoom_x", x_range)
        }
        if (!is.null(y_range)) update_zoom_slider("zoom_y", y_range)
      },
      ignoreNULL = TRUE
    )

    # ----- CREATE INPUTS -----
    input_sources <- c(
      scatter_sidebar_vals,
      palette_inputs,
      zoom_inputs
    )

    # ---- display plot -----
    display_scatter_plot(
      data = filtered_scatter_data,
      input_source = input_sources,
      output
    )
  })
}

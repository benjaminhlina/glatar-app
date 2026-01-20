view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(

    useShinyjs(),
    div(id = ns("scatter_ui"),
        style = "display:none;",
        h2("Scatter Plot"),
        plot_ui(title = "Scatter Plot",
                plot_id = "scatter_plot",
                height = "600px",
                ns = ns)
    )
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  moduleServer(id, function(input, output, session) {
    # ---- namespaces -----
    ns <- session$ns

    # ----- add in ui ------
    observeEvent(main_input$tabs, {
      shinyjs::toggle(
        id = "scatter_ui",
        condition = main_input$tabs == "scatter_plot"
      )
    }, ignoreInit = TRUE)

    # add activation
    scatter_activated <- reactiveVal(FALSE)


    # summary activate only if scatter_plot
    observeEvent(main_input$tabs, {
      req(main_input$tabs == "scatter_plot")
      scatter_activated(TRUE)
    }, ignoreInit = TRUE)

    # ---- Make scatter raw data -----
    scatter_data <- create_summary_data(con = con,
                                        main_input = main_input,
                                        input_source = scatter_sidebar_vals,
                                        tab = "scatter_plot",
                                        var_field = c(
                                          "x_choices",
                                          "y_choices"
                                        ), activated = scatter_activated())
    # ---- allow filtering -----
    filtered_scatter_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data)

    observeEvent(filtered_scatter_data(), {
      req(filtered_scatter_data())
      check_summary_data(filtered_scatter_data())
    }, ignoreInit = TRUE)

    # ---- display plot -----
    display_scatter_plot(data = filtered_scatter_data,
                         input_source = scatter_sidebar_vals,
                         output)

  # },
  # ignoreInit = TRUE
  # )
  }
  )
}

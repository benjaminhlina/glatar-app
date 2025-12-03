view_scatter_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          h2("Scatter Plot"),
                          plot_ui(title = "Scatter Plot",
                                  plot_id = "scatter_plot",
                                  height = "600px",
                                  ns = ns)
  )
}


scatter_plot_server <- function(id, con, main_input, scatter_sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    cat("[DEBUG] summary_info_server initialized for ID:", id, "\n")
    cat("[DEBUG] con object class:", class(con), "\n")

    # ---- namespaces
    ns <- session$ns


    scatter_data <- create_summary_data(
      con = con,
      main_input = main_input,
      tab = "scatter_plot",
      table_name_reactive = scatter_sidebar_vals$selected_table
    )

    check_summary_data(scatter_data)

    numeric_cols <- create_numeric_col(data = scatter_data)

    filtered_scatter_data <- create_filtered_data(
      input_source = scatter_sidebar_vals,
      data = scatter_data)

    display_scatter_plot(data = filtered_scatter_data,
                         input_source = scatter_sidebar_vals,
                         output)
  }
  )
}

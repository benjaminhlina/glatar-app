view_summary_info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(id = ns("summary_ui"),
        style = "display:none;",
        shiny::h2("Summary Statistics"),
        shiny::br(),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Summary Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(style = "overflow-x: auto; width: 100%;",
                DT::DTOutput(ns("summary_table_output")))
          )
        ),
        shiny::br(),
        plot_ui(title = "Summary Histograms",
                plot_id = "summary_histogram",
                height = "300px",
                ns = ns),
    )
  )
}

# ----- summmary server --------
summary_info_server <- function(id, con, main_input, summary_sidebar_vals) {
  moduleServer(id, function(input, output, session) {

    observeEvent(main_input$tabs, {
      shinyjs::toggle(
        id = "summary_ui",
        condition = main_input$tabs == "summary_info"
      )
    }, ignoreInit = TRUE)

    # ---- namespaces -----
    ns <- session$ns

    summary_export_df <- reactiveVal(NULL)

    summary_activated <- reactiveVal(FALSE)
    #
    # # ----- Cache input values to prevent reset -----
    # cached_vals <- reactiveVal(list())
    # # Update cache whenever inputs change (only when tab is active)
    # observe({
    #   req(main_input$tabs == "summary_info")
    #   # req(summary_activated())
    #
    #   current_vals <- summary_sidebar_vals
    #
    #   # Cache all the sidebar values
    # valid_vals <- list()
    #   for (name in names(current_vals)) {
    #     val <- current_vals[[name]]
    #     # Only cache if not NULL and not empty string
    #     if (!is.null(val) && length(val) > 0 && val != "") {
    #       valid_vals[[name]] <- val
    #     }
    #   }
    #
    # if (length(valid_vals) > 0) {
    #   cached_vals(valid_vals)
    # }
    # })
    #
    # # Create a reactive that returns cached values when tab is inactive
    # sidebar_vals_stable <- reactive({
    #   cache <- cached_vals()
    #
    #   if (main_input$tabs == "summary_info" && summary_activated()) {
    #     # Use live values when on tab, but fall back to cache for empty values
    #     current_vals <- summary_sidebar_vals()
    #
    #     # Merge: use current values if valid, otherwise use cached
    #     merged <- cache
    #     for (name in names(current_vals)) {
    #       val <- current_vals[[name]]
    #       if (!is.null(val) && length(val) > 0 && val != "") {
    #         merged[[name]] <- val
    #       }
    #     }
    #     merged
    #   } else {
    #     # Use cached values when off tab
    #     cache
    #   }
    # })
    # # ----- first create summary data -----
    observeEvent(main_input$tabs, {
      req(main_input$tabs == "summary_info")
      summary_data <- create_summary_data(con = con,
                                          main_input = main_input,
                                          input_source = summary_sidebar_vals,
                                          tab = "summary_info",
                                          var_field = "y_variable")
      # Cehck if summary is being triggered

      # filtered summary by waterbody and species
      filtered_summary_data <- create_filtered_data(
        input_source = summary_sidebar_vals,
        data = summary_data)

      # # ---- Generate Summary Statistics with Dynamic Grouping -----
      summary_mean_df <- create_mean_data(input_source = summary_sidebar_vals,
                                          data = filtered_summary_data)

      # ---- fix names ----

      summary_mean_df_names <- reactive({
        req(summary_mean_df())

        df <- summary_mean_df() |>
          dplyr::rename_with(~ convert_nice_name(.x))

      })


      # ---- check summary_data
      observeEvent(summary_mean_df_names(), {
        req(summary_mean_df_names())
        check_summary_data(summary_mean_df_names())
      }, ignoreInit = TRUE)

      #  ----- Render Summary Table -----
      display_table(data = summary_mean_df_names, output)

      # ---- create summary dats for histogram -----
      summary_data_hist <- create_summary_data(con = con,
                                          main_input = main_input,
                                          input_source = summary_sidebar_vals,
                                          tab = "summary_info",
                                          var_field = "hist_vars")
      # Cehck if summary is being triggered

      # filtered summary by waterbody and species
      filtered_summary_data_hist <- create_filtered_data(
        input_source = summary_sidebar_vals,
        data = summary_data_hist)

      # # ---- add in histogram ----
      display_hist(data = filtered_summary_data_hist,
                   input_source = summary_sidebar_vals,
                   output)

      # ----- grab reactive summary as recative val- ----
      summary_export_df(summary_mean_df_names)

    }, ignoreInit = TRUE)

    # return this so it can be exported -----
    return(list(
      summary_df = summary_export_df
    ))

  })
}


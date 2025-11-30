# mod_scatter_sidebar.R

scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "input.tabs == 'scatter_plot'",
      shiny::selectInput(ns("scatter_plots"),
                         "Select Table",
                         choices = c("Calorimetry" = "tbl_calorimetry",
                                     "Proximate Composition" = "tbl_proxcomp",
                                     "Isotopes" = "tbl_isotope")),
      shiny::selectInput(ns("scatter_grouping_vars"),
                         "Select Grouping Variables",
                         choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("scatter_waterbody_filter"),
                         "Select Waterbody", choices = NULL),
      shiny::selectInput(ns("scatter_species_filter"),
                         "Select Species", choices = NULL),
      shiny::selectInput(ns("x_var"),
                         "Select X Variable", choices = NULL)
    )
  )
}
# observe({
#   df <- summary_data()
#   req(df)
#
#   # if ("tbl_calorimetry" %in% input$selected_table) {
#   #   if ("Units" %in% names(df)) {
#   #     df <- df %>%
#   #       mutate("Energy Density",
#   #                     ~ case_when(
#   #                       # Units == "cal/g" ~ .x * 4.184,
#   #                       Units == "Joules / g" ~ .x,
#   #                       TRUE ~ .x
#   #                     )))
#   #   }
#   # }
#   # Grouping Variables: Allow dynamic selection
#   grouping_choices <- grouping_cols()
#
#   updateSelectInput(session, "scatter_grouping_vars",
#                     choices = grouping_choices,
#                     # selected = c("Waterbody",
#                     #              "Common Name")
#   )
#
#   # Waterbody Drop-down
#   updateSelectInput(session, "scatter_waterbody_filter",
#                     choices = c("All", sort(unique(df$Waterbody))),
#                     selected = "All")
#
#   # Species Drop-down
#   updateSelectInput(session, "scatter_species_filter",
#                     choices = c("All", sort(unique(df$`Common Name`))),
#                     selected = "All")
#   # Get numeric column choices
#   numeric_choices <- numeric_cols()
#
#   # Update histogram variable choices
#   updateSelectizeInput(session, "scatter_var",
#                        choices = setNames(numeric_choices, numeric_choices),
#                        # selected = "",
#                        server = TRUE)
#   # X Variable Options
#   x_choices <-c(
#     # "Total Length (mm)",
#     # "Fork Length (mm)",
#     "Weight")
#
#
#
#   x_choices <- x_choices[x_choices %in% names(df)]  # Ensure they exist
#
#   updateSelectInput(session, "x_var",
#                     choices = x_choices,
#                     selected = x_choices[1]
#   )
#
#
# })
# filtered_summary_dats <- reactive({
#   df <- summary_data()
#
#
#   req(df, input$scatter_grouping_vars)
#   # Apply filters - set ALL to no filter the data at all
#   if (!(input$scatter_waterbody_filter %in% "All")) {
#     df <- df %>%
#       filter(Waterbody == input$scatter_waterbody_filter)
#   }
#   if (!(input$scatter_species_filter %in% "All")) {
#     df <- df %>%
#       filter(`Common Name` == input$scatter_species_filter)
#   }
#   return(df)
# })
#
#
# output$scatter_plot <- renderPlot({
#   # Get raw data (not summarized)
#
#   df <- filtered_summary_dats()
#   req(df, input$scatter_var, input$x_var)
#
#   x_var <- input$x_var
#   y_var <- input$scatter_var
#   scatter_grouping_vars <- input$scatter_grouping_vars
#   req(x_var %in% names(df),
#       y_var %in% names(df))
#
#   df <- df %>%
#     filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
#   # nice_label <- get_nice_name(var)[[1]]
#   x_label <- get_nice_name(x_var)[[1]]
#   y_label <- get_nice_name(y_var)[[1]]
#
#   p <- ggplot(df, aes(
#     x = !!sym(x_var),
#     y = !!sym(y_var))) +
#     theme_bw(base_size = 15) +
#     theme(
#       panel.grid = element_blank(),
#       plot.title = element_markdown(hjust = 0.5),
#       axis.title = element_markdown(),
#       legend.title = element_markdown(),
#       legend.text = element_markdown()
#     ) +
#     labs(
#       x = x_label,
#       y = y_label,
#       title = paste("Scatter Plot of", y_label, "vs", x_label)
#     )
#
#   if (scatter_grouping_vars != "None") {
#     p <- p +
#       geom_point(
#         aes(fill = !!sym(scatter_grouping_vars)),
#         alpha = 0.7,
#         size = 5,
#         shape = 21
#       ) +
#       scale_fill_viridis_d(name = scatter_grouping_vars,
#                            option = "B",
#                            begin = 0.1,
#                            end = 0.9,
#                            alpha = 0.5
#       )
#   } else {
#     p <- p + geom_point(
#       alpha = 0.7,
#       size = 3,
#       shape = 21
#     )
#   }
#
#
#   p
# })

# ----- scatter sidebar -----
scatter_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(id = ns("scatter_ui"),
        style = "display:none;",
        shiny::selectInput(ns("scatter_grouping_vars"),
                           "Select Grouping Variables",
                           choices = NULL, multiple = TRUE),

        shiny::selectInput(ns("scatter_waterbody_filter"),
                           "Select Waterbody", choices = NULL,
                           multiple = TRUE),
        shiny::selectInput(ns("scatter_species_filter"),
                           "Select Species", choices = NULL,
                           multiple = TRUE),
        shiny::selectizeInput(ns("x_var"),
                           "Select X Variable", choices = NULL,
                           options = list(
                             render = I("
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
            render = I("
          {
            option: function(item, escape) {
              return '<div>' + item.label + '</div>';
            },
            item: function(item, escape) {
              return '<div>' + item.label + '</div>';
            }
          }
        ")
          )
        ),
        div(id = ns("grouping_message"),
            style = "padding: 10px;
                     margin: 10px 0;
                     background-color: #000000;
                     border-left: 4px solid #2196F3;
                     border-radius: 4px;",
            icon("info-circle"),
            " Select one or more grouping variables to generate a plot"
        )
    )
  )
}
# ---- scatter_sidebar_server -----
scatter_sidebar_server <- function(id, con, main_input) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggle(id = "scatter_ui",
                      condition = main_input$tabs == "scatter_plot")
    })

    # ---- intialize scatter plot -----
    initialized_scatter <- reactiveVal(FALSE)

    # ---- observe events -----
    observeEvent(main_input$tabs, {
      # require scatter tab and initalize scatter
      req(main_input$tabs == "scatter_plot")
      req(!initialized_scatter())

      # get sidebar df
      sidebar_df <- get_sidebar_df(con)

      # make input for filters to exclude all when it hits
      exclusive_all_observer(input, session, "scatter_waterbody_filter")
      exclusive_all_observer(input, session, "scatter_species_filter")

      # get use the reactive
      df <- sidebar_df()
      req(df)

      # get grouping
      grouping_choices <- get_groups(df) |>
        sort()

      grouping_choices <- setNames(grouping_choices,
                                   convert_nice_name(grouping_choices))

      # get numerical vals
      numeric_choices <- get_numeric_vars(con)

      # remove columns we don't want/need that are numerics
      numeric_choices <- setdiff(numeric_choices, c(
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
        "volume")
      )
      # make numeric names nice
      numeric_names <- convert_nice_name(numeric_choices)

      # get long variables that we need wide
      length_vars <- get_var_types(df, var = "length_type")
      energy_vars <- get_var_types(df, var = "energy_units")

      # create axis choices
      axis_choices <- sort(c(setNames(numeric_choices,
                                         numeric_names),
                                length_vars, energy_vars))

      # waterbody
      waterbody_choices <- df |>
        distinct(waterbody) |>
        # filter(!(is.na(waterbody))) |>
        arrange(waterbody) |>
        pull(waterbody)

      # species
      species_choices <-  df |>
        distinct(scientific_name) |>
        # filter(!(is.na(scientific_name))) |>
        arrange(scientific_name) |>
        pull(scientific_name)

      # get info for check dropdown
      n_wb <- length(waterbody_choices)
      n_sp <- length(species_choices)
      grp <- paste(grouping_choices, collapse = ', ')
      nc <- paste(axis_choices, collapse = ', ')

      # check_dropdowns()
      cli::cli_alert_success("Updating dropdowns")
      cli::cli_ul(c(
        "Waterbody unique values: {.val {n_wb}}",
        "Species unique values: {.val {n_sp}}",
        "Grouping choices: {.val {grp}}",
        "Numeric choices: {.val {nc}}"
      ))
      # Grouping Variables: Allow dynamic selection
      updateSelectInput(session, "scatter_grouping_vars",
                        choices = grouping_choices,
      )

      # Waterbody Drop-down
      updateSelectInput(session, "scatter_waterbody_filter",
                        choices = c("All", waterbody_choices),
                        selected = "All")

      # Species Drop-down
      updateSelectInput(session, "scatter_species_filter",
                        choices = c("All", species_choices),
                        selected = "All")
      # make x_choices

      updateSelectizeInput(session, "x_var",
                        choices = axis_choices,
                        server = TRUE,
                        selected = "age"
                        )

      # make y_choices

      updateSelectizeInput(session, "y_var",
                           choices = axis_choices,
                           server = TRUE,
                           selected = "age")

      initialized_scatter(TRUE)

    },
    ignoreInit = FALSE
    )

    # ----- add in toggle for grouping var on sidebar ------
    observe({
      if (is.null(input$scatter_grouping_vars) ||
          length(input$scatter_grouping_vars) == 0) {
        shinyjs::show("grouping_message")
      } else {
        shinyjs::hide("grouping_message")
      }
    })
    # ---- export what we need from the severer ----
    # we need grouping and hist variables we also need the function

    return(list(
      grouping_vars = reactive(input$scatter_grouping_vars),
      waterbody_filter = reactive(input$scatter_waterbody_filter),
      species_filter = reactive(input$scatter_species_filter),
      y_choices = reactive(input$y_var),
      x_choices = reactive(input$x_var)
    )
    )
}
)
}


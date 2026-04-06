# ---- load modules ------
source("load/load_functions.R")

load_scripts(folder = "modules", type = "modules")
load_scripts(folder = "R", type = "functions")


# ---- startup the app -----

source("startup/startup.R")


# ---- ui ------
ui <- shinydashboard::dashboardPage(
  # ----- title -----
  shinydashboard::dashboardHeader(
    title = "Great Lakes Aquatic Tissue Analysis Repository (GLATAR)",
    titleWidth = 500,
    shiny::tags$li(
      class = "dropdown",
      shiny::tags$a(
        href = "https://github.com/benjaminhlina/glatar-app",
        target = "_blank",
        shiny::icon("github", class = "fa-2x"),
        style = "padding-top: 10px; padding-bottom: 10px;"
      )
    )
  ),

  # ---- sidebar -----
  shinydashboard::dashboardSidebar(
    width = 275,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(
        "Home",
        tabName = "home",
        icon = shiny::icon("home")
      ),
      shinydashboard::menuItem(
        "How to use GLATAR",
        tabName = "how_to_use",
        icon = shiny::icon("readme")
      ),
      shinydashboard::menuItem(
        "Summary Tables",
        tabName = "summary_info",
        icon = shiny::icon("bar-chart")
      ),
      shinydashboard::menuItem(
        "Scatter Plot",
        tabName = "scatter_plot",
        icon = shiny::icon("chart-line")
      ),
      shinydashboard::menuItem(
        "View Raw Data",
        tabName = "view_data",
        icon = shiny::icon("table")
      ),
      shinydashboard::menuItem(
        "View Source Material",
        tabName = "view_source",
        icon = shiny::icon("book")
      ),
      shinydashboard::menuItem(
        "Map",
        tabName = "view_map",
        icon = shiny::icon("map")
      ),
      shinydashboard::menuItem(
        "Taxonomic Search",
        tabName = "taxa_search",
        icon = shiny::icon("fish")
      ),
      shinydashboard::menuItem(
        "Upload Data",
        tabName = "insert_data",
        icon = shiny::icon("plus")
      ),
      shinydashboard::menuItem(
        "Documentation",
        tabName = "docs",
        icon = shiny::icon("print")
      ),
      shinydashboard::menuItem(
        "About",
        tabName = "about",
        icon = shiny::icon("circle-info")
      ),
      shinydashboard::menuItem(
        "Logout",
        tabName = "logout",
        icon = shiny::icon("sign-out-alt")
      )
    ),
    shinyjs::useShinyjs(),
    # Modularized panels
    shiny::conditionalPanel(
      "input.tabs == 'summary_info'",
      summary_sidebar_ui("summary_sidebar")
    ),
    shiny::conditionalPanel(
      "input.tabs == 'scatter_plot'",
      scatter_sidebar_ui("scatter_sidebar")
    ),
    conditionalPanel(
      "input.tabs == 'view_data'",
      raw_data_sidebar_ui("raw_sidebar")
    )
  ),
  # ---- create display panes ----
  shinydashboard::dashboardBody(
    # add  analytics
    shiny::tags$head(
      # ----- add google analytics -----
      shiny::tags$script(src = "gtag.js"),
      # ---- shiny.tictoc ----
      shiny::tags$script(
        src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
      )
    ),
    # CSS for fixed footer
    app_version_head(),
    app_version_label(app_version),
    # tab itimes
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "home", home_tab_ui("home")),
      shinydashboard::tabItem(
        tabName = "how_to_use",
        how_to_ui("how_to_use")
      ),
      shinydashboard::tabItem(
        tabName = "summary_info",
        view_summary_info_ui("summary_info")
      ),
      shinydashboard::tabItem(
        tabName = "scatter_plot",
        view_scatter_plot_ui("scatter_plot")
      ),
      shinydashboard::tabItem(tabName = "view_data", view_data_ui("view_data")),
      shinydashboard::tabItem(
        tabName = "view_source",
        view_source_ui("view_source")
      ),
      shinydashboard::tabItem(tabName = "view_map", view_map_ui("view_map")),
      shinydashboard::tabItem(
        tabName = "taxa_search",
        taxa_search_ui("taxa_search")
      ),
      shinydashboard::tabItem(
        tabName = "insert_data",
        upload_data_ui("insert_data")
      ),
      shinydashboard::tabItem(tabName = "docs", docs_ui("docs")),
      shinydashboard::tabItem(tabName = "about", about_ui("about"))
    )
  )
)

# ------ Main Server -----
server <- function(input, output, session) {
  options(
    shiny.usecairo = FALSE
    # shiny.trace = TRUE
  )
  ram_tracker()
  session$allowReconnect("force")

  # ---- tab authentication -----
  tab_auth <- tab_auth_server(
    input = input,
    output = output,
    session = session,
    credentials = credentials,
    # off = TRUE
  )

  # ---- add in logout tab -----
  logout_server(id = "logout", session)

  # ----- link to docs -----
  shiny::observeEvent(input$go_docs, {
    shinydashboard::updateTabItems(session, "tabs", "docs")
  })

  how_to_server("how_to_use", parent_session = session)

  # ---- get map -----
  view_map_server("view_map", con)
  # ----- summary pane -----
  # ----- summary dropdowns -----
  summary_sidebar_vals <- summary_sidebar_server(
    "summary_sidebar",
    con,
    main_input = input
  )

  # ----- view summary ------
  summary_info <- summary_info_server(
    "summary_info",
    con,
    main_input = input,
    summary_sidebar_vals = summary_sidebar_vals
  )
  # make the download button run

  summary_sidebar_vals$register_summary(summary_info)

  # ---- scatter plot -----
  # ---- scatter plot dropdowns -----
  scatter_sidebar_vals <- scatter_sidebar_server(
    "scatter_sidebar",
    con,
    main_input = input
  )
  # ---- create and view scatter plot -----
  scatter_plot <- scatter_plot_server(
    "scatter_plot",
    con,
    main_input = input,
    scatter_sidebar_vals = scatter_sidebar_vals
  )
  # ----- get tables -----
  raw_sidebar_vals <- raw_data_sidebar_server(
    "raw_sidebar",
    con,
    main_input = input,
    auth_state = tab_auth$auth_state
  )
  view_data <- view_data_server(
    "view_data",
    con,
    main_input = input,
    raw_sidebar_vals = raw_sidebar_vals,
    auth_state = tab_auth$auth_state
  )

  raw_sidebar_vals$register_raw(view_data)

  # ----- view source -----

  view_source_server(
    id = "view_source",
    con = con,
    main_input = input
  )
  # ---- upload data -----
  upload_data_server("insert_data", con, auth_state = tab_auth$auth_state)
  # ---- taxa search -----
  taxa_search_server("taxa_search", con)
  docs_server("docs")

  shiny::observeEvent(input$logout_btn, {
    tab_auth$logout()
  })
}

# render ui and serve together to create dashboard
shiny::shinyApp(ui = ui, server = server)

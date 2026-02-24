home_tab_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = "home",
    h2("Welcome to the Great Lakes Aquatic Tissue Analysis Repository (GLATAR)"),
    p("This toolbox allows you to explore, visualize, and manage
    energy density, proximate composition, stable isotope, thiamine, fatty acid,
    mercury and PCB  data
    for fish and aquatic invertebrates throughout the Great Lakes and
      North America."),
    shiny::br(),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Get Started",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        shiny::p("Use the sidebar to:"),
        tags$ul(
          tags$li("Visualize sampling locations on a map."),
          tags$li("View and download summary statistic tables."),
          tags$li("View visualizations of energy density,
                   proximate composition, stable isotope, thiamine,
                  fatty acid, mercury and PCB data."),
             tags$li("Upload new data into the database (i.e.,
                  if you're a contributing member)."),
             tags$li("View and filter your uploaded raw data (i.e.,
                  if you're a contributing member).")

        )
      ),
      shinydashboard::box(
        title = "About the Database",
        width = 6,
        status = "info",
        solidHeader = TRUE,
        shiny::p("The Great Lakes Aquatic Tissue Analysis Repository (GLATAR)
          contains data on energy density, proximate composition, stable isotopes,
           thiamine, fatty acid, mercury, and PCBs for fish and aquatic invertebrates
          collected across the Great Lakes and North America.
          The data and this repository helps researchers and managers understand
          ecosystem health, bioenergetics, energy transfer, and food web
                 dynamics."),
        tags$ul(
          tags$li("The basic data entry template has fields to collect
                      energy density, proximate composition, and stable isotope data."
                    ),
          tags$li("The advanced data entry template has additional fields to collect
                      thiamine, fatty acid, mercury, and PCB data."
                    ), 
           tags$li(         
            tags$span("Both data entry templates can be found in the "),
            # tags$a(
            #   "clicking here",
            #   href = "data-entry-template/GLATAR_data_entry_template_v15.xlsx",
            #   download = "GLATAR_data_entry_template_v15.xlsx"
            # ),
            actionLink("go_docs", "documentation pane",),
            tags$span(".")
          ),

          tags$li("If you would like to contribute to this database,
          please create a username using your email address on the data upload
                  pane and wait for registration code to be sent to you."),
          tags$li("Any questions can be directed to the GLATAR
                  manager at:", tags$a(href = "mailto:benjamin.hlina@gmail.com",
                                       "benjamin.hlina@gmail.com"))
        )
      )
    ),
    # ---- Interactive database component diagram ----- 
    shiny::fluidRow(
      shinydashboard::box(
        title = "Explore Database Components",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        glatar_diagram_ui()
      )
  )
)
}

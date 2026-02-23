about_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "about",
    h2("About"),

    h3("Funding"),
    p("This database and toolbox is funded by the Great Lakes Fishery Commission
      through grant number 2025_JOH_460024."),

    h3("Contributors"),
    p("The following individuals provided thoughts and design for this project:"),
    p("Timothy B. Johnson, James E. Breck, Steven R. Chipps, David Deslauriers,
    Carolyn J. Foley, Mark W. Kershner, Charles P. Madenjian, Brent W. Metcalfe,
    Michael H.P. O’Brien, Steven A. Pothoven, Jonathan D. Pye, Kate M.J. Storey,
    Mark J. Wuenschel, and Benjamin L. Hlina."),

    h3("Code & Maintenance"),
    p("The database and toolbox are authored and maintained by Benjamin L. Hlina."),
    p("The source code is available on GitHub:",
      tags$a(href = "https://github.com/benjaminhlina/glatar-app",
             target = "_blank",
             icon("github"),
             "View Repository")),
    p("If you would like to contribute to the code that generates the toolbox,
      please fork the repository, create a branch within your fork,
      implement your suggestions, and create a pull request on the main repository for
      the maintainer to review."),

    h3("Citation"),
    p("If you use this toolbox in your research, please cite appropriately using the
        following: [INSERT CITATION].")
  )
}
about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # future logic here
  })
}

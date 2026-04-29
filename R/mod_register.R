# ---- register ui -----

#' Registration User Interface
#'
#' This function provides the user interface
#' to be able to register to GLATAR.
#'
#' @param id the shiny namespace id in this case
#' it is `"register"``.
#'
#' @return a shiny dashboard interface
#'
#' @export

register_ui <- function(id) {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = id,
    shinyjs::useShinyjs(),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        offset = 3,

        #  -----  Card wrapper -----
        register_fields(ns = ns)
      )
    )
  )
}

# ---- register ui -----

#' Registration Server
#'
#' Porvides the server side for the
#' `register_ui()`.
#'
#' @param id the shiny namespace id in this case
#' it is `"register"``.
#'
#' @return a shiny server object that contains
#' registration server.
#'
#' @export

register_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    register_observer(
      input,
      output,
      session
    )
  })
}

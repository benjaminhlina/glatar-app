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
        shiny::tags$div(
          style = paste(
            "background:#fff; border-radius:8px;",
            "box-shadow:0 2px 12px rgba(0,0,0,0.1);",
            "padding:32px 36px; margin-top:24px;"
          ),

          # Header
          shiny::tags$div(
            style = "text-align:center; margin-bottom:24px;",
            shiny::tags$img(
              src = "www/logo/glfc-logo.png",
              width = 80,
              style = "display:block; margin:0 auto 12px auto;"
            ),
            shiny::tags$h3(
              "GLATAR Registration",
              style = "margin:0; color:#2c3e50;"
            ),
            shiny::tags$p(
              "Complete the form below to request access to submit data and view 
              your raw data. You will receive a confirmation email once submitted.",
              style = "color:#7f8c8d; margin-top:8px;"
            )
          ),

          # ---- Status banners (hidden by default, toggled server-side) ----
          shinyjs::hidden(
            shiny::tags$div(
              id = ns("banner_error"),
              class = "alert alert-danger",
              style = "margin-bottom:12px;",
              shiny::icon("triangle-exclamation"),
              " All fields are required."
            )
          ),
          shinyjs::hidden(
            shiny::tags$div(
              id = ns("banner_success"),
              class = "alert alert-success",
              style = "margin-bottom:12px;",
              shiny::icon("circle-check"),
              " Registration submitted! Check your email for confirmation."
            )
          ),

          # ----- Form fields ------
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::textInput(
                ns("reg_first_name"),
                label = shiny::tags$span(shiny::icon("user"), " First Name"),
                placeholder = "Jane",
                width = "100%"
              )
            ),
            shiny::column(
              6,
              shiny::textInput(
                ns("reg_last_name"),
                label = shiny::tags$span(shiny::icon("user"), " Last Name"),
                placeholder = "Doe",
                width = "100%"
              )
            )
          ),
          shiny::textInput(
            ns("reg_affiliation"),
            label = shiny::tags$span(shiny::icon("building"), " Affiliation"),
            placeholder = "University / Agency / Organization",
            width = "100%"
          ),
          shiny::textInput(
            ns("reg_email"),
            label = shiny::tags$span(shiny::icon("envelope"), " Email"),
            placeholder = "user.name@example.com",
            width = "100%"
          ),

          # ----- Footer buttons ------

          shiny::actionButton(
            ns("reg_submit"),
            "Submit",
            class = "btn btn-success",
            icon = shiny::icon("paper-plane")
          )
        )
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

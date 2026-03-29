# ----- secure ui -----
secure_ui <- function(ui, off = FALSE) {
  if (isTRUE(off)) {
    return(ui)
  } else {
    sec_ui <- shinymanager::secure_app(
      ui,
      enable_admin = FALSE,
      # Bootstrap flatly, cerulean, cosmo,
      theme = "flatly",
      language = "en",
      timeout = 15.0,
      fab_position = "none",
      # Customize the login page appearance
      tags_top = shiny::tags$div(
        shiny::tags$h2(
          "Great Lakes Aquatic Tissue Analysis Repository (GLATAR)",
          style = "text-align: center; color: #2c3e50; margin-bottom: 20px;"
        ),
        shiny::tags$img(
          src = "logo/glfc-logo.png",
          width = 150,
          style = "display: block; margin: 0 auto 20px auto;"
        )
      ),

      tags_bottom = shiny::tags$div(
        shiny::tags$p(
          "Please login to access the application",
          style = "text-align: center; color: #7f8c8d;"
        )
      ),

      # Customize button colors and text
      choose_language = FALSE,
      lan = list(
        en = list(
          title = "Please Authenticate",
          user = "Email Address",
          password = "Password"
        )
      )
    )
    return(sec_ui)
  }
}

# ----- secure server -----
authorize_server <- function(credentials, off = FALSE) {
  if (isTRUE(off)) {
    NULL
  } else {
    auth_server <- shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(credentials),
    )
    return(auth_server)
  }
}

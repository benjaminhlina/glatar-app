# ---- protected tabs -----

protected_tabs <- function() {
  pt <- c("view_data", "insert_data")
  return(pt)
}

# ----- tab login modal -----
tab_login_modal <- function(failed = FALSE) {
  shiny::modalDialog(
    title = shiny::tags$div(
      shiny::tags$img(
        src = "www/logo/glfc-logo.png",
        width = 80,
        style = "display:block; margin: 0 auto 12px auto;"
      ),
      shiny::tags$h4(
        "Login Required to Access",
        style = "text-align:center; margin:0; color:#2c3e50;"
      )
    ),
    # Show error banner only after a failed attempt
    if (failed) {
      shiny::tags$div(
        class = "alert alert-danger",
        style = "margin-bottom:12px;",
        shiny::icon("triangle-exclamation"),
        " Invalid email or password."
      )
    },
    shiny::tags$p(
      "This section requires authentication.",
      style = "color:#7f8c8d; text-align:center; margin-bottom:16px;"
    ),
    shiny::textInput(
      "tab_login_user",
      label = shiny::tags$span(shiny::icon("envelope"), " Email"),
      placeholder = "user.name@gmail.com"
    ),
    shiny::passwordInput(
      "tab_login_pass",
      label = shiny::tags$span(shiny::icon("lock"), " Password")
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        "tab_login_submit",
        "Login",
        class = "btn btn-primary",
        icon = shiny::icon("right-to-bracket")
      )
    ),
    easyClose = FALSE,
    size = "s"
  )
}

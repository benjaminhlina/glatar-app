# ----- tab login modal -----
#' Tab Login Modal
#'
#' This function produces the login modal when clicking on the
#' protected tabs `raw_data` and `upload_data`.
#'
#' @param failed a logical value that when supplied
#' produces a warning message stating invalid email or
#' password. Default is `FALSE`
#'
#'
#' @export

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
      placeholder = "user.name@example.com"
    ),
    shiny::passwordInput(
      "tab_login_pass",
      label = shiny::tags$span(shiny::icon("lock"), " Password")
    ),
    shiny::tags$p(
      "Don't have an account?",
      shiny::actionLink("go_to_register", "Request access here."),
      style = "text-align:center; margin-top:4px; color:#7f8c8d;"
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

# ---- tab register modal ------
#' Tab Register Modal
#'
#' This function produces the registration modal on the login modal
#' when the registration button is slected
#'
#' @param failed a logical value that when supplied
#' prodocues warning when not all fields are filled out. Default is `FALSE`.
#' @param success a logical value that when supplied
#' prodocues a success messsage. Default is `FALSE`.
#'
#'
#' @export

tab_register_modal <- function(failed = FALSE, success = FALSE) {
  shiny::modalDialog(
    title = shiny::tags$div(
      shiny::tags$img(
        src = "www/logo/glfc-logo.png",
        width = 80,
        style = "display:block; margin: 0 auto 12px auto;"
      ),
      shiny::tags$h4(
        "GLATAR Registration",
        style = "text-align:center; margin:0; color:#2c3e50;"
      )
    ),

    # ── Status banners ──────────────────────────────────────────────────
    if (failed) {
      shiny::tags$div(
        class = "alert alert-danger",
        style = "margin-bottom:12px;",
        shiny::icon("triangle-exclamation"),
        " All fields are required. Please fill in every field."
      )
    },
    if (success) {
      shiny::tags$div(
        class = "alert alert-success",
        style = "margin-bottom:12px;",
        shiny::icon("circle-check"),
        " Registration submitted! Check your email for confirmation."
      )
    },

    shiny::tags$p(
      "Complete the form below to request access to submit data and view
       your raw data. You will receive a confirmation email once submitted.",
      style = "color:#7f8c8d; text-align:center; margin-bottom:16px;"
    ),

    # ----- Form fields ------
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::textInput(
          "reg_first_name",
          label = shiny::tags$span(shiny::icon("user"), " First Name"),
          placeholder = "Jane"
        )
      ),
      shiny::column(
        6,
        shiny::textInput(
          "reg_last_name",
          label = shiny::tags$span(shiny::icon("user"), " Last Name"),
          placeholder = "Doe"
        )
      )
    ),
    shiny::textInput(
      "reg_affiliation",
      label = shiny::tags$span(shiny::icon("building"), " Affiliation"),
      placeholder = "University / Agency / Organization"
    ),
    shiny::textInput(
      "reg_email",
      label = shiny::tags$span(shiny::icon("envelope"), " Email"),
      placeholder = "user.name@example.com"
    ),

    # ----- Footer ----
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(
        "reg_back",
        "Back to Login",
        class = "btn btn-default",
        icon = shiny::icon("arrow-left")
      ),
      shiny::actionButton(
        "reg_submit",
        "Submit",
        class = "btn btn-success",
        icon = shiny::icon("paper-plane")
      )
    ),
    easyClose = FALSE,
    size = "s"
  )
}

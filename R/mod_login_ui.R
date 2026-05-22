# ----- tab server ------
#' Authorization Server
#'
#' Provides the authorization server for the login modal that displays
#' on two locked tabs, uploading and user specific data.
#'
#' @param input a shiny input object.
#' @param output a shiny output object.
#' @param session a shiny session.
#' @param sidebar_id a `vector` containing the sidebar ids to allow
#' the login to display.
#' @param valid_users_emails a `vector` containing the valid user email
#' @param off a `logical` value that turns on and off the authorization
#' server. Default is `FALSE`. This switch is for development only.
#'
#' @details `tab_auth_server()` a shiny modal
#'
#' @name login_modal_server
#' @export

tab_auth_server <- function(
  input,
  output,
  session,
  valid_users_emails,
  sidebar_id = "tabs",
  off = FALSE
) {
  # ---- this alllows me to turn things on and off
  if (isTRUE(off)) {
    return(list(
      auth_state = shiny::reactiveVal(TRUE), # always authenticated
      logout = function() invisible(NULL) # no-op
    ))
  }
  auth_state <- shiny::reactiveVal(FALSE)
  pending_tab <- shiny::reactiveVal(NULL)
  login_failed <- shiny::reactiveVal(FALSE)
  password <- shiny::reactiveVal(FALSE)
  username <- shiny::reactiveVal(FALSE)

  # ------ Intercept tab navigation ------
  shiny::observeEvent(
    input[[sidebar_id]],
    {
      tab <- input[[sidebar_id]]

      if (tab %in% protected_tabs() && !auth_state()) {
        pending_tab(tab)
        login_failed(FALSE)
        shiny::showModal(tab_login_modal())
        shinydashboard::updateTabItems(session, sidebar_id, "home")
      }
    },
    ignoreInit = TRUE
  )

  # ------ Handle login submission ------
  shiny::observeEvent(input$tab_login_submit, {
    valid <- check_tab_credentials(
      input$tab_login_user,
      valid_users_emails
    )

    if (valid) {
      auth_state(TRUE)
      login_failed(FALSE)
      dest <- pending_tab()
      # Create username and password using your functions
      username <- make_pg_username(input$tab_login_user)
      password <- make_temp_password(input$tab_login_user)
      username(username)
      password(password)
      pending_tab(NULL)

      shiny::removeModal()

      shinyjs::delay(150, {
        if (!is.null(dest)) {
          shinydashboard::updateTabItems(session, sidebar_id, dest)
        }
      })
    } else {
      login_failed(TRUE)
      shiny::showModal(tab_login_modal(failed = TRUE))
      shiny::updateTextInput(session, "tab_login_pass", value = "")
    }
  })

  # ----- register modal -----
  register_observer(input, output, session, modal = TRUE)

  # ------ Handle cancel ------
  shiny::observeEvent(input$tab_login_cancel, {
    shiny::removeModal()
    shinydashboard::updateTabItems(session, sidebar_id, "home")
  })

  # ------ Logout helper ------
  logout <- function() {
    auth_state(FALSE)
    shinydashboard::updateTabItems(session, sidebar_id, "home")
    shiny::showNotification(
      "You have been logged out.",
      type = "message",
      duration = 3
    )
  }

  list(
    username = username,
    auth_state = auth_state,
    password = password,
    logout = logout
  )
}
# ----- logout -----
#' @param id the shiny namespace id name (i.e., `"logout"`).
#' @param parent_session a shiny server session.
#'
#' @details `logout_server()` a modal that asks the user if they really want to logout.
#' This prevents accidental logouts.
#'
#' @name login_modal_server
#' @export

logout_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(parent_session$input$tabs, {
      if (parent_session$input$tabs == "logout") {
        shinydashboard::updateTabItems(session, "tabs", "home")
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Logout",
          "Are you sure you want to log out?",
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              session$ns("confirm_logout"),
              "Logout",
              class = "btn btn-danger"
            )
          )
        ))
      }
    })

    shiny::observeEvent(input$confirm_logout, {
      shiny::removeModal()
      session$reload()
    })
  })
}

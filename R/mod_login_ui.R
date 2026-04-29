# ----- tab server ------

tab_auth_server <- function(
  input,
  output,
  session,
  credentials,
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
      input$tab_login_pass,
      credentials
    )

    if (valid) {
      auth_state(TRUE)
      login_failed(FALSE)
      dest <- pending_tab()
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
  # register_observer(input, output, session)
  shiny::observeEvent(input$go_to_register, {
    shiny::showModal(tab_register_modal())
  })

  shiny::observeEvent(input$reg_back, {
    shiny::showModal(tab_login_modal())
  })

  # ----- back to login when email has successfully ben sumbitted ----
  shiny::observeEvent(input$reg_back, {
    shiny::showModal(tab_login_modal())
  })

  # ---- Handle registration submission ----
  shiny::observeEvent(input$reg_submit, {
    first <- trimws(input$reg_first_name)
    last <- trimws(input$reg_last_name)
    affil <- trimws(input$reg_affiliation)
    email <- trimws(input$reg_email)

    fields_missing <- any(nchar(c(first, last, affil, email)) == 0)
    email_invalid <- !grepl("^[^@]+@[^@]+\\.[^@]+$", email)

    if (fields_missing || email_invalid) {
      shiny::showModal(tab_register_modal(failed = TRUE))
      return()
    }

    tryCatch(
      {
        send_email(
          to_user = email,
          email_text = reg_confirmation_email_body(first, last, affil, email)
        )
        shiny::showModal(tab_register_modal(success = TRUE))
      },
      error = function(e) {
        shiny::showNotification(
          paste("Email could not be sent:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        # Still show success — registration was even if email fails
        shiny::showModal(tab_register_modal(success = FALSE))
      }
    )
  })
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
    auth_state = auth_state,
    logout = logout
  )
}

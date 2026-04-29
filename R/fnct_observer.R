# ----- filter observserer ----

#' Exclude all observer
#'
#' This function uses an `observerEvent()` to adjust dropdown
#' menus so that when other selections beside `"All"` is selected
#' it will not allow `"All"` to be selected until the entire box
#' is cleared.
#'
#'
#' @param input a server input value.
#' @param session a shiny session.
#' @param id the dropdown object to apply the observer
#'
#' @return an Event that excludes `"All"``
#' @export

exclusive_all_observer <- function(input, session, id) {
  shiny::observeEvent(
    input[[id]],
    {
      sel <- input[[id]]
      if ("All" %in% sel && length(sel) > 1) {
        shiny::updateSelectInput(
          session,
          id,
          selected = setdiff(sel, "All")
        )
      }
    },
    ignoreInit = TRUE
  )
}

# ----- register observer ------
#' Registration Observer
#'
#' This function uses an `observerEvent()` to provide
#' server side events either in the login modal or
#' the registration tab.
#'
#'
#' @param input a server input value.
#' @param output a server output value.
#' @param session a shiny session.
#' @param modal a logical that allows for this function to
#' be used in modal mode. Defaults to `FALSE`.
#'
#' @return an event that sends an email with registeration information.
#' @export

register_observer <- function(input, output, session, modal = FALSE) {
  # ----- back to login when email has successfully been sumbitted ----

  if (isTRUE(modal)) {
    shiny::observeEvent(input$go_to_register, {
      shiny::showModal(tab_register_modal())
    })

    # ----- back to login when email has successfully ben sumbitted ----
    shiny::observeEvent(input$reg_back, {
      shiny::showModal(tab_login_modal())
    })
  }

  shiny::observeEvent(input$reg_submit, {
    shinyjs::disable("reg_submit")
    on.exit(shinyjs::enable("reg_submit"), add = TRUE)

    if (isFALSE(modal)) {
      shinyjs::hide("banner_error")
      shinyjs::hide("banner_success")
    }
    # ---- Handle registration submission ----
    first <- trimws(input$reg_first_name)
    last <- trimws(input$reg_last_name)
    affil <- trimws(input$reg_affiliation)
    email <- trimws(input$reg_email)

    fields_missing <- any(nchar(c(first, last, affil, email)) == 0)
    email_invalid <- !grepl("^[^@]+@[^@]+\\.[^@]+$", email)

    if (fields_missing || email_invalid) {
      if (isTRUE(modal)) {
        shiny::showModal(tab_register_modal(failed = TRUE))
      } else {
        shinyjs::show("banner_error")
      }
      return()
    }

    tryCatch(
      {
        send_email(
          to_user = email,
          email_text = reg_confirmation_email_body(first, last, affil, email)
        )
        if (isTRUE(modal)) {
          shiny::showModal(tab_register_modal(success = TRUE))
        } else {
          shinyjs::show("banner_success")
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Email could not be sent:", conditionMessage(e)),
          type = "error",
          duration = 8
        )
      }
    )
  })
}

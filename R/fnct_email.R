# ---- email body html ------
#' Submission Email Body
#'
#' This function drafts the text body the submission email that
#' will be sent once the data has been successfully uploaded to
#' the database
#'
#' @param submission_id a `vector` containing the submission id
#' which will be used in the subject line and body of the emaul
#' @param submission_results an object returned by `upload_to_db`
#' that has the name of the table, the number of rows that were submitted, and
#' the submission id so this can be dynamically added to the the email text.
#' @return HTML of the email structure to be sent.
#'
#' @export

submission_email_body <- function(submission_id, submission_results) {
  results_lines <- purrr::imap_chr(submission_results, function(res, tbl_name) {
    glue::glue(
      "<li><strong>{tbl_name}</strong>: {res$rows_submitted} row(s) submitted</li>"
    )
  })
  results_block <- paste(results_lines, collapse = "\n")

  email_structure <- list(
    subject = paste("GLATAR Submission Received:", submission_id),
    email_body = glue::glue(
      '
    <h3>Submission Received</h3>
    <p>Thank you for your submission.</p>
    <p><strong>Submission ID:</strong> <code>{submission_id}</code></p>
    <p>Please keep this ID for reference.</p>
    <hr>
    <strong>Submission Summary:</strong>
    <ul>
      {results_block}
    </ul>
    <hr>
    <p>If you have questions, please contact the GLATAR manager at
    <a href="mailto:benjamin.hlina@gmail.com">benjamin.hlina@gmail.com</a>.</p>
    <p>— GLATAR Team</p>

    <p><em>Please do not reply to this email as it will not be received. 
    If you have any concerns, please email <a href="mailto:benjamin.hlina@gmail.com">benjamin.hlina@gmail.com</a> with 
    your submission ID in the subject line.</em></p>
  '
    )
  )
  return(email_structure)
}


# ----- email send ------
#' Send Email
#'
#' This function sends an email using `httr` using the
#' starndard HTTPS port of 443. Due to server firewall restrictions
#' we cannot use STMP protocols to send emails from the site but
#' can use HTTPS and resend.
#'
#' @param to_user a valid email address
#' @param email_text an object containing HTML for the subject
#' line and the body of the email.
#'
#' @export

send_email <- function(
  to_user,
  email_text
) {
  email_sent <- httr2::request("https://api.resend.com/emails") |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv("RESEND_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      data = list(
        from = "noreply@glatar.org",
        to = to_user,
        cc = "benjamin.hlina@gmail.com",
        subject = email_text$subject,
        html = email_text$email_body
      )
    ) |>
    httr2::req_perform()

  return(email_sent)
}

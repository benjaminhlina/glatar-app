# ---- registration email -----
# Registration Confirmation Email Body
#'
#' This function drafts the text body the registration confirmation
#' email body.
#'
#' @param first a `vector` containing the first name of the submitter.
#' @param last a `vector` containing the last name of the submitter.
#' @param affil a `vector` containing the affiliation of the submitter.
#' @param email a `vector` containing the email of the submitter.
#'
#' @return HTML of the email structure to be sent for the registration email.
#'
#' @export

reg_confirmation_email_body <- function(first, last, affil, email) {
  list(
    subject = "GLATAR Portal - Registration Received",
    email_body = glue::glue(
      '
      <div style="font-family: Arial, sans-serif; max-width: 600px; margin: auto; padding: 24px;">

        <img src="https://glatar.org/www/logo/glfc-logo.png"
             width="80" style="display:block; margin: 0 auto 16px auto;" />

        <h2 style="text-align:center; color:#2c3e50;">Registration Received</h2>

        <p>Hi <strong>{first}</strong>,</p>

        <p>Thank you for requesting access to the GLFC Portal. We have received
        your registration and will be in touch shortly.</p>

        <table style="width:100%; border-collapse:collapse; margin: 16px 0;">
          <tr style="background:#f2f2f2;">
            <td style="padding:8px 12px; font-weight:bold; width:40%;">Name</td>
            <td style="padding:8px 12px;">{first} {last}</td>
          </tr>
          <tr>
            <td style="padding:8px 12px; font-weight:bold;">Affiliation</td>
            <td style="padding:8px 12px;">{affil}</td>
          </tr>
          <tr style="background:#f2f2f2;">
            <td style="padding:8px 12px; font-weight:bold;">Email</td>
            <td style="padding:8px 12px;">{email}</td>
          </tr>
        </table>

        <p style="color:#7f8c8d; font-size:0.9em;">
          If you did not submit this request, please ignore this email.
        <em>Please do not reply to this email as it will not be received.</em></p>

        <hr style="border:none; border-top:1px solid #eee; margin:24px 0;" />
        <p style="text-align:center; color:#aaa; font-size:0.85em;">
          -GLATAR Team
        </p>

      </div>
    '
    )
  )
}
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
    <p> -GLATAR Team</p>

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
#' @param attachment_path path to attachement
#' @export

send_email <- function(
  to_user,
  email_text,
  attachment_path = NULL
) {
  body <- list(
    from = "noreply@glatar.org",
    to = to_user,
    cc = "benjamin.hlina@gmail.com",
    subject = email_text$subject,
    html = email_text$email_body
  )

  if (!is.null(attachment_path)) {
    raw_bytes <- readBin(
      attachment_path,
      "raw",
      file.info(attachment_path)$size
    )
    b64_content <- jsonlite::base64_enc(raw_bytes)

    body$attachments <- list(
      list(
        filename = basename(attachment_path),
        content = b64_content
      )
    )
  }

  email_sent <- httr2::request("https://api.resend.com/emails") |>
    httr2::req_headers(
      Authorization = paste("Bearer", Sys.getenv("RESEND_API_KEY")),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      data = body
    ) |>
    httr2::req_perform()

  return(email_sent)
}
# ---- email body html ------
#' Taxa Submission Email Body
#'
#' This function drafts the text body the taxa submission email that
#' will be sent once the submit to manager has been hit.
#' @param n_rows the number of rows being submitted
#' @param submitted_by the email of the submitter
#' @return HTML of the email structure to be sent.
#'
#' @export

taxa_submission_email_body <- function(n_rows, submitted_by) {
  email_structure <- list(
    subject = paste("GLATAR Taxa Submission -", Sys.Date()),
    email_body = glue::glue(
      '
      <h3>New Taxonomy Submission</h3>
      <p>A new taxonomy submission has been sent for review.</p>
      <hr>
      <strong>Submission Details:</strong>
      <ul>
        <li><strong>Submitted by:</strong> {submitted_by}</li>
        <li><strong>Date:</strong> {Sys.Date()}</li>
        <li><strong>Rows submitted:</strong> {n_rows}</li>
      </ul>
      <hr>
      <p>The submitted taxa are attached as an Excel file for your review.</p>
      <p>If you have questions, please contact the submitter directly or reply to
      <a href="mailto:benjamin.hlina@gmail.com">benjamin.hlina@gmail.com</a>.</p>
      <p>-GLATAR Team</p>
      <p><em>Please do not reply to this email as it will not be received.</em></p>
      '
    )
  )
  return(email_structure)
}

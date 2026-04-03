# ----- email body ------

email_body <- function(submission_id, submission_results) {
  results_lines <- purrr::imap_chr(submission_results, function(res, tbl_name) {
    glue::glue("- **{tbl_name}**: {res$rows_submitted} row(s) submitted")
  })
  results_block <- paste(results_lines, collapse = "\n")

  email <- blastula::compose_email(
    body = blastula::md(glue::glue(
      "
### Submission Received

Thank you for your submission.

**Submission ID:** `{submission_id}`
  
Please keep this ID for reference.
  
---

**Submission Summary:**

{results_block}

---

If you have questions, please contact the GLATAR manager at
benjamin.hlina@gmail.com or tim.johnson@ontario.ca.

— GLATAR Team
"
    ))
  )
}


send_submission_email <- function(to_user, submission_id, submission_results) {
  email <- email_body(submission_id, submission_results)

  blastula::smtp_send(
    email = email,
    to = c(to_user, "benjamin.hlina@gmail.com"),
    from = "noreply@glatar.org",
    subject = paste("GLATAR Submission Received:", submission_id),
    credentials = blastula::creds_envvar(
      user = "noreply@glatar.org",
      pass_envvar = "SMTP_PASSWORD",
      host = "mail.hover.com",
      port = 587,
      use_ssl = FALSE
    )
  )
}

send_submission_email <- function(to_user, submission_id) {
  email <- compose_email(
    body = md(glue::glue(
      "
### ✅ Submission Received

Thank you for your submission.

**Submission ID:** `{submission_id}`

Please keep this ID for reference.

If you have questions, please contact the database manager at
benjamin.hlina@gmail.com to this email.

— GLATAR Database Team
"
    ))
  )

  smtp_send(
    email,
    to = c(to_user, "your_email@domain.com"),
    from = "your_email@domain.com",
    subject = paste("Submission Received:", submission_id),
    credentials = creds
  )
}

send_submission_email <- function(to_user, submission_id) {
  email <- blastula::compose_email(
    body = blastula::md(glue::glue(
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

  blastula::smtp_send(
    email,
    to = c(to_user, "benjamin.hlinal@gmail.com"),
    from = "noreply@glatar.org",
    subject = paste("Submission Received:", submission_id),
    blastula::creds_envvar(
      user = "noreply@glatar.org",
      pass_envvar = "SMTP_PASSWORD",
      host = "mail.hover.com",
      port = 587,
      use_ssl = FALSE
    )
  )
}

# Send Email

This function sends an email using `httr` using the starndard HTTPS port
of 443. Due to server firewall restrictions we cannot use STMP protocols
to send emails from the site but can use HTTPS and resend.

## Usage

``` r
send_email(to_user, email_text)
```

## Arguments

- to_user:

  a valid email address

- email_text:

  an object containing HTML for the subject line and the body of the
  email.

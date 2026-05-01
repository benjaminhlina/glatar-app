# Registration Confirmation Email Body

This function drafts the text body the registration confirmation email
body.

## Usage

``` r
reg_confirmation_email_body(first, last, affil, email)
```

## Arguments

- first:

  a `vector` containing the first name of the submitter.

- last:

  a `vector` containing the last name of the submitter.

- affil:

  a `vector` containing the affiliation of the submitter.

- email:

  a `vector` containing the email of the submitter.

## Value

HTML of the email structure to be sent for the registration email.

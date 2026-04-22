# Submission Email Body

This function drafts the text body the submission email that will be
sent once the data has been successfully uploaded to the database

## Usage

``` r
submission_email_body(submission_id, submission_results)
```

## Arguments

- submission_id:

  a `vector` containing the submission id which will be used in the
  subject line and body of the emaul

- submission_results:

  an object returned by `upload_to_db` that has the name of the table,
  the number of rows that were submitted, and the submission id so this
  can be dynamically added to the the email text.

## Value

HTML of the email structure to be sent.

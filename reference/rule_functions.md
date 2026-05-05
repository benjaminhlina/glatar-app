# Valdiation rules

These functions supply different validation rules to be used with
`{validator}`. They include things like a rule regarind column names,
email styles, or whether the field can be blank.

## Usage

``` r
rule_blank(required_fields)

rule_column_names(required_fields)

rule_email(submission_email)

rule_len(required_fields)

rule_match(exprs, field)

rule_na(required_fields)
```

## Arguments

- required_fields:

  is a vector containing the fields to apply the validation rule to.

- submission_email:

  the email of the submitter.

- exprs:

  is the validation rule expression to be checked

- field:

  is the column of interest, usually this is `col_name` or `issue`/.

## Value

A rule to be evaluated by `{validator}`.

## Details

`rule_blank()` checks whether a feild is blank.

`rule_column_names()` checks the column names.

`rule_email()` checks if the submission email is in a valid format.

`rule_len()` checks if the length of the column equals 1.

`rule_match()` checks if the valid exprssion matches a given rule.

`rule_na()` checks if a column is `NA`.

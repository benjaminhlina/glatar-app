# --- not blank rules -----
#' Valdiation rules
#'
#' These functions supply different validation rules to be used with
#' `{validator}`. They include things like a rule regarind column names,
#' email styles, or whether the field can be blank.
#'
#' @param required_fields is a vector containing the fields to apply the validation
#' rule to.
#'
#' @details `rule_blank()` checks whether a feild is blank.
#'
#' @return A rule to be evaluated by `{validator}`.
#'
#' @name rule_functions
#' @export
rule_blank <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(nchar(trimws(COL)) > 0, list(COL = as.name(x)))
    }),
    paste0("not_blank_", required_fields)
  )
  return(rule)
}

# --- chec column names -----

#' @param required_fields is a vector containing the fields to apply the validation
#' rule to.
#'
#' @details `rule_column_names()` checks the column names.
#'
#'
#' @name rule_functions
#' @export
rule_column_names <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(COL %in% colnames(.), list(COL = x))
    }),
    paste0("not_field_", required_fields)
  )
  return(rule)
}
# --- special email rule ---
#' @param submission_email the email of the submitter.
#'
#' @details `rule_email()` checks if the submission email is in a valid format.
#'
#'
#' @name rule_functions
#' @export

rule_email <- function(submission_email) {
  rule <- list(
    valid_email = substitute(
      grepl("@", submission_email) & grepl("\\.", submission_email)
    )
  )
  return(rule)
}

# --- length == 1 rules
#' @param required_fields is a vector containing the fields to apply the validation
#' rule to.
#'
#' @details `rule_len()` checks if the length of the column equals 1.
#'
#'
#' @name rule_functions
#' @export
rule_len <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(length(COL) == 1, list(COL = as.name(x)))
    }),
    paste0("len_", required_fields)
  )
  return(rule)
}

# ----- rule match -----
#' @param exprs is the validation rule expression to be checked
#' @param field is the column of interest, usually this is `col_name`
#' or `issue`/.
#'
#' @details `rule_match()` checks if the valid exprssion matches a given rule.
#'
#'
#' @name rule_functions
#' @export
rule_match <- function(exprs, field) {
  matched_rule <- vapply(
    exprs,
    function(expr) {
      i <- which(vapply(rule_map$pat, grepl, logical(1), x = expr))
      if (length(i)) rule_map[[field]][i[1]] else NA_character_
    },
    character(1)
  )
  return(matched_rule)
}

# --- not NA rules -----
#' @param required_fields is a vector containing the fields to apply the validation
#' rule to.
#'
#'
#' @details `rule_na()` checks if a column is `NA`.
#'
#'
#' @name rule_functions
#' @export
rule_na <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(!is.na(COL), list(COL = as.name(x)))
    }),
    paste0("not_na_", required_fields)
  )
  return(rule)
}

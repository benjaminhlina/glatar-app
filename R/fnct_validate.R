# ---- get valid taxonomy ------
#' Validator functions
#'
#' These functions work within their own enviornment. They take
#' in a `data.frame` and a set of `rules` either supplied by
#' `rule_*` functions or within the function itself to check. Once
#' both the `data.frame` and `rules` are set these functions use
#' `confront()` from `{validate}` to confront
#' the `data.frame` with rules.
#'
#' @param df a `data.frame` that requires validation.
#'
#' @details
#'  `the_golden_lance()` is named after Galtar's most prized possesion, his golden lance, which
#' he uses to fight evil. This function validates incoming data to be uploaded to `tbl_samples`.
#' Let `the_golden_lance()` have its say.
#'
#' @return Returns a `validate` object.
#'
#'
#' @name validation_functions
#' @export

# -----the_golden_lance is validate_tbl_samples ------
# galtar and the golden lance
the_golden_lance <- function(df) {
  required_fields <- sample_required_fields()

  # Which optional columns were actually submitted
  # present_optional <- optional_fields[optional_fields %in% colnames(df)]

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
    # } else if {
    #   # Build rules for required fields + any optional fields that are present
    #   all_fields_to_validate <- c(required_fields, present_optional)

    #   rules <- do.call(
    #     validate::validator,
    #     c(rule_column_names(all_fields_to_validate))
    #   )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    not_null_fields <- not_null_fields()

    numeric_fields <- numeric_fields()

    flag_fields <- flag_fields()

    rule_strings <- c(
      paste0("!is.na(", not_null_fields, ")"),
      paste0(flag_fields, " == TRUE | is.na(", flag_fields, ")"),
      paste0(
        "grepl('^-?[0-9]*\\\\.?[0-9]+$', as.character(",
        numeric_fields,
        ")) | is.na(",
        numeric_fields,
        ")"
      )
    )

    rules_to_check <- data.frame(
      name = c(
        paste0("not_null__", not_null_fields),
        paste0("flag__", flag_fields),
        paste0("numeric__", numeric_fields)
      ),
      rule = rule_strings
    )
    rules <- validate::validator(
      .data = rules_to_check
    )
  }

  out <- validate::confront(df, rules)

  return(out)
}

# ----- validate source ------
#' @param df a `data.frame` that requires validation.
#'
#' @details
#'  `validate_tbl_sources()` validates incoming data to be uploaded to `tbl_sources`.
#'
#'
#' @name validation_functions
#' @export
validate_tbl_sources <- function(df) {
  required_fields <- sources_required_fields()

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    rules <- validate::validator(
      # ---- required fields
      !is.na(source_id),
      !is.na(publication_type),

      !is.na(author_names),
      !is.na(publication_year),
      !is.na(email),
      # !is.na(title),

      publication_type %in%
        c("Journal Article", "Book", "Book Section", "Report", "Unpublished")
    )
  }

  out <- validate::confront(df, rules)

  return(out)
}
# ----- validate_tbl_submission ------
#' @param df a `data.frame` that requires validation.
#'
#' @details
#'  `validate_tbl_submission()` validates incoming data to be uploaded to `tbl_submission`.
#'
#'
#' @name validation_functions
#' @export
validate_tbl_submission <- function(df) {
  required_fields <- submission_required_fields()
  submission_email <- df$submission_email

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    rules <- do.call(
      validate::validator,
      c(
        rule_len(required_fields),
        rule_na(required_fields),
        rule_blank(required_fields),
        rule_email(submission_email)
      )
    )
  }
  out <- validate::confront(df, rules)

  return(out)
}

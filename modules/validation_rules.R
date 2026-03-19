# --- not blank rules
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
rule_column_names <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(COL %in% colnames(.), list(COL = x))
    }),
    paste0("not_field_", required_fields)
  )
  return(rule)
}
# --- special email rule
rule_email <- list(
  valid_email = substitute(
    grepl("@", submission_email) & grepl("\\.", submission_email)
  )
)

# --- length == 1 rules
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

# --- not NA rules
rule_na <- function(required_fields) {
  rule <- stats::setNames(
    lapply(required_fields, function(x) {
      substitute(!is.na(COL), list(COL = as.name(x)))
    }),
    paste0("not_na_", required_fields)
  )
  return(rule)
}

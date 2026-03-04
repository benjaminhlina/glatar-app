# ---- make validation message ----
make_validation_message <- function(column, message) {
  rlang::new_formula(
    lhs = rlang::expr(grepl(!!paste0("\\.", column), expression)),
    rhs = message
  )
}

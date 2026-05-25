# ----- make pg username  -----

#' Make functions
#'
#' These functions make information that is to be used during the
#' login processes
#'
#' @param email a valid email
#'
#' @details
#' `make_pg_username()` returns a username that matches with the
#' database user name.
#'
#' @name make_functions
#' @export

make_pg_username <- function(email) {
  check_email(email, "email")
  user_name <- paste0(
    "usr_",
    gsub("[^a-z0-9]", "_", tolower(sub("@.*", "", email)))
  )
  return(user_name)
}

# ----- make temp pw -----
#' @param email a valid email
#'
#' @details
#' `make_temp_password()` returns a temporay password
#'
#' @name make_functions
#' @export
make_temp_password <- function(email) {
  check_email(email, "email")
  paste0("Chng_", toupper(substr(gsub("[^a-zA-Z0-9]", "", email), 1, 8)), "_1!")
}

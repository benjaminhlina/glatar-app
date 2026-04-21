#' A `data.frame` containing naming conventions
#'
#'
#' @format `data.frame` containing 87 rows and 2 variables
#'  \describe{
#'    \item{raw_names}{name database columns}
#'    \item{nice_names}{nice names to transform them into}
#' }
"naming_conventions"

#' A `data.frame` containing validation rules and names
#'
#' @format `data.frame` containing 23 rows and 3 variables
#'  \describe{
#'    \item{pat}{validation pattern}
#'    \item{col}{column name that is being validated}
#'    \item{issue}{what is the issue that the column has}
#' }
"rule_map"

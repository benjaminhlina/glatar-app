# ----- message dropdowns -----

#' Message functions
#'
#' These functions use `{cli}` to display messages in other functions
#' when different opperations occur. They are helpfui in seeing
#' how the suer interacts with the app as well as diagnosing
#' isssues that may arrise.
#'
#' @param waterbody_choices a vector of water body choices
#' @param common_name_choices a vector of common name choices
#' @param grouping_choices a vector of grouping variable choices
#' @param numeric_choices a vector of numerical choices
#'
#' @details
#' `msg_dropdowns()` returns info on the drop downs selected.
#'
#' @name msg_functions
#'
#' @export

msg_dropdowns <- function(
  waterbody_choices,
  common_name_choices,
  grouping_choices,
  numeric_choices
) {
  cli::cli_alert_success("Updating dropdowns")
  cli::cli_ul(c(
    "Waterbody unique values: {length(waterbody_choices)}",
    "Species unique values: {length(common_name_choices)}",
    "Grouping choices: {paste(grouping_choices, collapse = ', ')}",
    "Numeric choices: {paste(numeric_choices, collapse = ', ')}"
  ))
}

# ---- msg_hist_ui -------

#' @param df the `data.frame` that is beinb supplied to the histogram
#' @param var the `x` variable that is being supplied to the histogram
#' @param type_val the type of value supplied
#' @param col the column name of the column in
#'  dataframe that is of interest
#'
#'
#' @details
#' `msg_hist_ui()` provides info on the histogogram
#'
#' @name msg_functions
#'
#' @export
# ---- check lenght_ui -----
msg_hist_ui <- function(
  df,
  var,
  type_val,
  col
) {
  cli::cli_alert_info("UI var: {var}")
  cli::cli_alert_info("Mapped type_val: {type_val}")
  cli::cli_alert_info(
    "Unique df$type_val: {paste(unique(df$col), collapse=', ')}"
  )
}

# ----- msg_hist_vars -------

#' @param df the `data.frame` that has summary information.
#' @param var the `x` variable that is being supplied to the histogram
#' @param ba vector that is either `"before"` or `"after"`.
#'
#' @details
#' `msg_hist_vars()` provides info on the histogogram variables
#'
#' @name msg_functions
#' @export
msg_hist_vars <- function(df, var, ba) {
  if (ba == "before") {
    cli::cli_alert_info("Variable: {.var {var}}")
    cli::cli_alert_info("Rows before filtering: {.val {nrow(df)}}")
    cli::cli_alert_info(
      "Sample values: {.val {paste(head(sort(unique(df[[var]]))),
                        collapse = ', ')}}"
    )
  }
  if (ba == "after") {
    cli::cli_alert_success("Rows after filtering: {.val {nrow(df)}}")
  }
}


# ----- msg_input_source ------

#' @param input_source_name the name of the input
#' @param envir the R environment
#'
#' @details
#' `msg_input_source()` returns info on whether info from valid sources is being used.
#'
#' @name msg_functions
#'
#' @export
msg_input_source <- function(input_source_name, envir = parent.frame()) {
  valid_sources <- c("summary_sidebar_vals", "scatter_sidebar_vals")

  # Check if it's a valid name
  if (length(input_source_name) != 1 || !input_source_name %in% valid_sources) {
    cli::cli_abort(c(
      "Invalid {.arg input_source_name} provided",
      "x" = "You supplied: {.val {input_source_name}}",
      "i" = "Must be one of: {.val {valid_sources}}"
    ))
  }
}

# ====== msg_log_agent -----

#' @param x an `validator` object
#' @param name of the validator
#' @param show how many values to display, default is `10`
#'
#' @details
#' `msg_log_agent()` returns info on validator agents
#' and whether they were successful.
#'
#' @name msg_functions
#' @export
msg_log_agent <- function(x, name, show = 10) {
  raw_vals <- validate::values(x)
  vals <- unlist(raw_vals, use.names = FALSE)

  cli::cli_h3(name)

  cli::cli_alert_info("Rules: {length(vals)}")
  cli::cli_alert_info("Passed: {sum(vals, na.rm = TRUE)}")
  cli::cli_alert_info("Failed: {sum(!vals, na.rm = TRUE)}")
  cli::cli_alert_info("NA: {sum(is.na(vals))}")
  cli::cli_alert_info("all(..., na.rm=TRUE): {all(vals, na.rm = TRUE)}")

  if (any(is.na(vals))) {
    cli::cli_alert_warning("{name} contains NA results")

    df <- validate::as.data.frame(x)

    rule_cols <- setdiff(names(df), names(df)[!grepl("^\\.", names(df))])

    # validator columns are logical
    rule_cols <- names(df)[sapply(df, is.logical)]

    na_map <- df[, rule_cols, drop = FALSE]

    rows_with_na <- which(apply(na_map, 1, function(r) any(is.na(r))))

    cli::cli_alert_warning("Rows with NA: {length(rows_with_na)}")

    if (length(rows_with_na) > 0) {
      cli::cli_alert_info(
        "First rows with NA: {paste(head(rows_with_na, show), collapse = ', ')}"
      )
      cli::cli_verbatim({
        utils::head(df[rows_with_na, ], show)
      })
    }

    cols_with_na <- names(which(colSums(is.na(na_map)) > 0))

    cli::cli_alert_warning(
      "Rules producing NA: {cols_with_na}"
    )
  }

  invisible(NULL)
}


# ----- msg_mean_data ------

#' @param df the `data.frame` that has summary information.
#' @param summary_grouping_vars the summary grouping variables
#' @param y_vals the `y` value of interst
#'
#' @details
#' `msg_mean_data()` returns info on the mean data object
#'
#' @name msg_functions
#' @export

# ----- chekc mean_data ------
msg_mean_data <- function(df, summary_grouping_vars, y_vals) {
  cli::cli_h2("create_mean_data() diagnostics")

  cli::cli_ul(c(
    "df class: {paste(class(df), collapse = ', ')}",
    "df rows (if local): {tryCatch(nrow(df), msg = function(e) 'lazy tbl')}",
    "grouping_vars: {if (is.null(summary_grouping_vars)) 'NULL' else paste(summary_grouping_vars, collapse = ', ')}",
    "length(grouping_vars): {length(summary_grouping_vars)}",
    "y_vals: {if (is.null(y_vals)) 'NULL' else paste(y_vals, collapse = ', ')}",
    "length(y_vals): {length(y_vals)}"
  ))

  cli::cli_rule()
}


# ---- ehck if summary data is being triggered ----

#' @param df the `data.frame` that has summary information.
#' @param name the object name that has been triggered. It
#' is usually a `reactive` object.
#'
#' @details
#' `msg_summary_data()`returns info about summary data
#'
#' @name msg_functions
#' @export

msg_summary_data <- function(df, name = deparse(substitute(df))) {
  cli::cli_alert_success("{name} triggered")

  # Lazy dbplyr table → do NOT validate rows
  if (inherits(df, "tbl_lazy")) {
    cli::cli_alert_info("{name} is lazy (query constructed)")
    return(invisible(TRUE))
  }

  # Try/catch safety
  if (inherits(df, "try-msg")) {
    cli::cli_alert_danger("{name} failed completely")
    return(invisible(FALSE))
  }

  # Empty data
  if (nrow(df) == 0) {
    cli::cli_alert_warning("{name} has 0 rows")
    return(invisible(FALSE))
  }

  cli::cli_alert_success(
    "{name} rows: {.val {nrow(df)}}, cols: {.val {ncol(df)}}"
  )

  cli::cli_text("{.field {sort(names(df))}}")

  invisible(TRUE)
}

# ----- selected vars ------
#' @param selected_vars an object that has selected variables from
#' dropdowns.
#'
#' @details
#' `msg_selected_vars()`returns info about the selected varialbes
#'
#' @name msg_functions
#' @export

msg_selected_vars <- function(selected_vars) {
  cli::cli_ul(c(
    "y_variable value: {if (is.null(selected_vars)) 'NULL' else paste(selected_vars, collapse = ', ')}",
    "length(y_variable): {length(selected_vars)}"
  ))
}

# ---- check tab name -----
#' @param tab the name of the tab.
#'
#' @details
#' `msg_tab_name()`tab name.
#'
#' @name msg_functions
#' @export
msg_tab_name <- function(tab) {
  if (
    !(tab %in% c("summary_info", "scatter_plot", "view_data", "view_source"))
  ) {
    cli::cli_abort("Cannot execute function for {.val {tab}} tab")
  }
}

old_log_agent <- function(x, name) {

  vals <- values(x)
  smry <- summary(x)

  cli::cli_h3(name)

  cli::cli_alert_info("Rules: {length(vals)}")
  cli::cli_alert_info("Passed: {sum(vals, na.rm = TRUE)}")
  cli::cli_alert_info("Failed: {sum(!vals, na.rm = TRUE)}")
  cli::cli_alert_info("NA: {sum(is.na(vals))}")

  cli::cli_alert_info("all(..., na.rm=TRUE): {all(vals, na.rm = TRUE)}")

  if (any(is.na(vals))) {
    cli::cli_alert_warning("{name} contains NA results")
  }

  invisible(NULL)
}

old_2_log_agent <- function(x, name) {

  raw_vals <- values(x)

  # ---- flatten safely ----
  vals <- unlist(raw_vals, use.names = FALSE)

  cli::cli_h3(name)

  cli::cli_alert_info("Rules: {length(vals)}")
  cli::cli_alert_info("Passed: {sum(vals, na.rm = TRUE)}")
  cli::cli_alert_info("Failed: {sum(!vals, na.rm = TRUE)}")
  cli::cli_alert_info("NA: {sum(is.na(vals))}")

  cli::cli_alert_info("all(..., na.rm=TRUE): {all(vals, na.rm = TRUE)}")

  if (any(is.na(vals))) {
    cli::cli_alert_warning("{name} contains NA results")
  }

  invisible(NULL)
}

old_3_log_agent <- function(x, name, show = 10) {

  raw_vals <- values(x)
  vals <- unlist(raw_vals, use.names = FALSE)

  cli::cli_h3(name)

  cli::cli_alert_info("Rules: {length(vals)}")
  cli::cli_alert_info("Passed: {sum(vals, na.rm = TRUE)}")
  cli::cli_alert_info("Failed: {sum(!vals, na.rm = TRUE)}")
  cli::cli_alert_info("NA: {sum(is.na(vals))}")
  cli::cli_alert_info("all(..., na.rm=TRUE): {all(vals, na.rm = TRUE)}")

  if (any(is.na(vals))) {
    cli::cli_alert_warning("{name} contains NA results")

    # ---- summary table ----
    smry <- summary(x)

    cli::cli_alert_info("Rules with NA:")

    na_rules <- smry[is.na(smry$passes), ]

    print(utils::head(na_rules, show))

    # ---- row-level view ----
    df <- as.data.frame(x, add_columns = TRUE)

    if (".valid" %in% names(df)) {
      bad <- df[is.na(df$.valid), , drop = FALSE]

      if (nrow(bad) > 0) {
        cli::cli_alert_warning("Rows with NA validity:")
        print(utils::head(bad, show))
      }
    }
  }

  invisible(NULL)
}


log_agent <- function(x, name, show = 10) {

  raw_vals <- values(x)
  vals <- unlist(raw_vals, use.names = FALSE)

  cli::cli_h3(name)

  cli::cli_alert_info("Rules: {length(vals)}")
  cli::cli_alert_info("Passed: {sum(vals, na.rm = TRUE)}")
  cli::cli_alert_info("Failed: {sum(!vals, na.rm = TRUE)}")
  cli::cli_alert_info("NA: {sum(is.na(vals))}")
  cli::cli_alert_info("all(..., na.rm=TRUE): {all(vals, na.rm = TRUE)}")

  if (any(is.na(vals))) {
    cli::cli_alert_warning("{name} contains NA results")

    df <- as.data.frame(x, add_columns = TRUE)

    rule_cols <- setdiff(names(df), names(df)[!grepl("^\\.", names(df))])

    # validator columns are logical
    rule_cols <- names(df)[sapply(df, is.logical)]

    na_map <- df[, rule_cols, drop = FALSE]

    rows_with_na <- which(apply(na_map, 1, function(r) any(is.na(r))))

    cli::cli_alert_warning("Rows with NA: {length(rows_with_na)}")

    if (length(rows_with_na) > 0) {
      cli::cli_alert_info("First rows with NA: {paste(head(rows_with_na, show), collapse = ', ')}")
      print(utils::head(df[rows_with_na, ], show))
    }

    cols_with_na <- names(which(colSums(is.na(na_map)) > 0))

    cli::cli_alert_warning("Rules producing NA:")
    print(cols_with_na)
  }

  invisible(NULL)
}

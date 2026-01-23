fix_var_generic <- function(df, var_raw) {


  # detect if it's one of the synthetic length vars
  if (grepl("^length_mm__", var_raw)) {
    # split if length_mm__ is presnet in

    parts <- strsplit(var_raw, "__")[[1]]
    # grab the second element of part
    var_type <- parts[2]

    # Filter df to matching length type
    df <- df %>%
      dplyr::filter(length_type == var_type)

    # Dynamic label
    var_label <- paste0(stringr::str_to_title(var_type), " Length (mm)")
    var <- "length_mm"

  } else if (grepl("^energy_units__", var_raw)) {

    parts <- strsplit(var_raw, "__")[[1]]
    # grab the second element of part
    var_type <- parts[2]
    cli::cli_alert_danger("var_type is: {.field {var_type}}")

    df <- df %>%
      dplyr::filter(energy_units == var_type)

    eu <- df |>
      distinct(energy_units) |>
      pull()
    cli::cli_alert_danger("units is: {.field {eu}}")
    # Dynamic label
    var_label <- paste0("Energy Density (", var_type, ")")
    var <- "energy_measurement"

    cli::cli_alert_danger("var_label is: {.field {var_label}}")


  } else {
    cli::cli_alert_info("Checking for {.field {var_raw}} in columns...")
    cli::cli_inform("Available columns: {.val {colnames(df)}}")
    cli::cli_inform("var_raw %in% colnames(df): {var_raw %in% colnames(df)}")
    req(var_raw %in% colnames(df))
    # Normal variable
    var <- var_raw
    var_label <- convert_nice_name(var)[[1]]
  }

  list(
    df = df,
    var = var,
    var_label = var_label
  )

}
# ---- fix tittle label -----
fix_title_label <-  function(x, max = NULL) {
  if (is.null(max)) {
    max <- 4
  }
  if (length(x) <= max) {
    paste(x, collapse = ", ")
  } else {
    paste0(
      paste(head(x, max), collapse = ", "),
      ", <br>… (", length(x) - max, " more)"
    )
  }
}

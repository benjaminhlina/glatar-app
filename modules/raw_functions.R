create_raw_data <- function(
  con,
  main_input,
  input_source,
  var_field,
  tab = NULL,
  activated = NULL
) {
  shiny::reactive({
    # use for other tabs ---
    if (!is.null(tab)) {
      check_tab_name(tab)

      shiny::req(main_input$tabs == tab)
    }

    if (!is.null(activated)) {
      shiny::req(activated)
    }

    # get connection
    con_db <- if (inherits(con, "reactive")) con() else con

    # get selected vars
    # Handle multiple var_fields
    selected_vars <- c()

    for (field in var_field) {
      vars <- input_source[[field]]
      vars_val <- if (inherits(vars, "reactive")) vars() else vars
      cli::cli_alert_info(
        "Field: {field}, Value: {vars_val}, Length: {length(vars_val)}"
      )
      selected_vars <- c(selected_vars, vars_val)
    }

    # remove null selected_vars
    selected_vars <- unique(selected_vars[!is.null(selected_vars)])

    # alert
    cli::cli_alert("selected vars is: {.var {selected_vars}}")

    # check slected _vars
    check_selected_vars(selected_vars = selected_vars)

    # get groups

    shiny::req(con_db)

    # ----- if grouping_vars is null or length is 0 return a null object all
    # together

    # ---- actually get data when group_vars is valid ----
    df <- get_raw_data(
      con = con_db,
      selected_vars = selected_vars
    )

    return(df)
  })
}

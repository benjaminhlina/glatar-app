# ---- sumary data -----
# args here are con and main input with tab being used in view_summary and
# view_plot
create_summary_data <- function(con, main_input, tab = NULL) {
  reactive({

    if (!is.null(tab)) {
      check_tab_name(tab)

      req(main_input$tabs == tab)
    }

    table_name <- get_selected_table(main_input)

    req(table_name)

    check_table_name(table_name)

    # ---- acctuat gert data =----
    df <- get_summary_data(con = con, table_name)

    df
  })
}





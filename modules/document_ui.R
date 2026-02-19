docs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "Documentation & Templates",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        tags$p("Download the data entry templates and documentation below:"),

        fluidRow(

          # ---- Templates column ----
          column(
            width = 6,
            tags$h4("Templates"),

            tags$ul(
              tags$li(
                tags$a(
                  "GLATAR Data Entry Template (v15)",
                  href = "data-entry-template/GLATAR_data_entry_template_v15.xlsx",
                  download = "GLATAR_data_entry_template_v15.xlsx",
                  target = "_blank"
                )
              )

              # add more templates here
            )
          ),

            )
          )
        )
      )
    )
  )
}

docs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # future logic here
  })
}

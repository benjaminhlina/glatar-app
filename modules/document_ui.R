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
                  "Basic Data Entry Template (v16)",
                  href = "data-entry-template/GLATAR_data_entry_template_v16.xlsx",
                  download = "GLATAR_data_entry_template_v16.xlsx",
                  target = "_blank"
                )
              ),
              # add more templates here
              tags$li(
                tags$a(
                  "Advanced Data Entry Template (v16)",
                  href = "data-entry-template/GLATAR_data_entry_template_v16_long.xlsx",
                  download = "GLATAR_data_entry_template_v16_long.xlsx",
                  target = "_blank"
                )
              )

              # add more templates here
            )
          ),

          # ---- Documentation column ----
          column(
            width = 6,
            tags$h4("Documentation"),

            # tags$ul(
            #   tags$li(
            #     tags$a(
            #       "GLATAR User Guide",
            #       href = "www/documentation/GLATAR_user_guide.pdf",
            #       target = "_blank"
            #     )
            #   ),
            # tags$li(
            #   tags$a(
            #     "Data Submission Instructions",
            #     href = "www/documentation/data_submission_instructions.pdf",
            #     target = "_blank"
            #   )
            # )
            tags$li(
              tags$a(
                "Fatty Acid Documentation",
                href = "documentation/fatty_acids_documentation.pdf",
                target = "_blank"
              )
            ),
            tags$li(
              tags$a(
                "Polychlorinated Biphenyls (PCBs) Documentation",
                href = "documentation/polychlorinated_biphenyls_documentation.pdf",
                target = "_blank"
              )
            ),
            tags$li(
              tags$a(
                "Thiamine and Thiaminase Documentation",
                href = "documentation/thiamine_and_thiaminase_documentation.pdf",
                target = "_blank"
              )
            )
          )
        )

      )
    )
  )
}

docs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ---- future logic -----
  })
}

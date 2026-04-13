docs_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Documentation & Templates",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        shiny::tags$p(
          "Download the data entry templates and documentation below:"
        ),

        shiny::fluidRow(
          # ---- Templates column ----
          shiny::column(
            width = 6,
            shiny::tags$h4("Templates"),
            shiny::tags$h5(
              "Basic Data Entry Template - collects energy density, 
                  proximate composition, and stable isotope data."
            ),

            shiny::tags$ul(
              shiny::tags$li(
                style = "white-space: nowrap;",
                shiny::tags$a(
                  "Basic Data Entry Template (v21)",
                  href = "data-entry-template/GLATAR_data_entry_template_v21.xlsx",
                  download = "GLATAR_data_entry_template_v21.xlsx",
                  target = "_blank"
                )
              )
            ),

            shiny::tags$h5(
              "Extended Data Entry Template - additional fields to collect thiamine, fatty and amino acid, mercury, and PCB data."
            ),
            # add more templates here
            shiny::tags$ul(
              shiny::tags$li(
                style = "white-space: nowrap;",
                shiny::tags$a(
                  "Extended Data Entry Template (v21)",
                  href = "data-entry-template/GLATAR_data_entry_template_v21_long.xlsx",
                  download = "GLATAR_data_entry_template_v21_long.xlsx",
                  target = "_blank",
                  style = "display: inline;"
                )
              )

              # add more templates here
            )
          ),

          # ---- Documentation column ----
          shiny::column(
            width = 6,
            shiny::tags$h4("Documentation"),

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
            shiny::tags$li(
              shiny::tags$a(
                "Rationale and Purpose of GLATAR",
                href = "documentation/rationale_for_GLATAR.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
                "Calorimetry Documentation",
                href = "documentation/calorimetry_documentation.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
                "Fatty Acid Documentation",
                href = "documentation/fatty_acids_documentation.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
                "Mercury and Methylmercury Documentation",
                href = "documentation/mercury_and_methylmercury_documentation.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
                "Polychlorinated Biphenyls (PCBs) Documentation",
                href = "documentation/polychlorinated_biphenyls_documentation.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
                "Stable Isotope Documentation",
                href = "documentation/stable_isotope_documentation.pdf",
                target = "_blank"
              )
            ),
            shiny::tags$li(
              shiny::tags$a(
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

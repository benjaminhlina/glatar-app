how_to_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = "how_to_use",
    shiny::h2("How to Use GLATAR"),

    # ── 1. Working with Existing Data ────────────────────────────────────────
    shiny::h3("1. Working with Existing Data"),
    shiny::p(
      "First, determine if you want to summarise existing data in a table (",
      shiny::actionLink(ns("go_summarize_intro"), "Summarize Tab"),
      "), visualize raw data (",
      shiny::actionLink(ns("go_scatter_intro"), "Scatter Plot"),
      "), or view raw data (",
      shiny::actionLink(ns("go_raw_intro"), "View Raw Data"),
      ")."
    ),

    # Summary Statistics
    shiny::h4(shiny::actionLink(
      ns("go_summarize_heading"),
      "Summary Statistics"
    )),
    shiny::tags$ol(
      type = "a",
      shiny::tags$li(
        shiny::strong("Select a theme"),
        " (e.g. calorimetry, fatty acids). For all pulldowns,
        hover your mouse over the box and options appear. As you become more familiar with GLATAR
        you can begin typing relevant text and it will autofill."
      ),
      shiny::tags$li(
        shiny::strong("Select the data fields of interest"),
        " for the tabular summary
        (e.g. \u00b9\u2075N, total length, etc.). You can select up to 4 variables."
      ),
      shiny::tags$li(
        shiny::strong("Select the data field of interest"),
        " for the frequency histogram
        (e.g. \u00b9\u2075N). Only one variable can be selected."
      ),
      shiny::tags$li(
        shiny::strong("Apply data filters"),
        ", if you wish:",
        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong("Organism type:"),
            " fish, invertebrate, other, all."
          ),
          shiny::tags$li(
            shiny::strong("Species"),
            " (default is all species). You can select an
            unlimited number of species. Begin typing relevant text and options will appear."
          ),
          shiny::tags$li(
            shiny::strong("Grouping variables"),
            " (e.g. water body, common name)."
          ),
          shiny::tags$li(
            shiny::strong("Data type:"),
            " point estimates, composite samples, reported
            averages, coefficients from equations, or all data (data type will be reported in the
            summary table)."
          ),
          shiny::tags$li(
            shiny::strong("Waterbody"),
            " (e.g. Lake Ontario, Middle Fork River, etc.).
            The list is constrained to locations available in the database for the above selected criteria."
          )
        )
      ),
      shiny::tags$li(
        "As each selection is made the available data will be displayed in both the table and the
        histogram to the right."
      ),
      shiny::tags$li(
        "You can ",
        shiny::strong("export the tabular results"),
        " from your search to Excel using
        the button at the bottom of the menu."
      )
    ),

    # Scatter Plot
    shiny::h4(shiny::actionLink(ns("go_scatter_heading"), "Scatter Plot")),
    shiny::tags$ol(
      type = "a",
      shiny::tags$li(
        shiny::strong("Select a theme"),
        " (e.g. calorimetry, fatty acids). Hover your mouse over
        the box for options, or begin typing to autofill."
      ),
      shiny::tags$li(
        shiny::strong("Select up to four grouping variables"),
        " (e.g. waterbody, sex, month, etc.)."
      ),
      shiny::tags$li(
        shiny::strong("Select the x-variable"),
        " of interest (e.g. percent water (%), total length, etc.)."
      ),
      shiny::tags$li(
        shiny::strong("Select the y-variable"),
        " of interest (e.g. energy density, weight, etc.)."
      ),
      shiny::tags$li(
        shiny::strong("Apply data filters"),
        ", if you wish:",
        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong("Organism type:"),
            " fish, invertebrate, other, all."
          ),
          shiny::tags$li(
            shiny::strong("Species"),
            " (default is all species). You can select an
            unlimited number of species. Begin typing relevant text and options will appear."
          ),
          shiny::tags$li(
            shiny::strong("Data type:"),
            " point estimates, composite samples, reported
            averages, coefficients from equations, or all data."
          ),
          shiny::tags$li(
            shiny::strong("Waterbody"),
            " (e.g. Lake Ontario, Middle Fork River, etc.).
            The list is constrained to locations available in the database for the above selected criteria."
          )
        )
      ),
      shiny::tags$li(
        "As each selection is made the available data will be displayed in both the table and the
        histogram to the right."
      )
    ),

    # View Raw Data
    shiny::h4(shiny::actionLink(ns("go_raw_heading"), "View Raw Data")),
    shiny::tags$ol(
      type = "i",
      shiny::tags$li(
        shiny::strong("Select a theme"),
        " (e.g. calorimetry, stable isotopes, etc.). This view
        also allows you to explore metadata tables (e.g. data source, location, etc.)."
      ),
      shiny::tags$li(
        "Use the ",
        shiny::strong("search tool"),
        " to query specific terms within the chosen table
        (e.g. a specific author or waterbody)."
      )
    ),

    shiny::hr(),

    # ── 2. Map ────────────────────────────────────────────────────────────────
    shiny::h3(shiny::actionLink(ns("go_map_heading"), "2. Map")),
    shiny::tags$ol(
      type = "a",
      shiny::tags$li(
        "The map tool is a convenient way to ",
        shiny::strong("visualise the spatial extent"),
        " of data currently contained within GLATAR."
      ),
      shiny::tags$li(
        "The viewable area of the map can be easily scaled using the ",
        shiny::strong(
          "buttons in
        the upper left"
        ),
        "."
      ),
      shiny::tags$li(
        "Clicking on a specific dot will display the ",
        shiny::strong("geographic name and species"),
        " recorded for that location."
      )
    ),

    shiny::hr(),

    # ── 3. Searching by Taxon or Source ───────────────────────────────────────
    shiny::h3(shiny::actionLink(
      ns("go_search_heading"),
      "3. Searching by Taxon or Source"
    )),
    shiny::tags$ol(
      type = "a",
      shiny::tags$li(
        "A user can search GLATAR for records on a specific taxon using virtually any level of
        taxonomic classification (e.g. class, order, family, genus, species, common name). The
        search box uses ",
        shiny::strong("smart text"),
        " so as you enter characters, matching
        records appear. For example, typing \u201cAlo\u201d will list records for ",
        shiny::em("Alosa pseudoharengus"),
        ", ",
        shiny::em("Ictiobus cyprinellus"),
        " (Bigmouth buffalo), and Goldeneye (",
        shiny::em("Hiodon alosoides"),
        ")."
      ),
      shiny::tags$li(
        "Similarly, a user can search by ",
        shiny::strong("data source"),
        " (e.g. author, institution, journal, etc.). The more complete the entry in the search
        field, the more refined the result."
      ),
      shiny::tags$li(
        "These search tools are a convenient way to quickly check if data exist for a species or
        by a specific author \u2014 helpful when entering new data to avoid duplication."
      )
    ),

    shiny::hr(),

    # ── 4. Uploading Data ─────────────────────────────────────────────────────
    shiny::h3(shiny::actionLink(ns("go_upload_heading"), "4. Uploading Data")),

    shiny::h4("a. Data Entry Templates"),
    shiny::p(
      "Data upload uses one of two pre-defined templates. It is important ",
      shiny::strong("not"),
      " to alter the templates (e.g. add or delete fields), nor to exclude any of the underlying
      data tables."
    ),
    shiny::tags$ol(
      type = "1",
      shiny::tags$li(
        "The ",
        shiny::strong("simple data entry template"),
        " only accepts calorimetry, proximate
        composition, and stable isotope data with all related metadata."
      ),
      shiny::tags$li(
        "The ",
        shiny::strong("extended data entry template"),
        " also allows the user to upload
        data on fatty acids, amino acids, thiamine / thiaminase, mercury, and PCBs, again with
        related metadata."
      )
    ),
    shiny::p(
      "The following instructions apply to both templates. Fields highlighted in ",
      shiny::span(style = "color: green; font-weight: bold;", "green"),
      " are mandatory and cannot be left blank; all other fields can be left blank if no
      information is available."
    ),
    shiny::tags$ol(
      type = "i",
      shiny::tags$li(
        "The ",
        shiny::strong("first tab"),
        " is the ",
        shiny::strong("data dictionary"),
        " \u2014 fully describing all data fields with definitions, examples, and constraints."
      ),
      shiny::tags$li(
        "The ",
        shiny::strong("second tab"),
        " is the ",
        shiny::strong("submission record"),
        " \u2014 recording who entered the data and their affiliation and contact information."
      ),
      shiny::tags$li(
        "The ",
        shiny::strong("third tab"),
        " is the ",
        shiny::strong("source record"),
        " \u2014 including authors, institutional affiliation, publication name, related fields
        (volume, pages, publisher, etc.), and the DOI or ISBN."
      ),
      shiny::tags$li(
        "The ",
        shiny::strong("final tab"),
        " is the ",
        shiny::strong("sample record"),
        " \u2014 linking back to the source record and containing both the data and a broad array
        of metadata fields to facilitate database queries."
      ),
      shiny::tags$li(
        "For all tables, ",
        shiny::strong("copy and paste of existing rows"),
        " can expedite data entry where multiple records share common metadata. Where possible,
        enter all data types in a given row (e.g. proximate composition, stable isotope, and
        fatty acid data together)."
      ),
      shiny::tags$li(
        "Once data have been entered, ",
        shiny::strong("save the file (MS Excel)"),
        " to your computer."
      )
    ),

    shiny::h4("b. Uploading Data to GLATAR"),
    shiny::tags$ol(
      type = "i",
      shiny::tags$li(
        "From the ",
        shiny::strong("Upload Data tab"),
        " in the menu, follow the on-screen instructions."
      ),
      shiny::tags$li(
        "Use the ",
        shiny::strong("Browse button"),
        " to select the pre-populated data entry template
        you wish to upload. A status message will confirm the file was successfully read."
      ),
      shiny::tags$li(
        "Click the ",
        shiny::strong("Upload and Process button"),
        ". This may take a moment depending
        on file size. GLATAR will validate the data against pre-defined entry standards. If a conflict
        is found (e.g. an unrecognised species), a message will identify the row in your template
        and suggest possible corrections. Taxonomic information relies on the ",
        shiny::a(
          "International Taxonomic Information System (ITIS)",
          href = "https://www.itis.gov",
          target = "_blank"
        ),
        ".",
        shiny::tags$ul(
          shiny::tags$li(
            "If you encounter conflicts you cannot resolve, follow the contact information provided
            in the comment box at the bottom of each validation report."
          )
        )
      ),
      shiny::tags$li(
        "Once your data have been validated, click the ",
        shiny::strong("Upload button"),
        " to append your data to GLATAR. ",
        shiny::strong("Note:"),
        " you will need to exit and re-enter GLATAR to see your appended
        data alongside pre-existing records."
      )
    ),

    shiny::hr(),

    # ── 5. Additional Resources ───────────────────────────────────────────────
    shiny::h3("5. Additional Resources"),
    shiny::tags$dl(
      shiny::tags$dt(shiny::actionLink(ns("go_docs_heading"), "Documentation")),
      shiny::tags$dd(
        "The Documentation pane provides brief narratives and key citations for each of the
        themes included in GLATAR."
      ),
      shiny::tags$dt(shiny::actionLink(ns("go_about_heading"), "About")),
      shiny::tags$dd(
        "The About pane provides information on funding, contributors, contacts, and how to
        cite GLATAR."
      ),
      shiny::tags$dt(shiny::actionLink(ns("go_home_heading"), "Home")),
      shiny::tags$dd(
        "The Home page provides general orientation to menus, a clickable visual taking you
        to related documentation pages, and general information about GLATAR."
      )
    ),

    shiny::hr(),

    # ── 6 & 7. Interface Tips ─────────────────────────────────────────────────
    shiny::h3("6\u20137. Interface Tips"),
    shiny::tags$ul(
      shiny::tags$li(
        "The ",
        shiny::strong("three horizontal lines"),
        " in the header bar hide / reveal the
        menu on the left side of the page."
      ),
      shiny::tags$li(
        "The ",
        shiny::strong("fox icon"),
        " on the right side of the header bar takes you to
        the source code on GitHub. The code is publicly available to encourage the community to
        explore and suggest enhancements for future incorporation into GLATAR."
      )
    )
  )
}


how_to_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    # Map each actionLink id to its shinydashboard tabName
    tab_links <- list(
      go_summarize_intro = "summary_info",
      go_summarize_heading = "summary_info",
      go_scatter_intro = "scatter_plot",
      go_scatter_heading = "scatter_plot",
      go_raw_intro = "view_data",
      go_raw_heading = "view_data",
      go_map_heading = "view_map",
      go_search_heading = "taxa_search",
      go_upload_heading = "insert_data",
      go_docs_heading = "docs",
      go_about_heading = "about",
      go_home_heading = "home"
    )

    purrr::walk2(names(tab_links), tab_links, function(link_id, tab_name) {
      shiny::observeEvent(input[[link_id]], {
        shinydashboard::updateTabItems(parent_session, "tabs", tab_name)
      })
    })
  })
}

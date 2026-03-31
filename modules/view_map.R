view_map_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Map of Locations"),
    shiny::p(
      "This panel provides an interactive map displaying the sample locations for data in GLATAR. 
      To use, explore the map and click on each location to view the waterbody, site, species collected 
      (i.e., common and scientific name), the name of the collector, and the data in GLATAR that was collected. 
      The colour of each location indicates whether the sample was derived from a wild or lab organism. "
    ),
    shinycssloaders::withSpinner(
      leaflet::leafletOutput(ns("map"), height = "700px", width = "100%"),
      type = 4,
      caption = "Please wait for the map to load..."
    )
  )
}

# ---- server -----
view_map_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      # Check if the table exists before proceeding
      if (!"tbl_location" %in% DBI::dbListTables(con)) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addMarkers(lng = 0, lat = 0, popup = "No Data")
        )
      }

      # Named vector: table name → friendly label
      data_table <- data_tables()

      flag_cols <- paste0("has_", sub("tbl_", "", names(data_table)))

      # ── 1. Base join ──────────────────────────────────────────────────────────────
      locs <- dplyr::tbl(con, "tbl_location") |>
        dplyr::left_join(dplyr::tbl(con, "tbl_samples"))

      # ── 2. Flag presence of each data type ──────────────────────────────────────
      locs <- get_data_tables(
        con,
        df = locs,
        data_tables = data_table,
        flag_cols = flag_cols,
        var = "sample_id"
      )

      # ── 3. Aggregate to unique locations ─────────────────────────────────────────
      locs <- clean_data_tables(
        df = locs,
        flag_cols = flag_cols,
        type = data_table,
        filter_coords = TRUE,
        group_cols = c(
          "latitude",
          "longitude",
          "waterbody",
          "area",
          "common_name",
          "scientific_name",
          "pi_name",
          "wild_lab"
        )
      ) |>
        dplyr::mutate(
          wild_lab = stringr::str_to_sentence(wild_lab),
          popup_info = paste(
            "<b>Waterbody:</b>",
            waterbody,
            "<br>",
            "<b>Area:</b>",
            area,
            "<br>",
            "<b>Common Name:</b>",
            common_name,
            "<br>",
            "<b>Scientific Name:</b>",
            scientific_name,
            "<br>",
            "<b>Collector Name:</b>",
            pi_name,
            "<br>",
            "<b>Data Types:</b>",
            data_tables,
            "<br>",
            "<b>n:</b>",
            n_samples
          )
        )

      # Ensure data is not empty

      pal <- leaflet::colorFactor(
        palette = c("blue", "red"),
        levels = c("Wild", "Lab")
      )
      if (nrow(locs) == 0) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addMarkers(lng = 0, lat = 0, popup = "No Data Available")
        )
      }

      # Create map with popups
      leaflet::leaflet(locs) |>
        leaflet::addTiles() |>
        leaflet::addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup_info,
          radius = 5,
          color = ~ pal(wild_lab),
          fillColor = ~ pal(wild_lab),
          fillOpacity = 0.7
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~wild_lab,
          title = "Sample Type"
        )
    })
  })
}

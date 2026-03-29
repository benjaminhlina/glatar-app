view_map_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Map of Locations"),
    leaflet::leafletOutput(ns("map"), height = "700px", width = "100%")
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

      # Fetch location data
      locs <- dplyr::tbl(con, 'tbl_location') |>
        dplyr::left_join(dplyr::tbl(con, "tbl_samples"))

      # Ensure required columns exist
      missing_cols <- setdiff(
        c("latitude", "longitude", "waterbody", "area", "site", "site_depth"),
        colnames(locs)
      )

      # remove locations that don't have lon
      locs <- locs |>
        dplyr::filter(!(is.na(longitude))) |>
        dplyr::select(
          latitude,
          longitude,
          waterbody,
          area,
          common_name,
          scientific_name
        ) |>
        dplyr::collect()

      # if columns are missing return blank map with
      if (length(missing_cols) > 0) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addMarkers(
              lng = 0,
              lat = 0,
              popup = "Missing Required Columns"
            )
        )
      }

      # Ensure data is not empty
      if (nrow(locs) == 0) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addMarkers(lng = 0, lat = 0, popup = "No Data Available")
        )
      }
      # Create popup content dynamically
      locs$popup_info <- paste(
        "<b>Waterbody:</b>",
        locs$waterbody,
        "<br>",
        "<b>Common Name:</b>",
        locs$common_name,
        "<br>",
        "<b>Scientific Name:</b>",
        locs$scientific_name
      )

      # Create map with popups
      leaflet::leaflet(locs) |>
        leaflet::addTiles() |>
        leaflet::addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~popup_info,
          radius = 5,
          color = "blue",
          fillOpacity = 0.7
        )
    })
  })
}

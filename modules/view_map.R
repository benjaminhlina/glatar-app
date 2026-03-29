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

      # Named vector: table name → friendly label
      data_tables <- c(
        "tbl_amino_acid" = "Amino Acid",
        "tbl_calorimetry" = "Calorimetry",
        "tbl_contaminants" = "Contaminants",
        "tbl_fatty_acid" = "Fatty Acid",
        "tbl_isotope" = "Isotope",
        "tbl_lipid_composition" = "Lipid Composition",
        "tbl_proxcomp" = "Proximate Composition",
        "tbl_thiamine" = "Thiamine"
      )

      flag_cols <- paste0("has_", sub("tbl_", "", names(data_tables)))

      # ── 1. Base join ──────────────────────────────────────────────────────────────
      locs <- dplyr::tbl(con, "tbl_location") |>
        dplyr::left_join(dplyr::tbl(con, "tbl_samples"))

      # ── 2. Flag presence of each data type ──────────────────────────────────────
      for (i in seq_along(data_tables)) {
        locs <- locs |>
          dplyr::left_join(
            dplyr::tbl(con, names(data_tables)[i]) |>
              dplyr::distinct(sample_id) |>
              dplyr::mutate(!!flag_cols[i] := 1L),
            by = "sample_id"
          )
      }

      # ── 3. Aggregate to unique locations ─────────────────────────────────────────
      locs <- locs |>
        dplyr::filter(!is.na(longitude)) |>
        # Replace NA flags with 0 before aggregating
        dplyr::mutate(
          dplyr::across(dplyr::all_of(flag_cols), ~ dplyr::coalesce(., 0L))
        ) |>
        # Use group_by + summarise (not distinct) so flags are OR'd across samples
        dplyr::group_by(
          latitude,
          longitude,
          waterbody,
          area,
          common_name,
          scientific_name,
          pi_name,
          wild_lab
        ) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(flag_cols), max),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          data_types = purrr::pmap_chr(
            dplyr::pick(dplyr::all_of(flag_cols)),
            \(...) {
              flags <- c(...)
              labels <- unname(data_tables)[as.logical(flags)]
              if (length(labels) == 0) {
                "None"
              } else {
                paste(labels, collapse = ", ")
              }
            }
          ),
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
            data_types
          )
        )

      # Ensure required columns exist
      # missing_cols <- setdiff(
      #   c("latitude", "longitude", "waterbody", "area", "site", "site_depth"),
      #   colnames(locs)
      # )

      # # if columns are missing return blank map with
      # if (length(missing_cols) > 0) {
      #   return(
      #     leaflet::leaflet() |>
      #       leaflet::addTiles() |>
      #       leaflet::addMarkers(
      #         lng = 0,
      #         lat = 0,
      #         popup = "Missing Required Columns"
      #       )
      #   )
      # }

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

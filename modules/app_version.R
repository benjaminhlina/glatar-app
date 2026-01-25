app_version_head <- function() {

  tags$head(
    tags$style(HTML("
        #app-version {
          position: fixed;
          bottom: 6px;
          left: 280px;
          font-size: 12px;
          color: #888;
          z-index: 1000;
          display: flex;
          gap: 6px;
          align-items: center;
      }

      #app-version .version {
        background-color: #f4f4f4;
        border-radius: 3px;
        padding: 2px 6px;
        font-weight: 600;
        color: #444;
      }

      #app-version .build {
        background-color: #e9ecef;
        border-radius: 3px;
        padding: 2px 6px;
        color: #666;
        }
      "))
  )
}



app_version_label <- function(app_version, build_time = NULL) {
  if (is.null(build_time)) {
    build_time <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  }

  tags$div(
    id = "app-version",
    tags$span(class = "version", paste0("v", app_version)),
    tags$span(class = "build", paste0("Built: ", build_time))
  )
}



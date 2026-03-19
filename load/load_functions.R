# ------ load functions -----
load_scripts <- function(folder, type) {
  cli::cli_alert_info("Starting the loading of functions....")
  cli::cli_ul(list.files(folder, full.names = TRUE))

  files <- list.files(folder, full.names = TRUE)

  lapply(files, function(f) {
    tryCatch(
      source(f, local = FALSE),
      error = function(e) {
        cli::cli_alert_danger("Failed to load {.file {f}}: {e$message}")
      }
    )
  })

  cli::cli_alert_success(
    paste("All {length(files)}", type, "successfully loaded!")
  )
}

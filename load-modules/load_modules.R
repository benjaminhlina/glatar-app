load_modules <- function() {
  cli::cli_alert_info("Starting the loading of modules....")
  cli::cli_ul(list.files('modules', full.names = TRUE))

  module_files <- list.files("modules", full.names = TRUE)

  lapply(module_files, function(f) {
    tryCatch(
      source(f, local = FALSE),
      error = function(e) {
        cli::cli_alert_danger("Failed to load {.file {f}}: {e$message}")
      }
    )
  })

  cli::cli_alert_success(
    "All {length(module_files)} modules successfully loaded!"
  )
}

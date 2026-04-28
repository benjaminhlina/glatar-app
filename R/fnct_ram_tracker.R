# ---- ram tracker -----
#' Ram Tracker
#'
#' Tracks the amoutn of RAM used during shiny app
#' deployment.
#'
#' @returns displays the amont of RAM used during
#' app deployment. Checks RAM usage every 5 seeconds
#' and returns values in mb.
#'
#' @export
ram_tracker <- function() {
  shiny::observe({
    mem <- lobstr::mem_used()
    mem_mb <- round(as.numeric(mem) / 1024^2, 2)
    timestamp <- format(Sys.time(), "%H:%M:%S")

    cli::cli_alert_info("RAM: {mem_mb} mb at {timestamp}")

    shiny::invalidateLater(5000) # Print every 5 seconds
  })
}

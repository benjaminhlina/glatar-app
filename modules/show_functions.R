show_plot_message <- function(msg) {
  grid.newpage()
  grid.text(
    msg,
    x = 0.5,
    y = 0.5,
    gp = gpar(
      fontsize = 20,
      fontface = "bold",
      col = "black"
    )
  )
}


empty_scatter_message <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 0.5,
      label = msg,
      size = 6,
      fontface = "bold",
      hjust = 0.5,
      vjust = 0.5
    ) +
    ggplot2::theme_void()
}

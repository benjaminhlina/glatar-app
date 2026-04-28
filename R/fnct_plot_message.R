# ----- plot message ------
#' Plot message functions
#'
#' This function allows for [ggplot2](https://ggplot2.tidyverse.org/) to be used to display blank plots
#' with messages to the user.
#'
#' @param msg A character vector containing the message that is desired.
#' @return a `ggplot()` object to be displayed.

#' @export

plot_message <- function(msg) {
  # Wrap msg in markdown for styling if desired
  styled_msg <- glue::glue(
    "<span style='color:#333333; font-size:14pt;'>{msg}</span>"
  )

  ggplot2::ggplot() +
    ggplot2::theme_void() +
    # Outer box
    ggplot2::annotate(
      "rect",
      xmin = 0.1,
      xmax = 0.9,
      ymin = 0.4,
      ymax = 0.6,
      fill = "#ffffff",
      color = "#2196F3",
      linewidth = 2
    ) +
    # Blue left accent bar
    ggplot2::annotate(
      "rect",
      xmin = 0.1,
      xmax = 0.13,
      ymin = 0.4,
      ymax = 0.6,
      fill = "#2196F3",
      color = NA
    ) +
    # "i" icon circle
    ggplot2::annotate(
      "point",
      x = 0.17,
      y = 0.5,
      size = 12,
      color = "#2196F3"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.17,
      y = 0.5,
      label = "i",
      size = 8,
      color = "white",
      fontface = "bold"
    ) +
    # ggtext textbox — wraps automatically to fit width
    ggtext::geom_textbox(
      ggplot2::aes(x = 0.23, y = 0.5, label = styled_msg),
      width = grid::unit(0.62, "npc"),
      hjust = 0,
      vjust = 0.5,
      box.colour = NA,
      fill = NA,
      size = 5,
      box.padding = ggplot2::margin(0, 4, 0, 4)
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}

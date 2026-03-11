# ----- empty plot ------
# need to add ggtext to make this nicer
empty_plot <- function(msg) {
  # Wrap msg in markdown for styling if desired
  styled_msg <- glue::glue(
    "<span style='color:#333333; font-size:14pt;'>{msg}</span>"
  )

  ggplot() +
    theme_void() +
    # Outer box
    annotate(
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
    annotate(
      "rect",
      xmin = 0.1,
      xmax = 0.13,
      ymin = 0.4,
      ymax = 0.6,
      fill = "#2196F3",
      color = NA
    ) +
    # "i" icon circle
    annotate("point", x = 0.17, y = 0.5, size = 12, color = "#2196F3") +
    annotate(
      "text",
      x = 0.17,
      y = 0.5,
      label = "i",
      size = 8,
      color = "white",
      fontface = "bold"
    ) +
    # ggtext textbox — wraps automatically to fit width
    geom_textbox(
      aes(x = 0.23, y = 0.5, label = styled_msg),
      width = unit(0.62, "npc"),
      hjust = 0,
      vjust = 0.5,
      box.colour = NA,
      fill = NA,
      size = 5,
      box.padding = margin(0, 4, 0, 4)
    ) +
    xlim(0, 1) +
    ylim(0, 1)
}

is_empty <- function(x) {
  is.null(x) || length(x) == 0 || all(x == "")
}

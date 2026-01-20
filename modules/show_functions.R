# ----- empty plot ------
# need to add ggtext to make this nicer
empty_plot <- function(msg) {
  ggplot() +
    theme_void() +
    annotate("rect",
             xmin = 0.1, xmax = 0.9,
             ymin = 0.4, ymax = 0.6,
             fill = "#ffffff",
             color = "#2196F3",
             linewidth = 2) +
    annotate("rect",
             xmin = 0.1, xmax = 0.13,
             ymin = 0.4, ymax = 0.6,
             fill = "#2196F3",
             color = NA) +
    annotate("point",
             x = 0.17, y = 0.5,
             size = 12,
             color = "#2196F3") +
    annotate("text",
             x = 0.17, y = 0.5,
             label = "i",
             size = 8,
             color = "white",
             fontface = "bold") +
    annotate("text",
             x = 0.23, y = 0.5,
             label = msg,
             size = 6,
             color = "#333333",
             hjust = 0) +
    xlim(0, 1) +
    ylim(0, 1)

}

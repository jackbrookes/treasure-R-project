library(cowplot)

methods_fig <- plot_grid(
  ggdraw() + draw_image("diagrams/panels.png"),
  plot_grid(
    ggdraw() + draw_image("diagrams/original-tree.png"),
    ggdraw() + draw_image("diagrams/tree-treasure.png"),
    nrow = 2,
    labels = c("b\n   i", "\n   ii")
  ),
  ncol = 2,
  labels = c("a", "")
)

save_plot("figs/methods_plot.png", methods_fig, base_width = 8.5, base_height = 5, dpi = 600, type = "cairo")

library(cowplot)

# plots figures - i.e. combinations of graphs

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

save_plot(
  "figs/methods_plot.png",
  methods_fig,
  base_width = 8.5,
  base_height = 5,
  dpi = 600,
  type = "cairo"
  )

# results: 1

results1 <- plot_grid(
  selection_bias,
  reselect,
  nrow = 1,
  labels = letters,
  rel_widths = c(0.8, 2.0),
  axis = "tblr",
  align = "h"
)

save_plot(
  "figs/selection_and_reselection.png",
  results1, 
  type = "cairo",
  ncol = 2,
  base_height = 3, 
  base_width = 3.2,
  dpi = 600
)

# results: 2

results2 <- plot_grid(
  estimate_bias,
  estimate_corr,
  nrow = 1,
  labels = letters,
  rel_widths = c(0.8, 2.0),
  axis = "tblr",
  align = "h"
)

save_plot(
  "figs/estimate_bias_corr.png",
  results2, 
  type = "cairo",
  ncol = 2,
  base_height = 3, 
  base_width = 3.2,
  dpi = 600
)

# results: 3

model_g <- plot_grid(
  plot_grid(
    gen_graph,
    labels = c("a", ""),
    nrow = 2
  ),
  plot_grid(
    eta_graph + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
    sigma_graph + theme(plot.margin = unit(c(0.35, 0.2, 0.2, 0.2), "cm")),
    rel_widths = c(1.08, 1),
    labels = c("b", "c"),
    ncol = 2
  ),
  rel_widths = c(1, 1.5)
)


save_plot("figs/model.png", model_g, type = "cairo", nrow = 2, ncol = 2, base_height = 1.5, base_width = 3, dpi = 600)


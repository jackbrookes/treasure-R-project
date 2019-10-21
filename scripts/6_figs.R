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
  rel_widths = c(0.7, 2.0),
  axis = "tblr",
  align = "h"
)

save_plot(
  "figs/selection_and_reselection.png",
  results1, 
  type = "cairo",
  ncol = 2,
  base_height = 4, 
  base_width = 4.3,
  dpi = 600
)

# results: 2

results2 <- plot_grid(
  estimate_bias,
  estimate_corr,
  nrow = 1,
  labels = letters,
  rel_widths = c(0.7, 2.0),
  axis = "tblr",
  align = "h"
)

save_plot(
  "figs/estimate_bias_corr.png",
  results2, 
  type = "cairo",
  ncol = 2,
  base_height = 4, 
  base_width = 4.3,
  dpi = 600
)

# results: 3

model_g <- plot_grid(
  gen_graph,
  plot_grid(
    dens_graph("\u03B7 (Learning rate)", eta, eta_hdr95min, eta_hdr95max, eta_mean, "red") +
      scale_x_continuous(limits = c(0, 0.2), breaks = c(0, 0.1, 0.2)),
    dens_graph("\u03C3 (Standard deviation)", sigma, sigma_hdr95min, sigma_hdr95max, sigma_mean, "green") +
      scale_x_continuous(limits = c(0.2, 0.3), breaks = c(0.2, 0.25, 0.3)),
    ncol = 1,
    labels = c("b", "c")
  ),
  labels = c("a", ""),
  nrow = 1
)


save_plot("figs/model.png", model_g, type = "cairo", nrow = 2, ncol = 2, base_height = 1.5, base_width = 3, dpi = 600)


library(tidyverse)
library(cowplot) # needed for publication ready theme + plot_grid

# set theme

theme_set(
  cowplot::theme_cowplot(line_size = 0.25) +
    theme(axis.text.x = element_text(size = 11),
          panel.spacing = unit(0.2, 'inches'),
          strip.background = element_rect(fill = NA),
          strip.text = element_text(face = "bold", margin = margin(8, 8, 8, 8)))
)

# pre-prepare colour scales for consitancy

clrs_outcomes <- c(
  `Reward` = "#35ce02",
  `Chest error` = "#ed2715",
  `Cup error` = "#ba0bba"
)

clr_scale <- list(
  scale_discrete_manual(
    aesthetics = c("colour", "fill"),
    values = clrs_outcomes,
    guide = FALSE)
)

to_index <- function(x){
  as.numeric(factor(x, levels = unique(x)))
}

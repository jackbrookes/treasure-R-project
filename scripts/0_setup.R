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

#' function to darken chosen colours
darken <- function(color, factor = 1.4) {
  col <- col2rgb(color)
  col <- col / factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}

# pre-prepare colour scales for consitancy

clrs_outcomes <- c(
  `Reward` = "#35ce02",
  `Chest error` = "#ed2715",
  `Cup error` = "#ba0bba"
)

clrs_outcomes_dark <- clrs_outcomes %>% 
  map_chr(darken, factor = 2)
names(clrs_outcomes_dark) <- paste(names(clrs_outcomes_dark), "dark", sep = "_")

clr_scale <- list(
  scale_discrete_manual(
    aesthetics = c("colour", "fill"),
    values = c(clrs_outcomes, clrs_outcomes_dark),
    guide = FALSE)
)

to_index <- function(x){
  as.numeric(factor(x, levels = unique(x)))
}

# install.packages("hdrcde")
library(hdrcde)

calc_metrics <- function(x, h = .005) {
  hdr_95_region <- hdr(x, prob = 95, h = h)$hdr
  return(tibble(
    hdr_95_min = hdr_95_region[[1, 1]],
    hdr_95_max = hdr_95_region[[1, 2]],
    mean = mean(x)
  ))
}

####

fit <- readRDS("stan/generalisation_fit.RDS")

fit_samples <- rstan::extract(fit) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  select(-lp__)

params_summary <- fit_samples %>%
  summarise_all(~list(calc_metrics(.))) %>% 
  unnest(.sep = "_") %>% 
  mutate(g_hdr_95_max = min(1, g_hdr_95_max))

dens_graph <- function(name, param_var, hdr_95_min_var, hdr_95_max_var, mean_var, fill_clr) {
  param_var <- enquo(param_var)
  hdr_95_min_var <- enquo(hdr_95_min_var)
  hdr_95_max_var <- enquo(hdr_95_max_var)
  mean_var <- enquo(mean_var)
  
  ggplot(fit_samples) +
    geom_density(aes(x = !!param_var, y = ..scaled.. * 0.7), fill = fill_clr,
                 alpha = .5,
                 size = 0.2) +
    geom_point(aes(x = !!mean_var, y = 0.2),
               data = params_summary) +
    geom_errorbarh(aes(xmin = !!hdr_95_min_var, xmax = !!hdr_95_max_var, y = 0.2),
                   alpha = .5,
                   height = 0.3,
                   data = params_summary) +
    geom_text(aes(x = !!mean_var, y = 0.8, label = sprintf("%.3f", !!mean_var)),
              vjust = 0,
              data = params_summary,
              size = 2.5) +
    expand_limits(y = 1) +
    labs(x = NULL, title = name) +
    coord_cartesian(clip = 'off') +
    theme(line = element_line(size = 0.2), 
          plot.title = element_text(size = 9, face = "plain"),
          panel.spacing.y = unit(0.1, "cm"),
          axis.line = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
}

model_g <- cowplot::plot_grid(
  dens_graph("\u03B7 (Learning rate)", eta, eta_hdr_95_min, eta_hdr_95_max, eta_mean, "red") +
    scale_x_continuous(limits = c(0.01, 0.02), breaks = c(0.01, 0.015, 0.02)),
  dens_graph("g (Generalisation rate)", g, g_hdr_95_min, g_hdr_95_max, g_mean, "blue") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)),
  dens_graph("\u03C3 (Standard deviation)", sigma, sigma_hdr_95_min, sigma_hdr_95_max, sigma_mean, "green") +
    scale_x_continuous(limits = c(0.2, 0.3), breaks = c(0.2, 0.25, 0.3)),
  nrow = 1,
  labels = letters
)
  
save_plot("figs/model.png", model_g, type = "cairo", ncol = 3, base_height = 1.5, base_width = 2)





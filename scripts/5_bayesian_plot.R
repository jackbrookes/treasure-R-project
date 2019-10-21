# install.packages("hdrcde")
library(hdrcde)

# install.packages("ggridges")
library(ggridges)


calc_metrics <- function(x, h = .005) {
  hdr_95_region <- hdr(x, prob = 95, h = h)$hdr
  return(tibble(
    hdr95min = hdr_95_region[[1, 1]],
    hdr95max = hdr_95_region[[1, 2]],
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
  unnest(cols = c(eta, g.1, g.2, g.3, g.4, g.5, g.6, g.7, g.8, g.9, g.10, g.11, 
                  g.12, g.13, g.14, g.15, g.16, g.17, g.18, sigma), names_sep = "_")


gen_samples <- fit_samples %>% 
  select(starts_with("g.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "g.(.*)", 
               values_to = c("sample"))

gen_summary <- params_summary %>% 
  select(starts_with("g.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "g.(.*)_", 
               values_to = c("hdr95min", "hdr95max", "mean"))
  

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
                   alpha = 1,
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


pp_order <- arrange(gen_summary, -mean) %>% pull(pp_idx)

gen_graph <- ggplot(gen_samples, aes(x = sample, y = factor(pp_idx, levels = pp_order), fill = ..x..)) + 
  geom_density_ridges_gradient(bandwidth  = 0.01, scale = 2, size = 0.2) +
  scale_fill_gradient(guide = FALSE, low = "#1cd9ad", high = "#c01cd9") +
  labs(x = NULL, y = "Participant", title = "g (generalisation rate)") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + 
  scale_y_discrete(labels = NULL) + 
  coord_cartesian(clip = 'off') +
  theme(line = element_line(size = 0.2), 
        plot.title = element_text(size = 9, face = "plain"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9, face = "plain"),
        axis.text.y = element_blank())



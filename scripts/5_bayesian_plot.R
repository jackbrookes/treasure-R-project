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
  unnest(cols = c(eta.1, eta.2, eta.3, eta.4, eta.5, eta.6, eta.7, eta.8, eta.9, 
                  eta.10, eta.11, eta.12, eta.13, eta.14, eta.15, eta.16, eta.17, 
                  eta.18, g, sigma.1, sigma.2, sigma.3, sigma.4, sigma.5, sigma.6, 
                  sigma.7, sigma.8, sigma.9, sigma.10, sigma.11, sigma.12, 
                  sigma.13, sigma.14, sigma.15, sigma.16, sigma.17, sigma.18), names_sep = "_")


# gen

gen_graph <- ggplot(fit_samples) +
  geom_density(aes(x = g, y = ..scaled.. * 0.7), fill = "#eb60d8",
               alpha = .5,
               size = 0.2) +
  geom_point(aes(x = g_mean, y = 0.2),
             data = params_summary) +
  geom_errorbarh(aes(xmin = g_hdr95min, xmax = g_hdr95max, y = 0.2),
                 alpha = 1,
                 height = 0.3,
                 data = params_summary) +
  geom_text(aes(x = g_mean, y = 0.8, label = sprintf("%.3f", g_mean)),
            vjust = 0,
            data = params_summary,
            size = 2.5) +
  expand_limits(x = 0, y = 1) +
  labs(x = NULL, title = "g (generalisation rate)") +
  coord_cartesian(clip = 'off') +
  theme(line = element_line(size = 0.2), 
        plot.title = element_text(size = 9, face = "plain"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())




## eta

eta_samples <- fit_samples %>% 
  select(starts_with("eta.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "eta.(.*)", 
               values_to = c("sample"))

eta_summary <- params_summary %>% 
  select(starts_with("eta.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "eta.(.*)_", 
               values_to = c("hdr95min", "hdr95max", "mean"))

pp_order <- arrange(eta_summary, -mean) %>% pull(pp_idx)

eta_graph <- ggplot(eta_samples, aes(x = sample, y = factor(pp_idx, levels = pp_order), fill = ..x..)) + 
  geom_density_ridges_gradient(bandwidth  = 0.0002, scale = 2, size = 0.2) +
  scale_fill_gradient(guide = FALSE, low = "#1cd9ad", high = "#c01cd9") +
  labs(x = NULL, y = "Participant", title = "\u03B7 (Learning rate)") + 
  scale_y_discrete(labels = NULL) + 
  coord_cartesian(clip = 'off') +
  theme(line = element_line(size = 0.2), 
        plot.title = element_text(size = 9, face = "plain"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9, face = "plain"),
        axis.text.y = element_blank())



# sigma

sigma_samples <- fit_samples %>% 
  select(starts_with("sigma.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "sigma.(.*)", 
               values_to = c("sample"))

sigma_summary <- params_summary %>% 
  select(starts_with("sigma.")) %>% 
  pivot_longer(everything(),
               names_to = c("pp_idx"),
               names_pattern = "sigma.(.*)_", 
               values_to = c("hdr95min", "hdr95max", "mean"))

sigma_graph <- ggplot(sigma_samples, aes(x = sample, y = factor(pp_idx, levels = pp_order), fill = ..x..)) + 
  geom_density_ridges_gradient(bandwidth  = 0.0002, scale = 2, size = 0.2) +
  scale_fill_gradient(guide = FALSE, low = "#fccd3f", high = "#e64c1e") +
  labs(x = NULL, y = NULL, title = "\u03C3 (Standard deviation)") + 
  scale_y_discrete(labels = NULL) + 
  coord_cartesian(clip = 'off') +
  theme(line = element_line(size = 0.2), 
        plot.title = element_text(size = 9, face = "plain"),
        panel.spacing.y = unit(0.1, "cm"),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9, face = "plain"),
        axis.text.y = element_blank())


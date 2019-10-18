# selection bias ?
selection_bias <- ggplot(select_df, aes(x = "", y = selection_bias)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3) +
  geom_jitter(width = .1, size = 1, color = "grey60") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  annotate("text",
           x = "",
           y = c(1, -1),
           color = 'black',
           size = 3,
           label = c(paste0("italic(\"CupError+ pref.\")"),
                     paste0("italic(\"ChestError+ pref.\")")),
           parse = TRUE,
           hjust = 0.5) +
  labs(x = NULL, y = "Selection bias (p.p.)") + 
  expand_limits(y = c(0.75, -0.75)) +
  scale_y_continuous(labels = scales::percent) +
  ggsave("graphs/select.png", type = "cairo", height = 4, width = 2, dpi = 600)

# reselection vs outcome 
reselect <- ggplot(reselect_df, aes(x = prev_outcome, y = reselect_controlled, color = prev_outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3) +
  geom_jitter(width = .05) +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .1) +
  stat_summary(fun.y = mean, geom = "point", color = "black", size = 3) +
  labs(x = NULL, y = "Reselection preference (p.p.)") + 
  clr_scale + 
  expand_limits(y = c(0.5, -0.5)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(prev_chest)) + 
  ggsave("graphs/reselect.png", type = "cairo", height = 4, width = 6, dpi = 600)

# value estimates over time
value_ests <- ggplot(value_df, aes(x = trial_num, y = value_estimate, color = chest, fill = chest)) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = .1, color = NA) +
  stat_summary(fun.y = mean, geom = "line", size = .25) +
  labs(x = "Trial number", y = "Value estimate bias (%)", fill = "Chest", color = "Chest") + 
  expand_limits(y = c(0.5, -0.5)) +
  ggsave("graphs/value.png", type = "cairo", height = 4, width = 6, dpi = 600)

# high-level value estimates
ggplot(value_summary_df, aes(x = chest, y = mean_value_estimate, color = chest)) +
  geom_jitter(width = .05) +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .1) +
  stat_summary(fun.y = mean, geom = "point", color = "black", size = 3) +
  labs(x = "Chest", y = "Mean value estimate (%)", fill = "Chest", color = "Chest") + 
  ggsave("graphs/value_mean.png", type = "cairo", height = 4, width = 6, dpi = 600)

# are people's actions reflecting their high-level value estimates? 
ggplot(value_select_summary_df, aes(x = estimate_bias, y = selection_bias)) + 
  geom_point() + 
  stat_smooth(method = "lm", alpha = .2, color = "red") + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Mean value estimate bias (p.p.)", y = "Selection bias (p.p.)", fill = "Chest", color = "Chest") + 
  ggsave("graphs/value_probe_lm.png", type = "cairo", height = 4, width = 6, dpi = 600)


### figure outputs

save_plot(
  "figs/selection_and_reselection.png",
  plot_grid(
    selection_bias,
    reselect,
    nrow = 1,
    labels = letters,
    rel_widths = c(0.7, 2.0),
    axis = "tblr",
    align = "h"
  ), 
  type = "cairo",
  ncol = 2,
  base_height = 4, 
  base_width = 4.3,
  dpi = 600
)


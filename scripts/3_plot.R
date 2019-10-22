library(ggforce)

# selection bias ?
selection_bias <- ggplot(select_df, aes(x = "", y = selection_bias)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .25) +
  geom_sina(maxwidth = .4, size = .8, shape = 4, position = position_nudge(x = .2)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .3, size = .25, position = position_nudge(x = -.2)) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 21, color = "black", stroke = .25, fill = "white", position = position_nudge(x = -.2)) +
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
reselect <- ggplot(reselect_df, aes(x = prev_outcome,
                                    y = reselect_controlled,
                                    color = paste0(prev_outcome, "_dark"),
                                    fill = prev_outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .4, size = .25) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 21, color = "black", stroke = .25) +
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
estimate_bias <- ggplot(value_select_summary_df, aes(x = "", y = estimate_bias )) +
  geom_hline(yintercept = 0, linetype = "dashed", size = .25) +
  geom_sina(maxwidth = .4, size = .8, shape = 4, position = position_nudge(x = .2)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .3, size = .25, position = position_nudge(x = -.2)) +
  stat_summary(fun.y = mean, geom = "point", size = 2, shape = 21, color = "black", stroke = .25, fill = "white", position = position_nudge(x = -.2)) +
  annotate("text",
           x = "",
           y = c(1, -1),
           color = 'black',
           size = 3,
           label = c(paste0("italic(\"CupError+ pref.\")"),
                     paste0("italic(\"ChestError+ pref.\")")),
           parse = TRUE,
           hjust = 0.5) +
  labs(x = NULL, y = "Estimate bias (p.p.)") + 
  expand_limits(y = c(0.75, -0.75)) +
  scale_y_continuous(labels = scales::percent) +
  ggsave("graphs/probe_bias.png", type = "cairo", height = 4, width = 2, dpi = 600)

# are people's actions reflecting their high-level value estimates? 
estimate_corr <- ggplot(value_select_summary_df, aes(x = estimate_bias, y = selection_bias)) + 
  geom_point() + 
  stat_smooth(method = "lm", alpha = .15, color = "#079eb5", fill = "#98e3e2") + 
  annotate("text", x = -0.2, y = 0.6, parse = TRUE, hjust = 0,
           label = paste0("italic(y) == ",
                         signif(est_sel_fit$coef[[2]], 3),
                         "~italic(x)~",
                         ifelse(sign(est_sel_fit$coef[[1]]) == 1, '+', '-'),
                         "~",
                         signif(abs(est_sel_fit$coef[[1]]), 2),
                         "~\";\"~Adj.~italic(R) ^ 2 == ",
                         signif(summary(est_sel_fit)$adj.r.squared, 2),
                         "~\";\"~italic(p) == ",
                         signif(summary(est_sel_fit)$coef[2,4], 3))) + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Mean value estimate bias (p.p.)", y = "Selection bias (p.p.)", fill = "Chest", color = "Chest") + 
  ggsave("graphs/value_probe_lm.png", type = "cairo", height = 4, width = 6, dpi = 600)

# pearsons r
ggplot(b30_corr, aes(x = block_30, y = r)) + 
  geom_line() +
  geom_point() + 
  labs(x = "Probe number", y = "Pearson's r\n(Estimate bias vs selection bias)") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  expand_limits(y = 0) +
  ggsave("graphs/value_probe_pearsons.png", type = "cairo", height = 4, width = 6, dpi = 600)


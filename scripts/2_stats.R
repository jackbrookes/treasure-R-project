# install.packages("effsize")

# is there a bias in selection of either target?
t.test(select_df$selection_bias)
# cohen's d effect size
with(select_df, mean(selection_bias) / sd(selection_bias))

# can reselection rate be predicted by previous outcome / selected chest ?
# we use `Error(ppid)` to tell R that the error per participant. (this is a within subjects design)
aov(reselect_controlled ~ prev_outcome * prev_chest + Error(ppid), reselect_df) %>% 
  summary()

reselect_pairwise <- with(reselect_df,
                          pairwise.t.test(reselect_controlled, prev_outcome, paired = TRUE))
reselect_pairwise %>% broom::tidy() %>%
  mutate(`*` = ifelse(p.value <= 0.05, '*', '-'))


# is there a bias in probe of either target?
t.test(value_select_summary_df$estimate_bias)
t.test(value_select_summary_df$`CupError+_est`, value_select_summary_df$`ChestError+_est`, paired = TRUE)
# cohen's d effect size
with(value_select_summary_df, mean(estimate_bias) / sd(estimate_bias))


# do individual estimates of value predict selection bias?
est_sel_fit <- lm(selection_bias ~ estimate_bias, data = value_select_summary_df)

est_sel_fit %>% summary()
  
# install.packages("BayesFactor")
library(BayesFactor)


# can reselection rate be predicted by previous outcome / selected chest ?
# we use `Error(ppid)` to tell R that the error per participant. (this is a within subjects design)
aov(reselect_controlled ~ prev_outcome * prev_chest + Error(ppid), reselect_df) %>% 
  summary()

anovaBF(reselect_controlled ~ prev_outcome * selected_chest + Error(ppid), reselect_df)

# is there a bias in selection of either target?
t.test(select_df$selection_bias)
ttestBF(select_df$selection_bias)

# is there a bias in probe of either target?
t.test(value_select_summary_df$estimate_bias)

# do individual estimates of value predict selection bias?
lm(estimate_bias ~ selection_bias, data = value_select_summary_df) %>% 
  summary()
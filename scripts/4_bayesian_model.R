library(rstan)
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

model_df <- trials_df %>% 
  left_join(value_df %>%
              spread(chest, value_estimate) %>% 
              transmute(
                ppid,
                cuperror_ve = `CupError+`,
                chesterror_ve = `ChestError+`,
                trial_num,
              ),
            by = c("ppid", "trial_num")) %>% 
  mutate(cuperror_ve = zoo::na.approx(cuperror_ve, na.rm = FALSE),
         chesterror_ve = zoo::na.approx(chesterror_ve, na.rm = FALSE),
         cuperror_ve = ifelse(is.na(cuperror_ve), 0, cuperror_ve), 
         chesterror_ve = ifelse(is.na(chesterror_ve), 0, chesterror_ve),# stan doesnt support NAs
         c1_selection = zoo::rollmean(selected_chest == "CupError+", 30, fill = 0, align = 'right'),
         c2_selection = zoo::rollmean(selected_chest == "ChestError+", 30, fill = 0, align = 'right')) 


input_data <- model_df %>% 
  with(., list(
    N = nrow(.),
    trial_idx = trial_num,
    y = to_index(selected_chest),
    value_probe = cbind(cuperror_ve, chesterror_ve),
    # value_probe = cbind(c1_selection, c2_selection),
    k = as.integer(stage1 == "correct"),
    r = as.integer(stage2 == "open_reward"),
    initial_est = c(0.5, 0.5)
  ))
  

fit <- stan(file = "stan/generalisation.stan",
            data = input_data,
            iter = 2000,
            chains = 1,
            cores = 1,
            pars = c("eta", "g", "sigma"),
            control = list(adapt_delta = 0.999),
            diagnostic_file = "stan_diagnostics.txt") 

saveRDS(fit, "stan/generalisation_fit.RDS")
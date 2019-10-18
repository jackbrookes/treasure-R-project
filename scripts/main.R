# assumes data is stored in /data folder
source("scripts/0_setup.R")
source("scripts/1_preprocess.R")
source("scripts/2_plot.R")
source("scripts/3_stats.R")

# this will take a while, not neccessary to run
source("scripts/4_bayesian_model.R")

# if bayesian_model.R has not been run, reads latest model fit from disk 
source("scripts/5_bayesian_plot.R")

# generates figures for papeer
source("scripts/6_methods_figs.R")
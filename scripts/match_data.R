# Which Peacekeepers Keep the Peace? # 
# Sky Kunkel #
# 1/16/2023 #

#install.packages('C:/gurobi952/win64/R/gurobi_9.5-2.zip', repos=NULL)
library(gurobi); library(designmatch); library(gdata); library(tidyverse)

# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
#### Matching by where units with more and less women deployed ####
a = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(date >= "2005-09-01") %>%
  filter(t_ind == 1)

# match by where women and where only men deployed
a = a[order(a$t_bal, decreasing=TRUE), ]

control.variables = cbind(a$prio_mountains_mean, a$prio_ttime_mean, a$prio_urban_gc, a$prio_nlights_calib_mean, 
                          a$prio_pop_gpw_sum, a$prio_pop.dens, a$prec_gpcp, a$acled_viol_6)
t_ind = a$t_bal #treatment
t_id = which(t_ind==1) #treated
c_id = which(t_ind==0) #control
tab1 = meantab(control.variables, t_ind, t_id, c_id)
tab1

mom_covs = cbind(a$prio_mountains_mean, a$prio_ttime_mean, a$prio_urban_gc, a$prio_nlights_calib_mean, 
                 a$prio_pop_gpw_sum, a$prio_pop.dens, a$prec_gpcp, a$acled_viol_6)

for(i in 1:ncol(mom_covs)){
  mom_covs[is.na(mom_covs[,i]), i] <- mean(mom_covs[,i], na.rm = TRUE)
}

# define observed covariates for the matching

mom_tols = absstddif(mom_covs, t_ind, .1) # defining the tolerance of balance (0.1)
mom = list(covs = mom_covs, tols = mom_tols, targets = NULL) # merging the covariates and the tolerance

# Solver options
t_max = 60*5
solver = "gurobi"
approximate = 0
solver = list(name = solver, t_max = t_max, approximate = approximate,
              round_cplex = 0, trace = 1)

# Match
out_1 = cardmatch(t_ind, mom = mom, solver = solver) 

# Indices of the treated units and matched controls
t_id_1 = out_1$t_id
c_id_1 = out_1$c_id

# Standardized before matching

tab1
# Standardized after matching
covs = cbind(a$prio_mountains_mean, a$prio_ttime_mean, a$prio_urban_gc, a$prio_nlights_calib_mean, 
             a$prio_pop_gpw_sum, a$prio_pop.dens, a$prec_gpcp, a$acled_viol_6)
tab2 = meantab(covs, t_ind, t_id_1, c_id_1)
tab2
# Save matched sample 
b = a[c(t_id_1, c_id_1), ]
b$radpko_pko_lag_any = 0
b$radpko_pko_lag_any[b$radpko_pko_lag > 0] = 1

saveRDS(b, "./data/kunkel_wpks_matched_gender.rds")
rm(list = ls())
gc()



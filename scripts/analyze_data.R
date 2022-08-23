# Conflict Grids & Peacekeeping #
# By: Sky Kunkel

#### load libraries, read data ####
library(tidygeocoder)
library(tidyverse); library(viridis)
library(gdata); library(designmatch) 
library(magrittr)

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(gurobi); library(MASS); library(lme4); library(vtable)
library(sensitivitymw); library(lmtest); library(sandwich); library(glmmTMB)
library(ggeffects)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
a = readRDS("./data/kunkel_cg.rds")

##########################################################
### Unmatched Logit Models, no distinction between actors ###
##########################################################

#### Logit Model, continuous treatment####
# naive model
reg1 = glm.nb(event.b ~ pko_deployed, data = a)
summary(reg1)

reg2 = glm.nb(death ~ pko_deployed, data = a)
summary(reg2)

# model with controls #
reg3 = glm.nb(event.b ~ pko_deployed + mountains_mean + ttime_mean + urban_gc + 
                  nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
                pko_deployed*pko_lag + pko_deployed*viol_6,
                data = a)
summary(reg3)

reg4 = glm.nb(death ~ pko_deployed + mountains_mean + ttime_mean + pop_gpw_sum + 
                  pop.dens + pko_lag + viol_6 + pko_deployed*pko_lag + pko_deployed*viol_6, 
                data = a)
summary(reg4)
# won't run 

#### Logit Model, binary treatment####
# naive treatment model without controls
reg5 = glm.nb(event.b ~ t_ind, data = a)
summary(reg5)

reg6 = glm.nb(death ~ t_ind, data = a)
summary(reg6)

# naive treatment model with controls #
reg7 = glm.nb(event.b ~ t_ind + mountains_mean + ttime_mean + urban_gc + nlights_calib_mean + 
                  pop_gpw_sum + pop.dens + pko_lag + viol_6 + t_ind*pko_lag + t_ind*viol_6,
                data = a)
summary(reg7)

reg8 = glm.nb(death ~ t_ind + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + 
                  pko_lag + viol_6 + t_ind*pko_lag + t_ind*viol_6, 
                data = a)
summary(reg8)

# treatment by gender with controls #
reg9 = glm.nb(event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
                data = a)
summary(reg9)

reg10 = glm.nb(death ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6, 
                data = a)
summary(reg10)

# treatment by PK type with controls #
reg11 = glm.nb(event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = a)
summary(reg11)

reg12 = glm.nb(death ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6, 
               data = a)
summary(reg12)


##########################################################
 ### Unmatched Logit Models, disaggregated by actors ###
##########################################################

#### GOV OSV - Continuous treatment ####
reg13 = glm.nb(gov_event.b ~ pko_deployed + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
                  pko_deployed*pko_lag + pko_deployed*viol_6,
                data = a)
summary(reg13)

reg14 = glm.nb(gov_death.b ~ pko_deployed + mountains_mean + ttime_mean + pop_gpw_sum +
               pop.dens + pko_lag + viol_6 +
               pko_deployed*pko_lag + pko_deployed*viol_6,
                 data = a)
summary(reg14)

#### REB OSV - Continuous Treatment ####
reg15 = glm.nb(reb_event.b ~ pko_deployed + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
                 pko_deployed*pko_lag + pko_deployed*viol_6,
                 data = a)
summary(reg15)

reg16 = glm.nb(reb_death.b ~ pko_deployed + mountains_mean + ttime_mean + pop_gpw_sum +
                 pop.dens + pko_lag + viol_6 +
                 pko_deployed*pko_lag + pko_deployed*viol_6,
                 data = a)
summary(reg16)

#### GOV OSV - Naive Binary treatment ####
reg17 = glm.nb(gov_event.b ~ t_ind + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag +
                 t_ind*pko_lag + t_ind*viol_6,
               data = a)
summary(reg17)

reg18 = glm.nb(gov_death.b ~ t_ind + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag +
                 t_ind*pko_lag + t_ind*viol_6,
               data = a)
summary(reg18)

#### REB OSV - Naive Binary Treatment ####
reg19 = glm.nb(reb_event.b ~ t_ind + untrp + unpol + unmob + f_untrp.p +
                   f_unpol.p + f_unmob.p + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(reg19)

reg20 = glm.nb(reb_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
                 data = a)
summary(reg20)

#### GOV OSV - Binary treatment by gender ####
reg21 = glm.nb(gov_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = a)
summary(reg21)
se_reg_c1 <- round(coeftest(reg21, vcov = vcovPL(reg21, cluster = a$prio.grid)),4)
se_reg_c1

reg22 = glm.nb(gov_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = a)
summary(reg22)
se_reg_c2 <- round(coeftest(reg22, vcov = vcovPL(reg22, cluster = a$prio.grid)),4)
se_reg_c2
#### REB OSV - Binary treatment by gender ####
reg23 = glm.nb(reb_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = a)
summary(reg23)
se_reg_c3 <- round(coeftest(reg23, vcov = vcovPL(reg23, cluster = a$prio.grid)),4)
se_reg_c3

reg24 = glm.nb(reb_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = a)
summary(reg24)
se_reg_c4 <- round(coeftest(reg24, vcov = vcovPL(reg24, cluster = a$prio.grid)),4)
se_reg_c4

#### GOV OSV - Binary treatment by PK Type ####
reg25 = glm.nb(gov_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = a)
summary(reg25)
se_reg_c5 <- round(coeftest(reg25, vcov = vcovPL(reg25, cluster = a$prio.grid)),4)
se_reg_c5

reg26 = glm.nb(gov_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6, 
               data = a)
summary(reg26)
se_reg_c6 <- round(coeftest(reg26, vcov = vcovPL(reg26, cluster = a$prio.grid)),4)
se_reg_c6

#### REB OSV - Binary treatment by PK Type ####
reg27 = glm.nb(reb_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = a)
summary(reg27)
se_reg_c7 <- round(coeftest(reg27, vcov = vcovPL(reg27, cluster = a$prio.grid)),4)
se_reg_c7

reg28 = glm.nb(reb_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = a)
summary(reg28)
se_reg_c8 <- round(coeftest(reg28, vcov = vcovPL(reg28, cluster = a$prio.grid)),4)
se_reg_c8

# Save Standard Errors to objects for use in table
reg21se = se_reg_c1[,2]
reg22se = se_reg_c2[,2]
reg23se = se_reg_c3[,2]
reg24se = se_reg_c4[,2]
reg25se = se_reg_c5[,2]
reg26se = se_reg_c6[,2]
reg27se = se_reg_c7[,2]
reg28se = se_reg_c8[,2]

# Save P-values from robust clustering outputs for use in table
reg21p = se_reg_c1[,4]
reg22p = se_reg_c2[,4]
reg23p = se_reg_c3[,4]
reg24p = se_reg_c4[,4]
reg25p = se_reg_c5[,4]
reg26p = se_reg_c6[,4]
reg27p = se_reg_c7[,4]
reg28p = se_reg_c8[,4]

#### Figures and Plots for non-matched regressions ####

## logit outputs ##
# unmatched pk effectiveness by pk gender #
stargazer(reg21, reg22, reg23, reg24, title = "Pre-matched Results Pr(Violence) by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.",
          out = "./results/pre_matched_gender.txt")

# unmatched pk effectiveness by pk type #
stargazer(reg25, reg26, reg27, reg28, title = "Pre-matched Results Pr(Violence) by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.",
          out = "./results/pre_matched_troop.txt")

## Odds ratios outputs ##
stargazer(reg21, reg22, reg23, reg24, title = "Pre-matched Results Pr(Violence) by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          apply.coef = exp, t.auto=F, p.auto=F,
          out = "./results/pre_matched_gender_or.txt")

# unmatched pk effectiveness by pk type #
stargazer(reg25, reg26, reg27, reg28, title = "Pre-matched Results Pr(Violence) by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          apply.coef = exp, t.auto=F, p.auto=F,
          out = "./results/pre_matched_troop_or.txt")

# descriptive statistics table #
labs = c("Total PKs deployed", "Gender Balanced Units", "Gender Un-Balanced Units",
         "Majority Troop Units", "Majority Police Units", "Majority Observer Units")

stargazer(a[c("pko_deployed", "t_bal", "t_unbal", "untrp_maj", "unpol_maj", "unmob_maj")], covariate.labels = labs, digits = 3, 
          style = "ajps", omit.summary.stat = "n",
          title = "Treatments Summarized by Grid-month observations",
          out = "./results/pks_table.txt")

stargazer(a[c("event", "death", "gov_event.b", "reb_event.b","gov_death.b", "reb_event.b")], 
          covariate.labels = c("Violent Events", "Deaths", "Gov. Event", "Reb. Event", "Gov. Death", "Reb. Death"), digits = 3, 
          style = "ajps", omit.summary.stat = "n",
          title = "Outcomes Summarized by Grid-month observations",
          out = "./results/violence_table.txt")

rm(list = setdiff(ls(), "a")) 
gc()

##########################################
        ### Matching Analysis ###
##########################################

a = a[order(a$t_ind, decreasing=TRUE), ]

control.variables = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                          a$pop_gpw_sum, a$pop.dens, a$prec_gpcp, a$viol_6)
t_ind = a$t_ind #treatment
t_id = which(t_ind==1) #treated
c_id = which(t_ind==0) #control
tab1 = meantab(control.variables, t_ind, t_id, c_id)
tab1
# keep map stuff at the end of data.frame #
a = a %>% relocate(c("xcoord", "ycoord", "col", "row", "geometry"), .after = last_col())

mom_covs = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                 a$pop_gpw_sum, a$pop.dens, a$prec_gpcp, a$viol_6)

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
covs = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                        a$pop_gpw_sum, a$pop.dens, a$prec_gpcp, a$viol_6)
tab2 = meantab(covs, t_ind, t_id_1, c_id_1)
tab2
# Save matched sample 
b = a[c(t_id_1, c_id_1), ]

# number of observations before and after matching 

table((a$t_ind))
table(table(t_id_1))
table(table(c_id_1))


# Sensitivity tests
# gamma = 1
test_d_match1 = data.frame(b$gov_death.b[b$t_ind==1],b$gov_death.b[b$t_ind==0])
colnames(test_d_match1) = c("treated","control")

senmw(test_d_match1,gamma=1,method="t")$pval

test_d_match2 = data.frame(b$reb_death.b[b$t_ind==1],b$reb_death.b[b$t_ind==0])
colnames(test_d_match2) = c("treated","control")

senmw(test_d_match2,gamma=1,method="t")$pval

rm(list = setdiff(ls(), "b")) 
gc()
saveRDS(b, "./data/kunkel_cg_matched.RDS")



#### Negative binomial regression with matched sample and clustered standard errors ####
b = readRDS("./data/kunkel_cg_matched.RDS")
#### GOV OSV - Binary treatment by gender ####
reg1 = glm.nb(gov_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = b)
summary(reg1)
se_reg_c1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = b$prio.grid)),4)
se_reg_c1

reg2 = glm.nb(gov_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
               data = b)
summary(reg2)
se_reg_c2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = b$prio.grid)),4)
se_reg_c2

#### REB OSV - Binary treatment by gender ####
reg3 = glm.nb(reb_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6, data = b)
summary(reg3)
se_reg_c3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = b$prio.grid)),4)
se_reg_c3

reg4 = glm.nb(reb_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                 pko_lag + viol_6 +
                 t_bal*pko_lag + t_unbal*pko_lag +
                 t_bal*viol_6 + t_unbal*viol_6,
                 data = b)
summary(reg4)
se_reg_c4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = b$prio.grid)),4)
se_reg_c4

#### GOV OSV - Binary treatment by PK Type ####
reg5 = glm.nb(gov_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = b)
summary(reg5)
se_reg_c5 <- round(coeftest(reg5, vcov = vcovPL(reg5, cluster = b$prio.grid)),4)
se_reg_c5

reg6 = glm.nb(gov_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6, 
               data = b)
summary(reg6)
se_reg_c6 <- round(coeftest(reg6, vcov = vcovPL(reg6, cluster = b$prio.grid)),4)
se_reg_c6

#### REB OSV - Binary treatment by PK Type ####
reg7 = glm.nb(reb_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = b)
summary(reg7)
se_reg_c7 <- round(coeftest(reg7, vcov = vcovPL(reg7, cluster = b$prio.grid)),4)
se_reg_c7

reg8 = glm.nb(reb_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                 pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                 untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                 untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
               data = b)
summary(reg8)
se_reg_c8 <- round(coeftest(reg8, vcov = vcovPL(reg8, cluster = b$prio.grid)),4)
se_reg_c8

#### Matched Figures ####
# pk effectiveness by pk gender #
# Save Standard Errors to objects for use in table
reg21se = se_reg_c1[,2]
reg22se = se_reg_c2[,2]
reg23se = se_reg_c3[,2]
reg24se = se_reg_c4[,2]
reg25se = se_reg_c5[,2]
reg26se = se_reg_c6[,2]
reg27se = se_reg_c7[,2]
reg28se = se_reg_c8[,2]


### Exponentiate coeffecients and standard errors here??


# Save P-values from robust clustering outputs for use in table
reg21p = se_reg_c1[,4]
reg22p = se_reg_c2[,4]
reg23p = se_reg_c3[,4]
reg24p = se_reg_c4[,4]
reg25p = se_reg_c5[,4]
reg26p = se_reg_c6[,4]
reg27p = se_reg_c7[,4]
reg28p = se_reg_c8[,4]

#### Figures and Plots for non-matched regressions ####

## logit outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Pr(Violence) by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.",
          out = "./results/matched_gender.txt")

# matched pk effectiveness by pk type #
stargazer(reg5, reg6, reg7, reg8, title = "Matched Results Pr(Violence) by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          out = "./results/matched_troop.txt")


## odds ratios outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Pr(Violence) by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          apply.coef = exp, t.auto=F, p.auto=F,
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          out = "./results/matched_gender_or.txt")

stargazer(reg5, reg6, reg7, reg8, title = "Matched Results Pr(Violence) by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          apply.coef = exp, t.auto=F, p.auto=F,
          out = "./results/matched_troop_or.txt")



#### Continuous GOV OSV - Binary treatment by gender ####
rm(list = setdiff(ls(), "b")) 

reg1 = glm.nb(gov_event ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
              data = b)
summary(reg1)
se_reg_c1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = b$prio.grid)),4)
se_reg_c1

reg2 = glm.nb(gov_death ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                pko_lag + viol_6 +
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
              data = b)
summary(reg2)
se_reg_c2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = b$prio.grid)),4)
se_reg_c2

#### REB OSV - Binary treatment by gender ####
reg3 = glm.nb(reb_event ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6, data = b)
summary(reg3)
se_reg_c3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = b$prio.grid)),4)
se_reg_c3

reg4 = glm.nb(reb_death ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                pko_lag + viol_6 +
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
              data = b)
summary(reg4)
se_reg_c4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = b$prio.grid)),4)
se_reg_c4

#### GOV OSV - Binary treatment by PK Type ####
reg5 = glm.nb(gov_event ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = b)
summary(reg5)
se_reg_c5 <- round(coeftest(reg5, vcov = vcovPL(reg5, cluster = b$prio.grid)),4)
se_reg_c5

reg6 = glm.nb(gov_death ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6, 
              data = b)
summary(reg6)
se_reg_c6 <- round(coeftest(reg6, vcov = vcovPL(reg6, cluster = b$prio.grid)),4)
se_reg_c6

#### REB OSV - Binary treatment by PK Type ####
reg7 = glm.nb(reb_event ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = b)
summary(reg7)
se_reg_c7 <- round(coeftest(reg7, vcov = vcovPL(reg7, cluster = b$prio.grid)),4)
se_reg_c7

reg8 = glm.nb(reb_death ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = b)
summary(reg8)
se_reg_c8 <- round(coeftest(reg8, vcov = vcovPL(reg8, cluster = b$prio.grid)),4)
se_reg_c8

#### Matched Figures ####
# Save Standard Errors to objects for use in table
reg21se = se_reg_c1[,2]
reg22se = se_reg_c2[,2]
reg23se = se_reg_c3[,2]
reg24se = se_reg_c4[,2]
reg25se = se_reg_c5[,2]
reg26se = se_reg_c6[,2]
reg27se = se_reg_c7[,2]
reg28se = se_reg_c8[,2]

# Save P-values from robust clustering outputs for use in table
reg21p = se_reg_c1[,4]
reg22p = se_reg_c2[,4]
reg23p = se_reg_c3[,4]
reg24p = se_reg_c4[,4]
reg25p = se_reg_c5[,4]
reg26p = se_reg_c6[,4]
reg27p = se_reg_c7[,4]
reg28p = se_reg_c8[,4]

## logit outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Violence by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Count Outcome", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.",
          out = "./results/matched_gender_c.txt")

# matched pk effectiveness by pk type #
stargazer(reg5, reg6, reg7, reg8, title = "Matched Results Violence by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Count Outcome", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          out = "./results/matched_troop_c.txt")


## odds ratios outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Violence by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Count Outcome", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:pko_lag", "t_unbal:pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          apply.coef = exp, t.auto=F, p.auto=F,
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          out = "./results/matched_gender_or_c.txt")

stargazer(reg5, reg6, reg7, reg8, title = "Matched Results Violence by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Pr()", 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:pko_lag", "unpol_maj:pko_lag", "unmob_maj:pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"),
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          apply.coef = exp, t.auto=F, p.auto=F,
          out = "./results/matched_troop_or_c.txt")



# marginal effects #
reg1.bal = ggpredict(reg1, terms = "t_bal")
reg1.bal$group = "Gender Balanced"
reg1.unbal = ggpredict(reg1, terms = "t_unbal")
reg1.unbal$group = "Gender unbalanced"
reg1_gg = rbind(reg1.bal, reg1.unbal)

ggplot(reg1_gg) +
  geom_line(aes(x, predicted, colour = factor(group))) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = factor(group), 
                  fill = factor(group)), linetype = "dashed", alpha = 0.15) +
  ylab("Predicted Violence Events") + theme_pubclean() + 
  guides(fill=guide_legend(title="New Legend Title"))
  scale_x_continuous(breaks = seq(0,1,1)) + 
  


# descriptive statistics table #
labs = c("Total PKs deployed", "Gender Balanced Units", "Gender Un-Balanced Units",
         "Majority Troop Units", "Majority Police Units", "Majority Observer Units")

stargazer(b[c("pko_deployed", "t_bal", "t_unbal", "untrp_maj", "unpol_maj", "unmob_maj")], covariate.labels = labs, digits = 3, 
          style = "ajps", omit.summary.stat = "n",
          title = "Treatments Summarized by Grid-month observations",
          out = "./results/m_pks_table.txt")

stargazer(b[c("event", "death", "gov_event.b", "reb_event.b","gov_death.b", "reb_event.b")], 
          covariate.labels = c("Violent Events", "Deaths", "Gov. Event", "Reb. Event", "Gov. Death", "Reb. Death"), digits = 3, 
          style = "ajps", omit.summary.stat = "n",
          title = "Outcomes Summarized by Grid-month observations",
          out = "./results/m_violence_table.txt")



#### MLM by PK Gender ####

# Gov OSV #
ran.int1 = glmer.nb(gov_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                      nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                      t_bal*pko_lag + t_unbal*pko_lag +
                      t_bal*viol_6 + t_unbal*viol_6 +
                      (1 | ccode), data = b, family = nbinom2(link = "logit"), 
                    method="detect_separation")
summary(ran.int1)

ran.int2 = glmmTMB(gov_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + 
                      pop.dens + pko_lag + viol_6 +
                      t_bal*pko_lag + t_unbal*pko_lag +
                      t_bal*viol_6 + t_unbal*viol_6 +
                   (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int2)


# Rebel OSV #
ran.int3 = glmmTMB(reb_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
                      nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                      t_bal*pko_lag + t_unbal*pko_lag +
                      t_bal*viol_6 + t_unbal*viol_6 +
                   (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int3)

ran.int4 = glmmTMB(reb_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                      pko_lag + viol_6 +
                      t_bal*pko_lag + t_unbal*pko_lag +
                      t_bal*viol_6 + t_unbal*viol_6 +
                   (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int4)


stargazer(ran.int1, ran.int2, ran.int3, ran.int4, title = "MLM Matched, PKs by Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_b.txt")

#### MLM by PK Type ####
# Gov OSV #
ran.int5 = glmmTMB(gov_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                     urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                     untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                     untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6 +
                     (1 | ccode), data = b, family = nbinom2(link = "logit"),
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))
summary(ran.int5)

ran.int6 = glmmTMB(gov_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                     pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                     untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                     untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6 +
                     (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int6)


# Rebel OSV #
ran.int7 = glmmTMB(reb_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                     urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                     untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                     untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6 +
                     (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int7)

ran.int8 = glmmTMB(reb_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                     pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                     untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                     untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6 +
                     (1 | ccode), data = b, family = nbinom2(link = "logit"))
summary(ran.int8)


stargazer(ran.int5, ran.int6, ran.int7, ran.int8, title = "MLM Matched, PKs by Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_b.txt")

#########################
# Robustness Checks #
#########################

#### Re-testing Fjelde et al. (2019) DV w/ my data ####
a = readRDS("./data/kunkel_cg.rds")
# Gov OSV #
reg1 = glm.nb(gov_event.5 ~ units_deployed + untrp + unpol + unmob + f_untrp.p +
                    f_unpol.p + f_unmob.p + mountains_mean + ttime_mean + 
                    urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
              data = a)
summary(reg1)

reg2 = glm.nb(gov_death.5 ~ units_deployed + untrp + unpol + unmob + f_untrp.p +
                     f_unpol.p + f_unmob.p + mountains_mean + ttime_mean + pop_gpw_sum + 
                     pop.dens + pko_lag, data = a)
summary(reg2)

# Rebel OSV #
reg3 = glm.nb(reb_event.5 ~ units_deployed + untrp + unpol + unmob + f_untrp.p +
                     f_unpol.p + f_unmob.p + mountains_mean + ttime_mean + 
                     urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
              data = a)
summary(reg3) # reg3 doesn't run, likely not enough data

reg4 = glm.nb(reb_death.5 ~ units_deployed + untrp + unpol + unmob + f_untrp.p +
                     f_unpol.p + f_unmob.p + mountains_mean + ttime_mean + pop_gpw_sum + 
                     pop.dens + pko_lag, data = a)
summary(reg4)

stargazer(reg1, reg2, reg3, reg4, title = "Pre-matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/pre_matched_logit_5.txt")


#### Measuring Spatial Autocorrelation ####
# Spatial Durbin Model #

b.sf = st_as_sf(b)
nb = poly2nb(b.sf, queen = TRUE, row.names = b.sf$prio.grid)

 # store as list (most modeling packages require this)
 lw = nb2listw(nb, style = "W", zero.policy = TRUE)
 print(lw, zero.policy = TRUE) ## to look at lw contents

library(spatialreg)
 ### summarize by prio grid first ###
 #https://r-spatial.github.io/spatialreg/reference/SLX.html
 sp.durb1 = lmSLX(logit17, data = b, listw = lw)
 summary(sp.durb1)
 # Spatial durbin model says the DV is a function of three things:
   # neighbor DV values
   # our own IV values
   # neighbor IV values

 moran.test(prio.sp$pop_gpw_sum, listw = lw, zero.policy = TRUE) # Moran's I (eye) test
 # the closer the result is to 1, the more spatial dependence there is
 # for more info on this, see here: https://www.youtube.com/watch?v=6qZgchGCMds&ab_channel=BurkeyAcademy
   # and here: https://sites.google.com/site/econometricsacademy/econometrics-models/spatial-econometrics



#### Chi-Square Tests of model fit ####

anova(logit3, logit7.1, test = "Chisq")


#### Fjelde et al. DV ####
 
 # Gov OSV #
 logit21 = glm.nb(gov_event.5 ~ t_ind + untrp + unpol + unmob + f_untrp.p +
                    f_unpol.p + f_unmob.p + pko_lag + mountains_mean + ttime_mean + 
                    urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
 summary(logit21) 
 se_reg_c5 <- round(coeftest(logit21, vcov = vcovPL(logit21, cluster = b$prio.grid)),4)
 se_reg_c5
 
 logit22 = glm.nb(gov_death.5 ~ t_ind + untrp + unpol + unmob + f_untrp.p +
                    f_unpol.p + f_unmob.p + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                  data = b)
 summary(logit22)
 se_reg_c6 <- round(coeftest(logit22, vcov = vcovPL(logit22, cluster = b$prio.grid)),4)
 se_reg_c6
 
 
 # Rebel OSV #
 logit23 = glm.nb(reb_event.5 ~ t_ind + untrp + unpol + unmob + f_untrp.p +
                    f_unpol.p + f_unmob.p + pko_lag + mountains_mean + ttime_mean + 
                    urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
 summary(logit23) 
 se_reg_c7 <- round(coeftest(logit23, vcov = vcovPL(logit23, cluster = b$prio.grid)),4)
 se_reg_c7
 
 logit24 = glm.nb(reb_death.5 ~ t_ind + untrp + unpol + unmob + f_untrp.p +
                    f_unpol.p + f_unmob.p + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                  data = b)
 summary(logit24)
 se_reg_c8 <- round(coeftest(logit24, vcov = vcovPL(logit24, cluster = b$prio.grid)),4)
 se_reg_c8
 
 
 stargazer(logit21, logit22, logit23, logit24, title = "Matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
           out = "./results/Matched_logit_5.txt")




#### UCDP DVs ####
 

 
 

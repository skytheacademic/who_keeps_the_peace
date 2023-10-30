# Who Keeps the Peace? #
# Data analysis and plotting #
# By: Sky Kunkel

#### load libraries, read data ####
library(tidyverse); library(magrittr); library(ggpubr); library(ggiraphExtra); 
library(coefplot); library(stargazer); library(lmtest); library(sandwich)
library(ggeffects); library(MASS); library(jtools); library(broom.mixed)
library(lfe)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

a = readRDS("./data/kunkel_which_pks.rds") 
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019)) 
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100
c$radpko_m_pko_deployed = c$radpko_m_pko_deployed/100
c$radpko_f_pko_deployed = c$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)



##########################
## TWFE Models - BEGINNING
##########################

####### Hypothesis 1 #########

reg1 = felm(formula = ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid | 
              0 | prio.grid, data = a)
summary(reg1)

reg2 = felm(formula = ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid | 
              0 | prio.grid, data = a)
summary(reg2)

####### Hypothesis 2a #########
reg3 = felm(formula = ucdp_reb_vac_5 ~ radpko_f_prop | time + prio.grid | 
              0 | prio.grid, data = a)
summary(reg3)

reg4 = felm(formula = ucdp_reb_vac_all ~ radpko_f_prop | time + prio.grid | 
              0 | prio.grid, data = a)
summary(reg4)

stargazer(reg1, reg2, style = "AJPS", title = "TWFE Models Testing the Count of Peacekeepers",
          label = "tab:hyp_1", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Women PKs Deployed", "Men PKs Deployed"))

stargazer(reg3, reg4, style = "AJPS", title = "TWFE Models Testing the Proportion of Peacekeepers",
          label = "tab:hyp_2b", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Prop. Women Deployed", "Prop. Men Deployed"))
## test way to plot event study ##

# https://stackoverflow.com/questions/62881774/formula-with-interaction-terms-in-event-study-designs-using-r

library(fixest)

data(base_did)

est_did = feols(ucdp_reb_vac_all ~ i(treated_m, time, 0) | prio.grid + first_treated_m, a)

coefplot(est_did)

##########################
## TWFE Models - END
##########################



####### Hypothesis 2b #########
# Unmatched Data - Logit #
reg9 = glm(ucdp_reb_vac_5 ~ t_bal + t_unbal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a, family = negative.binomial(theta = 1))
names(reg9$coefficients) = c("(Intercept)", "Gender-mixed PK Unit", "Unbalanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "PK Lag", "Violence 6 Months Before")
se_reg9 <- round(coeftest(reg9, vcov = vcovPL(reg9, cluster = a$prio.grid)),4)
se_reg9

reg10 = glm(ucdp_reb_vac_all ~ t_bal + t_unbal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a, family = negative.binomial(theta = 1))
names(reg10$coefficients) = c("(Intercept)", "Gender-mixed PK Unit", "Unbalanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "PK Lag", "Violence 6 Months Before")
se_reg10 <- round(coeftest(reg10, vcov = vcovPL(reg10, cluster = a$prio.grid)),4)
se_reg10

# Matched Data - Logit #
reg11 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
           data = c, family = negative.binomial(theta = 1))
names(reg11$coefficients) = c("(Intercept)", "Gender-mixed PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "Night Lights", "Population Sum", "Population Density",
                              "PK Lag", "Violence 6 Months Before")
se_reg11 <- round(coeftest(reg11, vcov = vcovPL(reg11, cluster = c$prio.grid)),4)
se_reg11

reg12 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
           data = c, family = negative.binomial(theta = 1))
names(reg12$coefficients) = c("(Intercept)", "Gender-mixed PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "Night Lights", "Population Sum", "Population Density",
                              "PK Lag", "Violence 6 Months Before")
se_reg12 <- round(coeftest(reg12, vcov = vcovPL(reg12, cluster = c$prio.grid)),4)
se_reg12

### Plots ###
library(jtools); library(broom.mixed)
# effect_plot(reg1, pred = radpko_f_pko_deployed, interval = T, plot.points = T, jitter = 0.05)

pdf("./results/5_cont.pdf")
plot_summs(se_reg1, se_reg3, model.names = c("OLS", "2SLS"))
dev.off()

pdf("./results/all_cont.pdf")
plot_summs(se_reg2, se_reg4, model.names = c("OLS", "2SLS"))
dev.off()

svg("./results/images/2sls_cont.svg")
plot_summs(se_reg3, se_reg4, model.names = c("Binary Outcome", "Count Outcome"))
dev.off()

coefs_prop_w = c("Prop. Women PKs Deployed", "Avg. Mountain", 
               "Travel Time Nearest City", "Perc. Urban", "PK Lag", "Violence 6 Months Before")
coefs_prop_m = c("Prop. Men PKs Deployed", "Avg. Mountain", 
                 "Travel Time Nearest City", "Perc. Urban", "PK Lag", "Violence 6 Months Before")
z = c("OLS", "2SLS")
pdf("./results/5_prop_w.pdf")
plot_summs(se_reg5, se_reg7, coefs = coefs_prop_w, model.names = z)
dev.off()

pdf("./results/all_prop_w.pdf")
plot_summs(se_reg6, se_reg8, coefs = coefs_prop_w, model.names = z)
dev.off()

svg("./results/images/all_prop.svg")
plot_summs(se_reg7, se_reg8, coefs = coefs_prop_w, model.names = c("Binary Outcome", "Count Outcome"))
dev.off()

# maybe use plots below for these models

# show unmatched then matched models
coef_unmatch = c("Gender-mixed PK Unit", "Unbalanced PK Unit", "Avg. Mountain", 
                 "Travel Time Nearest City", "Perc. Urban","PK Lag", 
                 "Violence 6 Months Before")
coef_match = c("Gender-mixed PK Unit", "Travel Time Nearest City", #excluding perc. urban and PK lag (aesthetics)
               "Population Sum", "Population Density", "Violence 6 Months Before")

pdf("./results/unmatch_OR.pdf")
plot_summs(se_reg9, se_reg10, exp = T, coefs = coef_unmatch, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome") 
dev.off()

pdf("./results/match_OR.pdf", width = 15, height = 8)
plot_summs(se_reg11, se_reg12, exp = T, coefs = coef_match, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome") + 
  theme(axis.text.y = element_text(size=16), axis.text.x =element_text(size=12)) +
  xlab("Odds Ratios") + scale_x_continuous(limits = c(0.1, 1.15))
dev.off()

svg("./results/images/match_OR.svg")
plot_summs(se_reg11, se_reg12, exp = T, coefs = coef_match, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome")
dev.off()





#### Appendix ####
rm(list = setdiff(ls(), c("a", "c")))
gc()

### IV robustness ###

# Code instruments (maybe move to clean_data.R)
a$f_iv = (a$f_pko_africa/10000)*log(a$distance_to_capital)
a$m_iv = (a$m_pko_africa/10000)*log(a$distance_to_capital)
a$f_iv_prop = (a$pko_africa_prop_f)*log(a$distance_to_capital)
a$m_iv_prop = (a$pko_africa_prop_m)*log(a$distance_to_capital)

# 2SLS #
# first stage #
first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

# second stage #
# run the models
reg1 = lm(ucdp_reb_vac_5 ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg1$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$prio.grid)),4)
se_reg1

reg2 = lm(ucdp_reb_vac_all ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg2$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$prio.grid)),4)
se_reg2

### 2SLS ###
first.stage_f_prop = lm(radpko_f_prop ~ f_iv_prop, data = a)

iv_treat_f_prop = first.stage_f_prop$fitted

reg3 = lm(ucdp_reb_vac_5 ~ iv_treat_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg3$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = a$prio.grid)),4)
se_reg3

reg4 = lm(ucdp_reb_vac_all ~ iv_treat_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg4$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = a$prio.grid)),4)
se_reg4

# IV summary tables

stargazer(first.stage_f, first.stage_m, style = "AJPS", title = "1st Stage of Count Instrument", 
          dep.var.labels= c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_count",
          covariate.labels = c("Women in Africa x Distance to Capital", "Men in Africa x Distance to Capital"),
          omit ="Constant", omit.stat = c("adj.rsq","rsq", "ser", "N"))

stargazer(first.stage_f_prop, first.stage_m_prop, style = "AJPS", title = "1st State of Prop. Instrument",
          dep.var.labels = c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_prop", 
          covariate.labels = c("Prop. Women in Africa x Distance to Capital", "Prop. Men in Africa x Distance to Capital"),
          omit ="Constant", omit.stat = c("adj.rsq","rsq", "ser", "N"))

# model tables
stargazer(se_reg1, se_reg2, se_reg3, se_reg4, style = "AJPS", title = "IV Models for Hypothesis 1/2a",
          column.separate = c(2,2), label = "tab:hyp_1-2a")
stargazer(se_reg9, se_reg10, se_reg11, se_reg12, style = "AJPS", title = "Models Testing Hypothesis 3",
          column.labels = c("Logit"), column.separate = c(4), label = "tab:hyp_2b")







#### Matching based on where peacekeeping units with more women and less women went ####
rm(list = setdiff(ls(), c("a", "c")))
gc()

reg1 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + viol_6 + radpko_pko_lag_any,
          data = c, family = negative.binomial(theta = 1))
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = c$prio.grid)),4)
se_reg1

reg2 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + viol_6 + radpko_pko_lag_any,
           data = c, family = negative.binomial(theta = 1))
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = c$prio.grid)),4)
se_reg2

reg1se = se_reg1[,2]
reg2se = se_reg2[,2]

# Save P-values from robust clustering outputs for use in table
reg1p = se_reg1[,4]
reg2p = se_reg2[,4]

stargazer(reg1, reg2, title = "PKO Effectiveness by Peacekeeper Gender - Logit", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "apsr", dep.var.labels = c("Reb VAC (C)", "Reb VAC (B)"),
          covariate.labels = c("Female PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "Perc. Urban", "Night Lights",  "Population Sum", "Population Density", 
                               "Violence 6 Months Before", "PKO Lag (B)"),
          se = list(reg1se, reg2se), p = list(reg1p, reg2p),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level. B = Binary outcome, C = Count outcome.",
          out = "./results/logit.txt")

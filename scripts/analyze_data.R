# Who Keeps the Peace? #
# Data analysis and plotting #
# By: Sky Kunkel

#### Stargazer fix ####
## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# sourced from: https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
# # Unload stargazer if loaded
# detach("package:stargazer",unload=T)
# # Delete it
# remove.packages("stargazer")
# # Download the source
# download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# # Unpack
# untar("stargazer_5.2.3.tar.gz")
# # Read the sourcefile with .inside.bracket fun
# stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# # Move the length check 5 lines up so it precedes is.na(.)
# stargazer_src[1990] <- stargazer_src[1995]
# stargazer_src[1995] <- ""
# # Save back
# writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# # Compile and install the patched package
# install.packages("stargazer", repos = NULL, type="source")

#### load libraries, read data ####
library(tidyverse); library(magrittr); library(ggpubr); library(ggiraphExtra); 
library(coefplot); library(stargazer); library(lmtest); library(sandwich)
library(ggeffects); library(MASS)

# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

a = readRDS("./data/kunkel_which_pks.rds") 
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# descriptive statistics table #
labs = c("Total PKs Deployed", "Female PKs Deployed", "Male PKs Deployed", "Gender Balanced Units", "Gender Un-Balanced Units")

stargazer(a[c("radpko_pko_deployed", "radpko_f_pko_deployed", "radpko_m_pko_deployed", "t_bal", "t_unbal")], 
          covariate.labels = labs, digits = 3, style = "apsr", 
          title = "Descriptive Statistics of the Independent Variables", out = "./results/pks_table.txt")
labs1 = c("Gov VAC (C)", "Gov VAC (B)", "Reb VAC (C)", "Reb VAC (B)")
stargazer(a[c("ucdp_gov_vac_all", "ucdp_gov_vac_5", "ucdp_reb_vac_all", "ucdp_reb_vac_5")], 
          covariate.labels = labs1, digits = 3, style = "apsr", 
          title = "Descriptive Statistics of the Dependent Variables", out = "./results/violence_table.txt")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019)) 
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100
c$radpko_m_pko_deployed = c$radpko_m_pko_deployed/100
c$radpko_f_pko_deployed = c$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)

# Code instruments (maybe move to clean_data.R)
a$f_iv = (a$f_pko_africa/10000)*log(a$distance_to_capital)
a$m_iv = (a$m_pko_africa/10000)*log(a$distance_to_capital)
a$f_iv_prop = (a$pko_africa_prop_f)*log(a$distance_to_capital)
a$m_iv_prop = (a$pko_africa_prop_m)*log(a$distance_to_capital)

####### Hypothesis 1 #########

### OLS Models ###
reg1 = lm(ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg1$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before") # rename to make plotting easier later
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$prio.grid)),4)
se_reg1

reg2 = lm(ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
              radpko_pko_lag + viol_6,
            data = a)
names(reg2$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$prio.grid)),4)
se_reg2

### 2SLS ###
# first stage #
first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

# second stage #
# run the models
reg3 = lm(ucdp_reb_vac_5 ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg3$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = a$prio.grid)),4)
se_reg3

reg4 = lm(ucdp_reb_vac_all ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg4$coefficients) = c("(Intercept)", "Women PKs Deployed", "Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = a$prio.grid)),4)
se_reg4


####### Hypothesis 2a #########

### OLS ###
reg5 = lm(ucdp_reb_vac_5 ~ radpko_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
                   radpko_pko_lag + viol_6,
                 data = a)
names(reg5$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg5 <- round(coeftest(reg5, vcov = vcovPL(reg5, cluster = a$prio.grid)),4)
se_reg5

reg6 = lm(ucdp_reb_vac_5 ~ radpko_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
                   radpko_pko_lag + viol_6,
                 data = a)
names(reg6$coefficients) = c("(Intercept)", "Prop. Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg6 <- round(coeftest(reg6, vcov = vcovPL(reg6, cluster = a$prio.grid)),4)
se_reg6

reg7 = lm(ucdp_reb_vac_all ~ radpko_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
                     radpko_pko_lag + viol_6,
                   data = a)
names(reg7$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg7 <- round(coeftest(reg7, vcov = vcovPL(reg7, cluster = a$prio.grid)),4)
se_reg7

reg8 = lm(ucdp_reb_vac_all ~ radpko_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
                     radpko_pko_lag + viol_6,
                   data = a)
names(reg8$coefficients) = c("(Intercept)", "Prop. Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg8 <- round(coeftest(reg8, vcov = vcovPL(reg8, cluster = a$prio.grid)),4)
se_reg8

### 2SLS ###
first.stage_f_prop = lm(radpko_f_prop ~ f_iv_prop, data = a)
first.stage_m_prop = lm(radpko_m_prop ~ m_iv_prop, data = a)

iv_treat_f_prop = first.stage_f_prop$fitted
iv_treat_m_prop = first.stage_m_prop$fitted

reg9 = lm(ucdp_reb_vac_5 ~ iv_treat_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg9$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg9 <- round(coeftest(reg9, vcov = vcovPL(reg9, cluster = a$prio.grid)),4)
se_reg9

reg10 = lm(ucdp_reb_vac_5 ~ iv_treat_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg10$coefficients) = c("(Intercept)", "Prop. Men PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg10 <- round(coeftest(reg10, vcov = vcovPL(reg10, cluster = a$prio.grid)),4)
se_reg10

reg11 = lm(ucdp_reb_vac_all ~ iv_treat_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg11$coefficients) = c("(Intercept)", "Prop. Women PKs Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence 6 Months Before")
se_reg11 <- round(coeftest(reg11, vcov = vcovPL(reg11, cluster = a$prio.grid)),4)
se_reg11

reg12 = lm(ucdp_reb_vac_all ~ iv_treat_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
names(reg12$coefficients) = c("(Intercept)", "Prop. Men PKs Deployed",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "PK Lag", "Violence 6 Months Before")
se_reg12 <- round(coeftest(reg12, vcov = vcovPL(reg12, cluster = a$prio.grid)),4)
se_reg12


####### Hypothesis 2b #########
# Unmatched Data - Logit #
reg13 = glm(ucdp_reb_vac_5 ~ t_bal + t_unbal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a, family = negative.binomial(theta = 1))
names(reg13$coefficients) = c("(Intercept)", "Balanced PK Unit", "Unbalanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "PK Lag", "Violence 6 Months Before")
se_reg13 <- round(coeftest(reg13, vcov = vcovPL(reg13, cluster = a$prio.grid)),4)
se_reg13

reg14 = glm(ucdp_reb_vac_all ~ t_bal + t_unbal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a, family = negative.binomial(theta = 1))
names(reg14$coefficients) = c("(Intercept)", "Balanced PK Unit", "Unbalanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "PK Lag", "Violence 6 Months Before")
se_reg14 <- round(coeftest(reg14, vcov = vcovPL(reg14, cluster = a$prio.grid)),4)
se_reg14

# Matched Data - Logit #
reg15 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
           data = c, family = negative.binomial(theta = 1))
names(reg15$coefficients) = c("(Intercept)", "Balanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "Night Lights", "Population Sum", "Population Density",
                              "PK Lag", "Violence 6 Months Before")
se_reg15 <- round(coeftest(reg15, vcov = vcovPL(reg15, cluster = c$prio.grid)),4)
se_reg15

reg16 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
           data = c, family = negative.binomial(theta = 1))
names(reg16$coefficients) = c("(Intercept)", "Balanced PK Unit",
                              "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                              "Night Lights", "Population Sum", "Population Density",
                              "PK Lag", "Violence 6 Months Before")
se_reg16 <- round(coeftest(reg16, vcov = vcovPL(reg16, cluster = c$prio.grid)),4)
se_reg16


### Plots ###
library(jtools); library(broom.mixed)
effect_plot(reg1, pred = radpko_f_pko_deployed, interval = T, plot.points = T, jitter = 0.05)

## list of models:
# se_reg1: OLS, <4 deaths, continuous IV
# se_reg2: OLS, all deaths, continuous IV
# se_reg3: 2SLS, <4 deaths, continuous IV
# se_reg4: 2SLS, all deaths, continuous IV
# se_reg5: OLS, <4 deaths, f_prop IV
# se_reg6: OLS, <4 deaths, m_prop IV
# se_reg7: OLS, all deaths, f_prop IV
# se_reg8: OLS, all deaths, m_prop IV
# se_reg9: 2SLS, <4 deaths, f_prop_IV
# se_reg10: 2SLS, <4 deaths, m_prop IV
# se_reg11: 2SLS, all deaths, f_prop IV
# se_reg12: 2SLS, all deaths, m_prop IV
# se_reg13: Logit, <4 deaths, binary IV
# se_reg14: Logit, all deaths, binary IV
# se_reg15: Logit, <4 deaths, binary IV - matched
# se_reg16: Logit, all deaths, binary IV - matched

pdf("./results/5_cont.pdf")
plot_summs(se_reg1, se_reg3, model.names = c("OLS", "2SLS"))
dev.off()

pdf("./results/all_cont.pdf")
plot_summs(se_reg2, se_reg4, model.names = c("OLS", "2SLS"))
dev.off()

coefs_prop_w = c("Prop. Women PKs Deployed", "Avg. Mountain", 
               "Travel Time Nearest City", "Perc. Urban", "PK Lag", "Violence 6 Months Before")
coefs_prop_m = c("Prop. Men PKs Deployed", "Avg. Mountain", 
                 "Travel Time Nearest City", "Perc. Urban", "PK Lag", "Violence 6 Months Before")
z = c("OLS", "2SLS")
pdf("./results/5_prop_w.pdf")
plot_summs(se_reg5, se_reg9, coefs = coefs_prop_w, model.names = z)
dev.off()

pdf("./results/5_prop_m.pdf")
plot_summs(se_reg6, se_reg10, coefs = coefs_prop_m, model.names = z)
dev.off()

pdf("./results/all_prop_w.pdf")
plot_summs(se_reg7, se_reg11, coefs = coefs_prop_w, model.names = z)
dev.off()

pdf("./results/all_prop_m.pdf")
plot_summs(se_reg8, se_reg12, coefs = coefs_prop_m, model.names = z)
dev.off()

# maybe use plots below for these models

# show unmatched then matched models
coef_unmatch = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", 
                 "Travel Time Nearest City", "Perc. Urban","PK Lag", 
                 "Violence 6 Months Before")
coef_match = c("Balanced PK Unit", "Travel Time Nearest City", 
                 "Perc. Urban", "Population Sum", "Population Density", 
                 "PK Lag", "Violence 6 Months Before")

pdf("./results/unmatch_OR.pdf")
plot_summs(se_reg13, se_reg14, exp = T, coefs = coef_unmatch, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome") 
dev.off()

pdf("./results/match_OR.pdf")
plot_summs(se_reg15, se_reg16, exp = T, coefs = coef_match, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome")
dev.off()












############# OLD - TO DELETE ############# 


# RUN AND PLOT RESTRICTED/NAIVE MODELS

# analysis by total PKs
reg0 = lm(ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
summary(reg0)

reg00 = glm(ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             radpko_pko_lag + viol_6,
           data = a)
summary(reg00)

# analysis by proportion PKs
reg_fprop_5 = lm(ucdp_reb_vac_5 ~ radpko_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
summary(reg_fprop_5)

reg_mprop_5 = lm(ucdp_reb_vac_5 ~ radpko_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6,
          data = a)
summary(reg_mprop_5)

reg_fprop_all = lm(ucdp_reb_vac_all ~ radpko_f_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
              radpko_pko_lag + viol_6,
            data = a)
summary(reg_fprop_all)

reg_mprop_all = lm(ucdp_reb_vac_all ~ radpko_m_prop + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             radpko_pko_lag + viol_6,
           data = a)
summary(reg_mprop_all)

# predicted outcomes based on gender proportion #
pred_fprop_5 = ggpredict(reg_fprop_5, terms = "radpko_f_prop [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
pred_fprop_5$group = "Women PKs"
pred_mprop_5 = ggpredict(reg_mprop_5, terms = "radpko_m_prop [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
pred_mprop_5$group = "Men PKs"
pred_mprop_5_gg = rbind(pred_fprop_5, pred_mprop_5) %>% mutate(x = x/10)

pred_fprop_all = ggpredict(reg_fprop_all, terms = "radpko_f_prop [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
pred_fprop_all$group = "Women PKs"
pred_mprop_all = ggpredict(reg_mprop_all, terms = "radpko_m_prop [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
pred_mprop_all$group = "Men PKs"
pred_mprop_all_gg = rbind(pred_fprop_all, pred_mprop_all) %>% mutate(x = x/10)

pdf("./results/pred_5_prop.pdf")
ggplot(pred_mprop_5_gg) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Gender Proportion of Peacekeeping Unit") +
  ylab("Predicted Pr( >4 Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "none") +
  scale_colour_manual("",values=c("#228B22", "#6A1C36"))+
  scale_fill_manual("",values=c("black", "black"))
dev.off()

pdf("./results/pred_all_prop.pdf")
ggplot(pred_mprop_all_gg) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Gender Proportion of Peacekeeping Unit") +
  ylab("Predicted Civilian Fatalities") + theme_pubclean() +
  theme(legend.position = "none") +
  scale_colour_manual("",values=c("#228B22", "#6A1C36"))+
  scale_fill_manual("",values=c("black", "black"))
dev.off()

# predicted outcomes based on total counts #
reg0_f = ggpredict(reg0, terms = "radpko_f_pko_deployed [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
reg0_f$group = "Women PKs"
reg0_m = ggpredict(reg0, terms = "radpko_m_pko_deployed [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
reg0_m$group = "Men PKs"
reg0_gg = rbind(reg0_f, reg0_m)
reg00_f = ggpredict(reg00, terms = "radpko_f_pko_deployed [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
reg00_f$group = "Women PKs"
reg00_m = ggpredict(reg00, terms = "radpko_m_pko_deployed [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
reg00_m$group = "Men PKs"
reg00_gg = rbind(reg00_f, reg00_m)


pdf("./results/pred_5_total.pdf")
ggplot(reg0_gg) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Gender of Peacekeepers Deployed (100s)") +
  ylab("Predicted Pr( >4 Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "none") +
  scale_colour_manual("",values=c("#228B22", "#6A1C36"))+
  scale_fill_manual("",values=c("black", "black"))
dev.off()

pdf("./results/pred_all_total.pdf")
ggplot(reg00_gg) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Gender of Peacekeepers Deployed (100s)") +
  ylab("Predicted Civilian Fatalities") + theme_pubclean() +
  theme(legend.position = "none") +
  scale_colour_manual("",values=c("#228B22", "#6A1C36"))+
  scale_fill_manual("",values=c("black", "black"))
dev.off()

legend = 
  ggplot(reg00_gg) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  scale_colour_manual("",values=c("#228B22", "#6A1C36"))+
  scale_fill_manual("",values=c("black", "black")) + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.background = element_rect(color = "black"), 
        legend.position = "bottom", legend.key.size = unit(1.75, 'cm'), legend.margin=margin(c(5,5,5,5)))
pdf("./results/pred_legend.pdf")
as_ggplot(get_legend(legend))
dev.off()

#### 2SLS ####
# Code instrument for each 
a$f_iv = (a$f_pko_africa/10000)*log(a$distance_to_capital)
a$m_iv = (a$m_pko_africa/10000)*log(a$distance_to_capital)

first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)
summary(first.stage_f)
summary(first.stage_m)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

# Print Instrument Tables #
stargazer(first.stage_f, first.stage_m, style = "apsr", covariate.labels = c("Female PK Instrument", "Male PK Instrument"),
          dep.var.labels = c("Female PKs Deployed", "Male PKs Deployed"), out = "./results/1st_stage.txt")

# run the models
reg1 = lm(ucdp_gov_vac_5 ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
           radpko_pko_lag + viol_6,
           data = a)
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$prio.grid)),4)
se_reg1

reg2 = lm(ucdp_reb_vac_5 ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
           radpko_pko_lag + viol_6,
           data = a)
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$prio.grid)),4)
se_reg2

reg3 = lm(ucdp_gov_vac_all ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
          radpko_pko_lag + viol_6,
          data = a)
se_reg3 <- round(coeftest(reg3, vcov = vcovPL(reg3, cluster = a$prio.grid)),4)
se_reg3

reg4 = lm(ucdp_reb_vac_all ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
          radpko_pko_lag + viol_6,
          data = a)
se_reg4 <- round(coeftest(reg4, vcov = vcovPL(reg4, cluster = a$prio.grid)),4)
se_reg4


### output results into tex table ###
# Save Standard Errors to objects for use in table
reg1se = se_reg1[,2]
reg2se = se_reg2[,2]
reg3se = se_reg3[,2]
reg4se = se_reg4[,2]

# Save P-values from robust clustering outputs for use in table
reg1p = se_reg1[,4]
reg2p = se_reg2[,4]
reg3p = se_reg3[,4]
reg4p = se_reg4[,4]

# pk effectiveness by pk gender table #
stargazer(reg1, reg3, reg2, reg4, title = "PKO Effectiveness by Peacekeeper Gender - 2SLS", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "apsr", dep.var.labels = c("Gov VAC (B)", "Gov VAC (C)", "Reb VAC (B)", "Reb VAC (C)"),
          covariate.labels = c("Female PKs Deployed", "Male PKs Deployed", "Avg. Mountain", "Travel Time Nearest City",
                               "Perc. Urban", "PK Lag", "Violence 6 Months Before"),
          se = list(reg1se, reg3se, reg2se, reg4se), p = list(reg1p, reg3p, reg2p, reg4p),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level. B = Binary outcome, C = Count outcome.",
          out = "./results/2sls.txt")

# plot based on predicted values #

ggpredict(reg2, terms = c("iv_treat_f")) |> plot()



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

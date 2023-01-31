# Who Keeps the Peace? #
# Data analysis and plotting #
# By: Sky Kunkel

#### load libraries, read data ####
library(tidyverse); library(magrittr); library(ggpubr); library(ggiraphExtra); 
library(coefplot); library(stargazer); library(lmtest); library(sandwich)
library(ggeffects); library(MASS)

# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
a = readRDS("./data/kunkel_which_pks.rds") 
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019))
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100
c$radpko_m_pko_deployed = c$radpko_m_pko_deployed/100
c$radpko_f_pko_deployed = c$radpko_f_pko_deployed/100

# Plot 1 & 2
reg0 = lm(ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6 + radpko_f_pko_deployed*radpko_m_pko_deployed,
          data = a)
reg00 = lm(ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
            radpko_pko_lag + viol_6 + radpko_f_pko_deployed*radpko_m_pko_deployed,
          data = a)

# marginal effects on gender balance #
reg0_f = ggpredict(reg0, terms = "radpko_f_pko_deployed [0, 20, 40, 60, 80]")
reg0_m = ggpredict(reg0, terms = "radpko_m_pko_deployed [0, 20, 40, 60, 80]")
reg00_f = ggpredict(reg00, terms = "radpko_f_pko_deployed [0, 20, 40, 60, 80]")
reg00_m = ggpredict(reg00, terms = "radpko_m_pko_deployed [0, 20, 40, 60, 80]")

pdf("./results/pred_f_5.pdf")
ggplot(reg0_f) +
  geom_line(aes(x, predicted)) + 
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), linetype = "dashed", 
              alpha = 0.1, show.legend = F, colour = "dark grey") +
  xlab("Female Peacekeepers Deployed") +
  ylab("Predicted Pr( >4 Civilian Deaths)") + theme_pubclean() + 
  theme(legend.position = "none") 
dev.off()

pdf("./results/pred_m_5.pdf")
ggplot(reg0_m) +
  geom_line(aes(x, predicted)) + 
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), linetype = "dashed", 
              alpha = 0.1, show.legend = F, colour = "dark grey") +
  xlab('Male Peacekeepers Deployed') +
  ylab("Predicted Pr( >4 Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "none")
dev.off()

pdf("./results/pred_f_all.pdf")
ggplot(reg00_f) +
  geom_line(aes(x, predicted)) + 
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), linetype = "dashed", 
              alpha = 0.1, show.legend = F, colour = "dark grey") +
  xlab("Female Peacekeepers Deployed") +
  ylab("Predicted Civilian Fatalities") + theme_pubclean() +
  theme(legend.position = "none")
dev.off()

pdf("./results/pred_m_all.pdf")
ggplot(reg00_m) +
  geom_line(aes(x, predicted)) + 
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high), linetype = "dashed", 
              alpha = 0.1, show.legend = F, colour = "dark grey") +
  xlab('Male Peacekeepers Deployed') +
  ylab("Predicted Civilian Fatalities") + theme_pubclean() +
  theme(legend.position = "none")
dev.off()

# Code instrument for each 
a$f_iv = (a$f_pko_africa/10000)*log(a$distance_to_capital)
a$m_iv = (a$m_pko_africa/10000)*log(a$distance_to_capital)

first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)
summary(first.stage_f)
summary(first.stage_m)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

#### 2SLS ####
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


############# OLD - TO DELETE ############# 


# Figures and Plots for non-matched regressions 

# descriptive statistics table #
labs = c("Total PKs deployed", "Gender Balanced Units", "Gender Un-Balanced Units",
         "Majority Troop Units", "Majority Police Units", "Majority Observer Units")

stargazer(a[c("radpko_pko_deployed", "t_bal", "t_unbal", "untrp_maj", "unpol_maj", "unmob_maj")], covariate.labels = labs, digits = 3, 
          style = "ajps", omit.summary.stat = "n",
          title = "Treatments Summarized by Grid-month observations",
          out = "./results/pks_table.txt")

# Figures and Plots for matched regressions 

## logit outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Violence by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Pr(Violent Event)", "Total Violent Events","Pr(Fatality)","Total Fatalities"), 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:radpko_pko_lag", "t_unbal:radpko_pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.")

# matched pk effectiveness by pk type #
stargazer(reg5, reg6, reg7, reg8, title = "Matched Results Pr(Violence) by Troop Type", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          covariate.labels = c("Majority Trp. PK Unit", "Majority Pol. PK Unit", "Majority Obs. PK Unit", "Avg. Mountain", 
                               "Travel Time Nearest City", "% Urban", "Night Lights", "Population Sum", "Population Density", 
                               "PK Lag", "Violence 6 Months Before"), 
          se = list(reg25se, reg26se, reg27se, reg28se), p = list(reg25p, reg26p, reg27p, reg28p),
          omit = c("untrp_maj:radpko_pko_lag", "unpol_maj:radpko_pko_lag", "unmob_maj:radpko_pko_lag", "untrp_maj:viol_6", "unpol_maj:viol_6", 
                   "unmob_maj:viol_6"))


#Marginal effects plots

# marginal effects on gender balance #
reg2.bal = ggpredict(reg2, terms = "t_bal", condition = c(t_unbal = 0))
reg2.bal$group = "Incumbent Deaths, Gender Balanced PKs"
reg2.unbal = ggpredict(reg2, terms = "t_unbal", condition = c(t_bal = 0))
reg2.unbal$group = "Incumbent Deaths, Gender Unbalanced PKs"
reg2_gg = rbind(reg2.bal, reg2.unbal)
reg4.bal = ggpredict(reg4, terms = "t_bal", condition = c(t_unbal = 0))
reg4.bal$group = "Rebels Deaths, Gender Balanced PKs"
reg4.unbal = ggpredict(reg4, terms = "t_unbal", condition = c(t_bal = 0))
reg4.unbal$group = "Rebel Deaths, Gender Unbalanced PKs"
reg4_gg = rbind(reg4.bal, reg4.unbal)

gen_death = rbind(reg2_gg, reg4_gg)

ggplot(gen_death) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  ylim(-0.01, 0.15) + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Faction and Gender Balance of PK Unit")) +
  scale_x_continuous(breaks = seq(0,1,1))

ggplot(gen_death_1) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  ylim(-0.01, 0.75) + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Faction and Gender Balance of PK Unit")) +
  scale_x_continuous(breaks = seq(0,1,1))



# this plot works, testing with above
ggplot(reg2_gg) +
  geom_line(aes(x, predicted, colour = factor(group))) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = factor(group), 
                  fill = factor(group)), linetype = "dashed", alpha = 0.15) +
  ylab("Predicted Violence Events") + theme_pubclean() + 
  guides(fill=guide_legend(title="New Legend Title"))
scale_x_continuous(breaks = seq(0,1,1))


#### Matched Figures
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
          omit = c("t_bal:radpko_pko_lag", "t_unbal:radpko_pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          notes = "Robust Standard Errors clustered at the PRIO-Grid level.",
          out = "./results/matched_gender_c.txt")

## odds ratios outputs ##
stargazer(reg1, reg2, reg3, reg4, title = "Matched Results Violence by PK Gender", 
          align = TRUE, digits=3, font.size = "scriptsize",
          style = "ajps", dep.var.labels = c("Rebel Event", "Rebel Death","Gov't Event","Gov't Death"), 
          dep.var.caption = "Count Outcome", 
          covariate.labels = c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountain", "Travel Time Nearest City",
                               "% Urban", "Night Lights", "Population Sum", "Population Density", "PK Lag", 
                               "Violence 6 Months Before"), 
          se = list(reg21se, reg22se, reg23se, reg24se), p = list(reg21p, reg22p, reg23p, reg24p),
          omit = c("t_bal:radpko_pko_lag", "t_unbal:radpko_pko_lag", "t_bal:viol_6", "t_unbal:viol_6"),
          apply.coef = exp, t.auto=F, p.auto=F,
          notes = "Important: this table is only for interpretation in terms of estimates and p-values. Standard errors are not correctly inputted.",
          out = "./results/matched_gender_or_c.txt")







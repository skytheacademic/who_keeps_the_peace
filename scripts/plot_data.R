# Conflict Grids & Peacekeeping: Figures and Plots #

library(tidygeocoder)
library(tidyverse); library(viridis)
library(gdata); library(designmatch) 
library(magrittr)

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(gurobi); library(MASS); library(lme4); library(vtable)
library(sensitivitymw); library(lmtest); library(sandwich); library(magick)
library(ggeffects)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
a = readRDS("./data/kunkel_cg.rds")


##### Run and plot PKs by composition #####
# Troops #
reg1 = glm(gov_event.b ~ untrp + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
                untrp*pko_lag + untrp*viol_6,
               data = a, family = negative.binomial(theta = 1))

reg2 = glm(gov_death.b ~ untrp + mountains_mean + ttime_mean + pop_gpw_sum +
                 pop.dens + pko_lag + viol_6 +
                untrp:pko_lag + untrp:viol_6,
               data = a, family = negative.binomial(theta = 1))

reg3 = glm(reb_event.b ~ untrp + mountains_mean + ttime_mean + urban_gc + 
                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
                untrp*pko_lag + untrp*viol_6,
               data = a, family = negative.binomial(theta = 1))

reg4 = glm(reb_death.b ~ untrp + mountains_mean + ttime_mean + pop_gpw_sum +
                 pop.dens + pko_lag + viol_6 +
                untrp*pko_lag + untrp*viol_6,
               data = a, family = negative.binomial(theta = 1))

# Police #
reg5 = glm(gov_event.b ~ unpol + mountains_mean + ttime_mean + urban_gc + 
                nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
              unpol*pko_lag + unpol*viol_6,
              data = a, family = negative.binomial(theta = 1))

reg6 = glm(gov_death.b ~ unpol + mountains_mean + ttime_mean + pop_gpw_sum +
                pop.dens + pko_lag + viol_6 +
              unpol*pko_lag + unpol*viol_6,
              data = a, family = negative.binomial(theta = 1))

reg7 = glm(reb_event.b ~ unpol + mountains_mean + ttime_mean + urban_gc + 
                nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
              unpol*pko_lag + unpol*viol_6,
              data = a, family = negative.binomial(theta = 1))

reg8 = glm(reb_death.b ~ unpol + mountains_mean + ttime_mean + pop_gpw_sum +
                pop.dens + pko_lag + viol_6 +
              unpol*pko_lag + unpol*viol_6,
              data = a, family = negative.binomial(theta = 1))
# Observers #
reg9 = glm(gov_event.b ~ unmob + mountains_mean + ttime_mean + urban_gc + 
             nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
             unmob*pko_lag + unmob*viol_6,
           data = a, family = negative.binomial(theta = 1))

reg10 = glm(gov_death.b ~ unmob + mountains_mean + ttime_mean + pop_gpw_sum +
             pop.dens + pko_lag + viol_6 +
             unmob*pko_lag + unmob*viol_6,
           data = a, family = negative.binomial(theta = 1))

reg11 = glm(reb_event.b ~ unmob + mountains_mean + ttime_mean + urban_gc + 
             nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
             unmob*pko_lag + unmob*viol_6,
           data = a, family = negative.binomial(theta = 1))

reg12 = glm(reb_death.b ~ unmob + mountains_mean + ttime_mean + pop_gpw_sum +
             pop.dens + pko_lag + viol_6 +
             unmob*pko_lag + unmob*viol_6,
           data = a, family = negative.binomial(theta = 1))


# Predict MEs and Plot #

reg1.gg = ggpredict(reg1, terms = "untrp")
reg1.gg$group = "Incumbent Violent Events"
reg2.gg = ggpredict(reg2, terms = "untrp")
reg2.gg$group = "Incumbent Deaths"
me_pred_st_trp = rbind(reg1.gg, reg2.gg)
me_pred_st_trp = me_pred_st_trp[-c(13:14, 27:28),]

reg3.gg = ggpredict(reg3, terms = "untrp")
reg3.gg$group = "Rebel Violent Events"
reg4.gg = ggpredict(reg4, terms = "untrp")
reg4.gg$group = "Rebel Deaths"
me_pred_rb_trp = rbind(reg3.gg, reg4.gg)
me_pred_rb_trp = me_pred_st_trp[-c(13:14, 27:28),]






reg13.gg = ggpredict(reg13, terms = "pko_deployed")
reg13.gg$group = "Incumbent Violent Events"
reg14.gg = ggpredict(reg14, terms = "pko_deployed")
reg14.gg$group = "Incumbent Deaths"
gen_death.c.gov = rbind(reg13.gg, reg14.gg)





rm(list = setdiff(ls(), "a")) 
gc()
#### ME of violence by naive PKs ####
reg13 = glm(gov_event.b ~ pko_deployed + mountains_mean + ttime_mean + urban_gc + 
              nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
              pko_deployed*pko_lag + pko_deployed*viol_6,
            data = a, family = negative.binomial(theta = 1))
reg14 = glm(gov_death.b ~ pko_deployed + mountains_mean + ttime_mean + pop_gpw_sum +
              pop.dens + pko_lag + viol_6 +
              pko_deployed*pko_lag + pko_deployed*viol_6,
            data = a, family = negative.binomial(theta = 1))
#REB OSV - Continuous Treatment
reg15 = glm(reb_event.b ~ pko_deployed + mountains_mean + ttime_mean + urban_gc + 
              nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 +
              pko_deployed*pko_lag + pko_deployed*viol_6,
            data = a, family = negative.binomial(theta = 1))
reg16 = glm(reb_death.b ~ pko_deployed + mountains_mean + ttime_mean + pop_gpw_sum +
              pop.dens + pko_lag + viol_6 +
              pko_deployed*pko_lag + pko_deployed*viol_6,
            data = a, family = negative.binomial(theta = 1))
# marginal effects on pko treatment size #
reg13.gg = ggpredict(reg13, terms = "pko_deployed")
reg13.gg$group = "Incumbent Violent Events"
reg14.gg = ggpredict(reg14, terms = "pko_deployed")
reg14.gg$group = "Incumbent Deaths"
gen_death.c.gov = rbind(reg13.gg, reg14.gg)

reg15.gg = ggpredict(reg15, terms = "pko_deployed")
reg15.gg$group = "Rebel Violent Events"
reg16.gg = ggpredict(reg16, terms = "pko_deployed")
reg16.gg$group = "Rebel Deaths"
gen_death.c.reb = rbind(reg15.gg, reg16.gg)

pdf("./results/pks_pred_gov.pdf")
ggplot(gen_death.c.gov) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Peacekeeper Count") + ylab("Predicted Violence Against Civilians") + theme_pubclean() +
  ggtitle("Predicted Violence Outcomes from State Actors based on Peacekeeper Counts")
dev.off()

pdf("./results/pks_pred_reb.pdf")
ggplot(gen_death.c.reb) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  xlab("Peacekeeper Count") + ylab("Predicted Violence Against Civilians") + theme_pubclean() +
  ggtitle("Predicted Violence Outcomes from Rebel Actors based on Peacekeeper Counts")
dev.off()

rm(list = setdiff(ls(), "a")) 
gc()


#### ME of violence with matched PKs
rm(list = ls())
a = readRDS("./data/kunkel_cg_matched.RDS")

# reg1 = glm.nb(gov_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
#                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
#                 t_bal*pko_lag + t_unbal*pko_lag +
#                 t_bal*viol_6 + t_unbal*viol_6,
#               data = a)
reg2 = glm(gov_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                pko_lag + viol_6 +
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
              data = a, family = negative.binomial(theta = 1))
# REB OSV - Binary treatment by gender #
# reg3 = glm.nb(reb_event.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + urban_gc + 
#                 nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
#                 t_bal*pko_lag + t_unbal*pko_lag +
#                 t_bal*viol_6 + t_unbal*viol_6, data = a)
reg4 = glm(reb_death.b ~ t_bal + t_unbal + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                pko_lag + viol_6 +
                t_bal*pko_lag + t_unbal*pko_lag +
                t_bal*viol_6 + t_unbal*viol_6,
              data = a, family = negative.binomial(theta = 1))
# GOV OSV - Binary treatment by PK Type #
reg5 = glm(gov_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = a, family = negative.binomial(theta = 1))
reg6 = glm(gov_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6, 
              data = a, family = negative.binomial(theta = 1))
# REB OSV - Binary treatment by PK Type #
reg7 = glm(reb_event.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = a, family = negative.binomial(theta = 1))
reg8 = glm(reb_death.b ~ untrp_maj + unpol_maj + unmob_maj + mountains_mean + ttime_mean + 
                pop_gpw_sum + pop.dens + pko_lag + viol_6 + 
                untrp_maj*pko_lag + unpol_maj*pko_lag + unmob_maj*pko_lag +
                untrp_maj*viol_6 + unpol_maj*viol_6 + unmob_maj*viol_6,
              data = a, family = negative.binomial(theta = 1))


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


# let's make a plot for the odds ratios instead
# https://stackoverflow.com/questions/47085514/simple-way-to-visualise-odds-ratios-in-r

# marginal effects on peacekeeper composition
reg6.trp = ggpredict(reg6, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0))
reg6.trp$group = "Incumbent Deaths, Majority Troop PKs"
reg8.trp = ggpredict(reg8, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0))
reg8.trp$group = "Rebels Deaths, Majority Troop PKs"
death_trp_pred = rbind(reg6.trp, reg8.trp)

reg6.pol = ggpredict(reg6, terms = "unpol_maj", condition = c(untrp_maj = 0, unmob_maj = 0))
reg6.pol$group = "Incumbent Deaths, Majority Police PKs"
reg8.pol = ggpredict(reg8, terms = "unpol_maj", condition = c(untrp_maj = 0, unmob_maj = 0))
reg8.pol$group = "Rebels Deaths, Majority Police PKs"

reg6.mob = ggpredict(reg6, terms = "unmob_maj", condition = c(untrp_maj = 0, unpol_maj = 0))
reg6.mob$group = "Incumbent Deaths, Majority Observers PKs"
reg8.mob = ggpredict(reg8, terms = "unmob_maj", condition = c(untrp_maj = 0, unpol_maj = 0))
reg8.mob$group = "Rebels Deaths, Majority Observers PKs"
# reg6_gg = rbind(reg6.trp, reg6.pol, reg6.mob)


# reg8_gg = rbind(reg8.trp, reg8.pol,reg8.mob)
# gen_death_1 = rbind(reg6_gg, reg8_gg)

ggplot(death_trp_pred) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  ylim(-0.01, 0.10) + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Faction and Composition of PK Unit")) +
  scale_x_continuous(breaks = seq(0,1,1))



##### Plot of Mali w/ PRIO grids #####
# let's add a plot of Mali to check #
a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
#aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")

pdf("../results/mali_violent_events_prio.pdf")
plot_1
dev.off()

a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
# aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/mali_pks_prio.pdf")
plot_3
dev.off()



### Aggregate PK & PKO locations w/ violence and violent events ###

a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_1.pdf")
plot_1
dev.off()



table(a.min$t_ind)
a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(t_ind))

aa1 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa1$v[aa1$v == 0] <- NA

plot_2 = ggplot(data = aa1) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "PKO")

pdf("../results/plot_2.pdf")
plot_2
dev.off()


a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_3.pdf")
plot_3
dev.off()


a.min3 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(event))

aa3 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min3, by = "prio.grid")
aa3$v[aa3$v == 0] <- NA

plot_4 = ggplot(data = aa3) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent Events")

pdf("../results/plot_4.pdf")
plot_4
dev.off()


##### Time-series plots of MINUSMA #####
#subset
a.una = subset(a, country == "Democratic Republic of Congo") 


tapply(a.una$fatalities, a.una$date, sum)

# dates to subset for MINUSMA: 2017-06-01 2017-07-01 2017-08-01 2017-09-01
a.min.02 = subset(a.una, date == "2017-02-01")
a.min.03 = subset(a.una, date == "2017-03-01")
a.min.04 = subset(a.una, date == "2017-04-01")
a.min.05 = subset(a.una, date == "2017-05-01")


## 2017-02-01 ##
a.min.02.ft = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa4 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.ft, by = "prio.grid")
aa4$v[aa4$v == 0] <- NA

plot_5 = ggplot(data = aa4) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_5.pdf")
plot_5
dev.off()


a.min.02.pk = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa5 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.pk, by = "prio.grid")
aa5$v[aa5$v == 0] <- NA

plot_6 = ggplot(data = aa5) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_6.pdf")
plot_6
dev.off()


## 2017-03-01 ##

a.min.03.ft = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa6 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.ft, by = "prio.grid")
aa6$v[aa6$v == 0] <- NA

plot_7 = ggplot(data = aa6) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_7.pdf")
plot_7
dev.off()


a.min.03.pk = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa7 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.pk, by = "prio.grid")
aa7$v[aa7$v == 0] <- NA

plot_8 = ggplot(data = aa7) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_8.pdf")
plot_8
dev.off()


## 2017-04-01 ##

a.min.04.ft = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa8 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.ft, by = "prio.grid")
aa8$v[aa8$v == 0] <- NA

plot_9 = ggplot(data = aa8) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_9.pdf")
plot_9
dev.off()


a.min.04.pk = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa9 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.pk, by = "prio.grid")
aa9$v[aa9$v == 0] <- NA

plot_10 = ggplot(data = aa9) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_10.pdf")
plot_10
dev.off()


## 2017-05-01 ##

a.min.05.ft = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa10 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.ft, by = "prio.grid")
aa10$v[aa10$v == 0] <- NA

plot_11 = ggplot(data = aa10) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_11.pdf")
plot_11
dev.off()


a.min.05.pk = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed))

aa11 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.pk, by = "prio.grid")
aa11$v[aa11$v == 0] <- NA

plot_12 = ggplot(data = aa11) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_12.pdf")
plot_12
dev.off()




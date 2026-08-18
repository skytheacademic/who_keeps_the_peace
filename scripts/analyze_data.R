# Who Keeps the Peace? #
# Data analysis and plotting #
# By: Sky Kunkel
#
# This script is organized to follow the order of the paper, as of 07/15/2026. Each figure/table is
# tagged with its LaTeX label so replicators can line the output up with the manuscript:
#   Research Design ...... Fig  map_prop            [fig:map_prop]
#   Gendered Effects ..... Tab  hyp_1               [tab:hyp_1]
#                          Fig  2sls binary/count   [fig:5_cont] / [fig:all_cont]
#                          Tab  hyp_2b              [tab:hyp_2b]
#                          Fig  match_OR            [fig:match]
#   Appendix ............. Tab  1st_stage_count     [tab:1st_stage_count]
#                          Tab  1st_stage_prop      [tab:1st_stage_prop]
#                          Tab  IV models           [tab:hyp_1-2a]
#                          Tab  hyp 3               [tab:hyp_3]
#                          Tab  twfe robustness      [tab:twfe_robustness_count/prop]
#                          Tab  gov OSV             [tab:hyp_1_gov / hyp_2b_gov / hyp_2a_gov]
#                          Tab  interaction         [tab:app_hyp_1]
#                          Fig  cutoff binary/count [fig:cutoff_test_binary/count]
# The loveplot [fig:loveplot] is produced in match_data.R. The job-talk presentation figures
# live on the `job-talk` branch (make_slides.R).

#### load libraries, read data ####
library(groundhog)
groundhog.day <- "2026-06-01"
pkgs <- c("tidyverse", "magrittr", "ggpubr", "ggiraphExtra", "coefplot",
          "stargazer", "lmtest", "sandwich", "ggeffects", "MASS",
          "jtools", "broom.mixed", "lfe", "marginaleffects", "fixest", "sf")
groundhog.library(pkgs, groundhog.day)

# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

a = readRDS("./data/kunkel_which_pks.rds")
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019))
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)

############################################################
## Research Design -- Figure [fig:map_prop]               ##
############################################################
# Descriptive map: proportion of women among all Ch. VII peacekeepers deployed, aggregated by
# PRIO grid cell. 
a_map = readRDS("./data/kunkel_which_pks.rds") %>%
  as.data.frame()

df = a_map %>%
  group_by(prio.grid) %>%
  summarize(f_pko_deployed = sum(radpko_f_pko_deployed), m_pko_deployed = sum(radpko_m_pko_deployed),
            violence = sum(ucdp_reb_vac_all, ucdp_gov_vac_all))

prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", # get prio shapefiles
                    stringsAsFactors = F)
afr_shp  <- st_read(dsn = "./data/gadm/africa", layer = "afr_g2014_2013_0", # get Africa shapefiles
                    stringsAsFactors = F)

df$gid = df$prio.grid
df$prio.grid = NULL
df = left_join(df, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  dplyr::select(-c("geometry", "col", "row"))
df_pk = df %>%
  drop_na(f_pko_deployed, m_pko_deployed)
df_pk$prop_women = (df_pk$f_pko_deployed) / (df_pk$f_pko_deployed + df_pk$m_pko_deployed)
df_pk = left_join(df_pk, prio_shp)
df_pk = na.omit(df_pk)

z =
  ggplot() +
  geom_sf(aes(geometry = afr_shp$geometry), alpha = 0.3, fill = NA) +
  geom_sf(aes(fill = df_pk$prop_women, geometry = df_pk$geometry)) +
  scale_fill_gradient(low = "#f6f6f6", high = "#9E314B", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits = c(0, 0.168)) +
  labs(fill = "Proportion Women") +
  xlim(-14, 37) + ylim(-12, 21) +
  theme_void()

pdf("./results/map_prop.pdf")
z +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.3, 0.28),
        legend.margin = margin(10, 10, 10, 10))
dev.off()

# clean up environment
rm(a_map, df, df_pk, prio_shp, afr_shp, z); gc()

##########################
## TWFE Models - START  ##
##########################

####### Hypothesis 1 -- Table [tab:hyp_1] #########

reg1 = felm(formula = ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid |
              0 | prio.grid, data = a)
summary(reg1)

reg2 = felm(formula = ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid |
              0 | prio.grid, data = a)
summary(reg2)

stargazer(reg1, reg2, style = "AJPS", title = "TWFE Models Testing the Count of Peacekeepers",
          label = "tab:hyp_1", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Women PKs Deployed", "Men PKs Deployed"))

## 2SLS -- Figures [fig:5_cont] (binary) and [fig:all_cont] (count) ##
# first stage #
first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

# second stage #

### Continuous ###
reg1 = lm(ucdp_reb_vac_5 ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
            radpko_pko_lag + viol_6,
          data = a)
names(reg1$coefficients) = c("(Intercept)", "Fem. PKs", "Male PKs",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence Lag")
se_reg1 <- round(coeftest(reg1, vcov = vcovPL(reg1, cluster = a$prio.grid)),4)
se_reg1

reg2 = lm(ucdp_reb_vac_all ~ iv_treat_f + iv_treat_m + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
            radpko_pko_lag + viol_6,
          data = a)
names(reg2$coefficients) = c("(Intercept)", "Fem. PKs", "Male PKs",
                             "Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                             "PK Lag", "Violence Lag")
se_reg2 <- round(coeftest(reg2, vcov = vcovPL(reg2, cluster = a$prio.grid)),4)
se_reg2

## plot model outputs ##
# pdf("./results/2sls_binaryoutcome_totalPKs.pdf", height = 3, width = 7)
# plot_summs(se_reg1, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
#                                    "PK Lag", "Violence Lag"))
# dev.off()

# pdf("./results/2sls_countoutcome_totalPKs.pdf")
# plot_summs(se_reg2, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
#                                    "PK Lag", "Violence Lag"))
# dev.off()

#### test new plotting method ####
td = tidy(se_reg1) %>%
  mutate(
    conf.low  = estimate - qnorm(0.975) * std.error,
    conf.high = estimate + qnorm(0.975) * std.error
  ) %>%
  mutate(
    label = recode(term,
      "Fem. PKs"   = "Female PKs",
      "Male PKs"   = "Male PKs",
      "(Intercept)"= "(Intercept)",
      "Avg. Mountain" = "Avg. Mountain",
      "Travel Time Nearest City" = "Travel Time\nNearest City",
      "Perc. Urban"  = "Perc. Urban",
      "PK Lag"       = "PK Lag",
      "Violence Lag" = "Violence Lag"
    ),
    group = case_when(
      label %in% c("Female PKs","Male PKs") ~ "treat",
      label == "(Intercept)"              ~ "intercept",
      TRUE                                ~ "control"
    ),
    label = fct_relevel(label,
      "(Intercept)", "Female PKs","Male PKs",
      "Avg. Mountain","Travel Time\nNearest City","Perc. Urban","PK Lag","Violence Lag"
    )
  )

pdf("./results/2sls_binaryoutcome_totalPKs.pdf", height = 4, width = 6)
ggplot(td, aes(x = estimate, y = label, color = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(treat = "#5B92E5", control = "gray70", intercept = "black")) +
  scale_y_discrete(limits = rev) +
  labs(x = "Estimate (binary)", y = NULL) +
  theme_pubr() +
  theme(legend.position = "none")
dev.off()

td2 = tidy(se_reg2) %>%
  mutate(
    conf.low  = estimate - qnorm(0.975) * std.error,
    conf.high = estimate + qnorm(0.975) * std.error
  ) %>%
  mutate(
    label = recode(term,
      "Fem. PKs"   = "Female PKs",
      "Male PKs"   = "Male PKs",
      "(Intercept)"= "(Intercept)",
      "Avg. Mountain" = "Avg. Mountain",
      "Travel Time Nearest City" = "Travel Time\nNearest City",
      "Perc. Urban"  = "Perc. Urban",
      "PK Lag"       = "PK Lag",
      "Violence Lag" = "Violence Lag"
    ),
    group = case_when(
      label %in% c("Female PKs","Male PKs") ~ "treat",
      label == "(Intercept)"              ~ "intercept",
      TRUE                                ~ "control"
    ),
    label = fct_relevel(label,
      "(Intercept)", "Female PKs","Male PKs",
      "Avg. Mountain","Travel Time\nNearest City","Perc. Urban","PK Lag","Violence Lag"
    )
  )

pdf("./results/2sls_countoutcome_totalPKs.pdf", height = 4, width = 6)
ggplot(td2, aes(x = estimate, y = label, color = group)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0, linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(treat = "#5B92E5", control = "gray70", intercept = "black")) +
  scale_y_discrete(limits = rev) +
  labs(x = "Estimate (count)", y = NULL) +
  theme_pubr() +
  theme(legend.position = "none")
dev.off()



####### Hypothesis 2a -- Table [tab:hyp_2b] #########
reg3 = felm(formula = ucdp_reb_vac_5 ~ radpko_f_prop | time + prio.grid |
              0 | prio.grid, data = a)
summary(reg3)
## Note: I'm using plot_predictions for proportional plot, which does not support felm models. For plotting,
  # I'm using feols from the fixest package. Each model produces identical results, although felm
  # rounds non-estimate values (e.g., p-values).
  # felm results  - estimate: -0.004403, p-value - 0.0247 *
  # feols results - estimate: -0.004403, p-value - 0.024718 *
reg3_plot = feols(fml = (ucdp_reb_vac_5 ~ radpko_f_prop | time + prio.grid), data = a, cluster = "prio.grid")
#######################
## not sure why this is producing different plot?? is it because I fixed the data?
#######################
reg4 = felm(formula = ucdp_reb_vac_all ~ radpko_f_prop | time + prio.grid |
              0 | prio.grid, data = a)
summary(reg4)

## predicted Pr(violence) when prop deployed increases
# women
# pdf("./results/prop_women_pr_death_pred.pdf", height = 10, width = 10)
plot_predictions(reg3_plot, condition = "radpko_f_prop", vcov = FALSE) +  # vcov=FALSE: newer marginaleffects refuses SEs on FE models
  xlab("Proportion Female Peacekeepers Deployed") + ylab("Predicted Pr(Civilian) Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24)) +
  xlim(c(0, 0.20001)) +
  ylim(c(-0.0075, 0.012))
# dev.off()

stargazer(reg3, reg4, style = "AJPS", title = "TWFE Models Testing the Proportion of Peacekeepers",
          label = "tab:hyp_2b", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Prop. Women Deployed"))

##########################
## TWFE Models - END    ##
##########################

####### Hypothesis 2b -- Figure [fig:match] (matched odds ratios) #########
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

### Plot ###
coef_match = c("Gender-mixed PK Unit", "Travel Time Nearest City", #excluding perc. urban and PK lag (aesthetics)
               "Population Sum", "Population Density", "Violence 6 Months Before")

pdf("./results/match_OR.pdf", width = 10, height = 8)
plot_summs(se_reg11, se_reg12, exp = T, coefs = coef_match, model.names =
             c("Binary", "Count"),
           legend.title = "Model by\nOutcome") +
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18),
        axis.title.x.bottom = element_text(size = 22), legend.text=element_text(size=18),
        legend.title = element_text(size=18)) +
  xlab("Odds Ratios") + scale_x_continuous(limits = c(0.1, 1.2))
dev.off()
# Slide-only variants of these matched models (unmatch_OR, match_OR_binary, match_predicted_*)
# are generated by make_slides.R on the job-talk branch.

# free the large negbin model objects; keep the se_reg* matrices for Table [tab:hyp_3] below
rm(reg9, reg10, reg11, reg12); gc()

#### Appendix ####
### IV robustness ###

# 2SLS #
# first stage #
first.stage_f = lm(radpko_f_pko_deployed ~ f_iv, data = a)
first.stage_m = lm(radpko_m_pko_deployed ~ m_iv, data = a)

iv_treat_f = first.stage_f$fitted
iv_treat_m = first.stage_m$fitted

# second stage #

### Continuous ###
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

### Prop ###
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

# First stage tables -- Tables [tab:1st_stage_count] and [tab:1st_stage_prop]
stargazer(first.stage_f, first.stage_m, style = "APSR", title = "1st Stage of Count Instrument",
          dep.var.labels= c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_count",
          covariate.labels = c("Women in Africa x Distance to Capital", "Men in Africa x Distance to Capital"))

stargazer(first.stage_f_prop, style = "APSR", title = "1st State of Prop. Instrument",
          dep.var.labels = c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_prop",
          covariate.labels = c("Prop. Women in Africa x Distance to Capital", "Prop. Men in Africa x Distance to Capital"))

# Model table -- Table [tab:hyp_1-2a]
stargazer(se_reg1, se_reg2, se_reg3, se_reg4, style = "APSR", title = "IV Models for Hypotheses 1/2a",
          column.separate = c(2,2), label = "tab:hyp_1-2a") # IV analyses
# (the IV odds-ratio figures 2sls_*_propPKs are generated by make_slides.R on the job-talk branch)

### Hypothesis 3 -- Table [tab:hyp_3] ###
stargazer(se_reg9, se_reg10, se_reg11, se_reg12, style = "APSR", title = "Models Testing Hypothesis 3",
          column.labels = c("Logit"), column.separate = c(4), label = "tab:hyp_3") # Logit analyses


#### Matching - full tables (writes results/logit.txt) ####
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


###################
# TWFE Robustness -- Tables [tab:twfe_robustness_count] and [tab:twfe_robustness_prop] #
###################

# Truncate each grid at the end of its 1st deployment of peacekeeper presence
# Grids that never host peacekeepers keep their full panel. A grid that peacekeepers 
# leave and later re-enter would otherwise re-enter the sample as a fresh treatment, 
# creating a "forbidden comparison"; see Callaway and Sant'Anna (and other recent DiD lit)
# for more context.
pko_dep = a %>%
  arrange(prio.grid, time) %>%
  group_by(prio.grid) %>%
  summarize(pko_leave = if (!any(t_ind == 1)) NA_real_ else {
    tf = min(time[t_ind == 1])
    off = time[time > tf & t_ind == 0]
    if (length(off) == 0) max(time) else min(off) - 1
  }, .groups = "drop")

a = a %>%
  left_join(pko_dep, by = "prio.grid") %>%
  filter(is.na(pko_leave) | time <= pko_leave)
rm(pko_dep)

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
          label = "tab:twfe_robustness_count", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Women PKs Deployed", "Men PKs Deployed"))

stargazer(reg3, reg4, style = "AJPS", title = "TWFE Models Testing the Proportion of Peacekeepers",
          label = "tab:twfe_robustness_prop", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
          covariate.labels = c("Prop. Women Deployed", "Prop. Men Deployed"))



### GOV OSV ROBUSTNESS CHECK -- Tables [tab:hyp_1_gov], [tab:hyp_2b_gov], [tab:hyp_2a_gov] ###
rm(list = ls())
gc()
a = readRDS("./data/kunkel_which_pks.rds")
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019))
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)

reg1 = felm(formula = ucdp_gov_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg1)

reg2 = felm(formula = ucdp_gov_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg2)

####### Hypothesis 2a #########
reg3 = felm(formula = ucdp_gov_vac_5 ~ radpko_f_prop | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg3)

reg4 = felm(formula = ucdp_gov_vac_all ~ radpko_f_prop | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg4)

stargazer(reg1, reg2, style = "AJPS", title = "TWFE Models Testing the Count of Peacekeepers",
label = "tab:hyp_1_gov", dep.var.labels = c("Gov OSV (B)", "Gov OSV (C)"),
covariate.labels = c("Women PKs Deployed", "Men PKs Deployed"))

stargazer(reg3, reg4, style = "AJPS", title = "TWFE Models Testing the Proportion of Peacekeepers",
label = "tab:hyp_2b_gov", dep.var.labels = c("Gov OSV (B)", "Gov OSV (C)"),
covariate.labels = c("Prop. Women Deployed", "Prop. Men Deployed"))


# Matched Data - Logit #
reg11 = glm(ucdp_gov_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
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

stargazer(se_reg11, se_reg12, style = "APSR", title = "Matched Logit Models",
          label = "tab:hyp_2a_gov")


##### Interaction: Proportion Fem PKs deployed * total -- Table [tab:app_hyp_1] #####

reg1 = felm(formula = ucdp_reb_vac_5 ~ radpko_f_prop*radpko_f_pko_deployed | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg1)

reg2 = felm(formula = ucdp_reb_vac_all ~ radpko_f_prop*radpko_f_pko_deployed | time + prio.grid |
  0 | prio.grid, data = a)
summary(reg2)

stargazer(reg1, reg2, style = "AJPS", title = "TWFE Models: Proportion interacted with Total Female PKs Deployed",
label = "tab:app_hyp_1", dep.var.labels = c("Rebel OSV (B)", "Rebel OSV (C)"),
covariate.labels = c("Prop. Women Deployed", "Female PKs Deployed", "Prop. * Total Fem."))

## data plotting (exploratory; not a paper figure) ##
data_long <- a %>%
  dplyr::select(radpko_f_untrp, radpko_f_unpol, radpko_f_unmob) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  filter(value > 0)

# 1. Scatter Plot
ggplot(data_long, aes(x = variable, y = value, color = variable)) +
  geom_jitter(width = 0.2) +
  theme_minimal() +
  labs(title = "Scatter Plot of Variables > 0")

# Transform data to long format with corresponding deployed counts
data_long <- a %>%
  mutate(row_id = row_number()) %>%  # Create a unique ID for joining
  pivot_longer(cols = c(radpko_f_untrp, radpko_f_unpol, radpko_f_unmob),
               names_to = "variable",
               values_to = "value") %>%
  mutate(radpko_f_prop = a$radpko_f_prop[row_id]) %>%
  filter(radpko_f_prop > 0)


ggplot(data_long, aes(x = value, y = radpko_f_prop, color = variable)) +
  geom_point(alpha = 0.6, size = 3) +
  theme_minimal() +
  labs(title = "Scatter Plot: Variable Values vs. radpko_f_prop",
       x = "Values of Variables (>0)",
       y = "radpko_f_prop",
       color = "Variable")

#### Cutoff Testing -- Figures [fig:cutoff_test_binary] and [fig:cutoff_test_count] ####
d_450 = readRDS("./data/cutoff_robustness/kunkel_wpks_matched_450_gender.rds")
d_475 = readRDS("./data/cutoff_robustness/kunkel_wpks_matched_475_gender.rds")
d_500 = readRDS("./data/kunkel_wpks_matched_gender.rds")
d_525 = readRDS("./data/cutoff_robustness/kunkel_wpks_matched_525_gender.rds")
d_550 = readRDS("./data/cutoff_robustness/kunkel_wpks_matched_550_gender.rds")

# 0.450 #
reg450 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_450, family = negative.binomial(theta = 1))
se_reg450 <- round(coeftest(reg450, vcov = vcovPL(reg450, cluster = d_450$prio.grid)),4)
se_reg450

# 0.475 #
reg475 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_475, family = negative.binomial(theta = 1))
se_reg475 <- round(coeftest(reg475, vcov = vcovPL(reg475, cluster = d_475$prio.grid)),4)
se_reg475

# 0.500 #
reg500 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_500, family = negative.binomial(theta = 1))
se_reg500 <- round(coeftest(reg500, vcov = vcovPL(reg500, cluster = d_500$prio.grid)),4)
se_reg500

# 0.525 #
reg525 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_525, family = negative.binomial(theta = 1))
se_reg525 <- round(coeftest(reg525, vcov = vcovPL(reg525, cluster = d_525$prio.grid)),4)
se_reg525

# 0.550 #
reg550 = glm(ucdp_reb_vac_5 ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_550, family = negative.binomial(theta = 1))
se_reg550 <- round(coeftest(reg550, vcov = vcovPL(reg550, cluster = d_550$prio.grid)),4)
se_reg550

extract_estimates <- function(se_reg, var) {
  estimate <- se_reg[var, "Estimate"]
  se <- se_reg[var, "Std. Error"]
  ci_lower <- estimate - 1.96 * se
  ci_upper <- estimate + 1.96 * se

  tibble(
    model = deparse(substitute(se_reg)),
    estimate = estimate,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

df_plot <- bind_rows(
  extract_estimates(se_reg450, "t_bal") %>% mutate(data = "45th"),
  extract_estimates(se_reg475, "t_bal") %>% mutate(data = "47.5"),
  extract_estimates(se_reg500, "t_bal") %>% mutate(data = "50th"),
  extract_estimates(se_reg525, "t_bal") %>% mutate(data = "52.5"),
  extract_estimates(se_reg550, "t_bal") %>% mutate(data = "55th")
)

pdf("./results/cutoff_testing_binary.pdf")
ggplot(df_plot, aes(x = data, y = estimate, color = data)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("45th" = "blue", "47.5" = "blue", "50th" = "black", "52.5" = "blue", "55th" = "blue")) +
  theme_pubclean() +
  labs(x = "Percentile for matching cutoff", y = "Estimate with 95% CI", title = "") +
  coord_cartesian(ylim = c(min(df_plot$ci_lower) - 0.1, 0.5)) +
  theme(legend.position = "none")
dev.off()

rm(list = setdiff(ls(), c("d_450", "d_475", "d_500", "d_525", "d_550", "extract_estimates")))

## count outcome ##
reg450 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_450, family = negative.binomial(theta = 1))
se_reg450 <- round(coeftest(reg450, vcov = vcovPL(reg450, cluster = d_450$prio.grid)),4)
se_reg450

reg475 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_475, family = negative.binomial(theta = 1))
se_reg475 <- round(coeftest(reg475, vcov = vcovPL(reg475, cluster = d_475$prio.grid)),4)
se_reg475

reg500 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_500, family = negative.binomial(theta = 1))
se_reg500 <- round(coeftest(reg500, vcov = vcovPL(reg500, cluster = d_500$prio.grid)),4)
se_reg500

reg525 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_525, family = negative.binomial(theta = 1))
se_reg525 <- round(coeftest(reg525, vcov = vcovPL(reg525, cluster = d_525$prio.grid)),4)
se_reg525

reg550 = glm(ucdp_reb_vac_all ~ t_bal + prio_mountains_mean + prio_ttime_mean + prio_urban_gc +
  prio_nlights_calib_mean + prio_pop_gpw_sum + prio_pop.dens + radpko_pko_lag_any + viol_6,
data = d_550, family = negative.binomial(theta = 1))
se_reg550 <- round(coeftest(reg550, vcov = vcovPL(reg550, cluster = d_550$prio.grid)),4)
se_reg550

df_plot_cont <- bind_rows(
  extract_estimates(se_reg450, "t_bal") %>% mutate(data = "45th"),
  extract_estimates(se_reg475, "t_bal") %>% mutate(data = "47.5"),
  extract_estimates(se_reg500, "t_bal") %>% mutate(data = "50th"),
  extract_estimates(se_reg525, "t_bal") %>% mutate(data = "52.5"),
  extract_estimates(se_reg550, "t_bal") %>% mutate(data = "55th")
)

pdf("./results/cutoff_testing_cont.pdf")
ggplot(df_plot_cont, aes(x = data, y = estimate, color = data)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("45th" = "blue", "47.5" = "blue", "50th" = "black", "52.5" = "blue", "55th" = "blue")) +
  theme_pubclean() +
  labs(x = "Percentile for matching cutoff", y = "Estimate with 95% CI", title = "") +
  # coord_cartesian(ylim = c(min(df_plot_cont$ci_lower) - 0.1, 0.5)) +
  theme(legend.position = "none")
dev.off()

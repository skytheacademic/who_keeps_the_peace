# Who Keeps the Peace? #
# Data analysis and plotting #
# By: Sky Kunkel

#### load libraries, read data ####
library(tidyverse)
library(magrittr)
library(ggpubr)
library(ggiraphExtra)
library(coefplot)
library(stargazer)
library(lmtest)
library(sandwich)
library(ggeffects)
library(MASS)
library(jtools)
library(broom.mixed)
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

# stargazer(se_reg9, se_reg10, se_reg11, se_reg12, style = "APSR", title = "Matched Logit Models",
#           label = "tab:logit_match",
#           covariate.labels = c("Gender-mixed PK Unit", "Unbalanced PK Unit"))

### Plots ###
# effect_plot(reg1, pred = radpko_f_pko_deployed, interval = T, plot.points = T, jitter = 0.05)

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

pdf("./results/match_OR.pdf", width = 10, height = 8)
plot_summs(se_reg11, se_reg12, exp = T, coefs = coef_match, model.names = 
             c("Binary", "Count"), 
           legend.title = "Model by\nOutcome") + 
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18), 
        axis.title.x.bottom = element_text(size = 22), legend.text=element_text(size=18),
        legend.title = element_text(size=18)) +
  xlab("Odds Ratios") + scale_x_continuous(limits = c(0.1, 1.15))
dev.off()

stargazer(se_reg11, se_reg12, style = "APSR", title = "Matched Logit Models",
          label = "tab:logit_match",
          covariate.labels = c("Gender-mixed PK Unit", "Unbalanced PK Unit"))

pdf("./results/match_OR_binary.pdf", width = 10, height = 8)
plot_summs(se_reg11, exp = T, coefs = coef_match) + 
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18), 
        axis.title.x.bottom = element_text(size = 22), legend.text=element_text(size=18),
        legend.title = element_text(size=18)) +
  xlab("Odds Ratios") + scale_x_continuous(limits = c(0.1, 1.1))
dev.off()

svg("./results/images/match_OR.svg")
plot_summs(se_reg11, se_reg12, exp = T, coefs = coef_match, model.names = 
             c("Binary", "Count"), legend.title = "Model by Outcome")
dev.off()

### try new way to see models ###
library(marginaleffects)
gg_reg11 <- ggpredict(reg11, terms = "t_bal")
gg_reg12 <- ggpredict(reg12, terms = "t_bal")

pdf("./results/match_predicted_binary.pdf", width = 10, height = 8)
plot_predictions(reg11, condition = "t_bal") +
  xlab("Gender-mixed PK Unit") +
  ylab("Predicted Pr(Civilian Deaths by Rebels)") +
  theme_pubclean() +
  theme(
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  )
dev.off()

pdf("./results/match_predicted_total.pdf", width = 10, height = 8)
plot_predictions(reg12, condition = "t_bal") +
  xlab("Gender-mixed PK Unit") +
  ylab("Predicted Civilian Deaths by Rebels") +
  theme_pubclean() +
  theme(
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  )
dev.off()

plot(gg_reg11)
plot(gg_reg12)









######### plot it from scratch ############
library(ggstance)    # For horizontal error bars

# Convert coeftest output to data frames
tidy_reg11 <- data.frame(
  term = rownames(se_reg11),
  estimate = se_reg11[, "Estimate"],
  std.error = se_reg11[, "Std. Error"],
  statistic = se_reg11[, "z value"],
  p.value = se_reg11[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

tidy_reg12 <- data.frame(
  term = rownames(se_reg12),
  estimate = se_reg12[, "Estimate"],
  std.error = se_reg12[, "Std. Error"],
  statistic = se_reg12[, "z value"],
  p.value = se_reg12[, "Pr(>|z|)"],
  stringsAsFactors = FALSE
)

# Compute confidence intervals
alpha <- 0.05  # Significance level
z_value <- qnorm(1 - alpha / 2)  # Critical value for two-tailed test

# For reg11
tidy_reg11$conf.low <- tidy_reg11$estimate - z_value * tidy_reg11$std.error
tidy_reg11$conf.high <- tidy_reg11$estimate + z_value * tidy_reg11$std.error

# For reg12
tidy_reg12$conf.low <- tidy_reg12$estimate - z_value * tidy_reg12$std.error
tidy_reg12$conf.high <- tidy_reg12$estimate + z_value * tidy_reg12$std.error

# Exponentiate estimates and confidence intervals for reg11
tidy_reg11 <- tidy_reg11 %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

# Exponentiate estimates and confidence intervals for reg12
tidy_reg12 <- tidy_reg12 %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

# Create a data frame of coefficient names
coef_names <- data.frame(
  term = names(reg11$coefficients),
  coef_name = c("(Intercept)", "Gender-mixed\nPK Unit",
                "Avg. Mountain", "Travel Time\nNearest City", "Perc. Urban",
                "Night Lights", "Population Sum", "Population Density",
                "PK Lag", "Violence 6\nMonths Before"),
  stringsAsFactors = FALSE
)

# Merge the tidy data frames with the coefficient names
tidy_reg11 <- left_join(tidy_reg11, coef_names, by = "term")
tidy_reg12 <- left_join(tidy_reg12, coef_names, by = "term")

# Add model identifiers
tidy_reg11$model <- "Binary"
tidy_reg12$model <- "Count"

# Combine the data frames
tidy_all <- bind_rows(tidy_reg11, tidy_reg12)

# Define the coefficients to plot
coef_match <- c("Gender-mixed\nPK Unit", "Travel Time\nNearest City",
                "Population Sum", "Population Density", "Violence 6\nMonths Before")

# Filter to include only the coefficients of interest
tidy_all <- tidy_all %>%
  filter(coef_name %in% coef_match)

# Create an ordered factor for the coefficients
tidy_all$coef_name <- factor(tidy_all$coef_name, levels = rev(coef_match))

# Assign integer positions to the coefficients for consistent plotting
coef_positions <- data.frame(
  coef_name = rev(coef_match),
  ypos = 1:length(coef_match)
)

# Merge positions into the data
tidy_all <- left_join(tidy_all, coef_positions, by = "coef_name")

# Create data for the first plot (set reg12 estimates to NA)
tidy_plot1 <- tidy_all %>%
  mutate(
    estimate = ifelse(model == "Count", NA, estimate),
    conf.low = ifelse(model == "Count", NA, conf.low),
    conf.high = ifelse(model == "Count", NA, conf.high)
  )


# Create the first plot
plot1 <- ggplot(tidy_plot1, aes(x = estimate, y = ypos, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointrangeh(position = position_dodgev(height = 0.5), na.rm = TRUE, size = 1) +
  scale_y_continuous(breaks = coef_positions$ypos, labels = coef_positions$coef_name) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(limits = c(0.1, 1.15)) +
  labs(x = "Odds Ratios (log scale)", y = "", color = "Model by\nOutcome") +
  theme_pubclean() +
  theme(
    # Existing theme settings
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 24),
    legend.position = "right",
    # Make legend key background transparent
    legend.key = element_rect(fill = "transparent", color = NA)
  ) +
  scale_color_manual(values = c("Binary" = "#49B7FC", "Count" = "#F2A663"))


# Create the second plot with both models
plot2 <- ggplot(tidy_all, aes(x = estimate, y = ypos, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_pointrangeh(position = position_dodgev(height = 0.5), na.rm = TRUE, size = 1) +
  scale_y_continuous(breaks = coef_positions$ypos, labels = coef_positions$coef_name) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(limits = c(0.1, 1.15)) +
  labs(x = "Odds Ratios (log scale)", y = "", color = "Model by\nOutcome") +
  theme_pubclean() +
  theme(
    # Existing theme settings
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 24),
    legend.position = "right",
    # Make legend key background transparent
    legend.key = element_rect(fill = "transparent", color = NA)
  ) +
  scale_color_manual(values = c("Binary" = "#49B7FC", "Count" = "#F2A663"))

# Save the first plot
ggsave("./results/match_OR_slide1.pdf", plot1, width = 12, height = 8, units = "in")

# Save the second plot
ggsave("./results/match_OR_slide2.pdf", plot2, width = 12, height = 8, units = "in")

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

stargazer(first.stage_f, first.stage_m, style = "APSR", title = "1st Stage of Count Instrument", 
          dep.var.labels= c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_count",
          covariate.labels = c("Women in Africa x Distance to Capital", "Men in Africa x Distance to Capital"))

stargazer(first.stage_f_prop, style = "APSR", title = "1st State of Prop. Instrument",
          dep.var.labels = c("Women Deployed", "Men Deployed"), label = "tab:1st_stage_prop", 
          covariate.labels = c("Prop. Women in Africa x Distance to Capital", "Prop. Men in Africa x Distance to Capital"))

# Model tables
stargazer(se_reg1, se_reg2, se_reg3, se_reg4, style = "APSR", title = "IV Models for Hypotheses 1/2a",
          column.separate = c(2,2), label = "tab:hyp_1-2a") # IV analyses
stargazer(se_reg9, se_reg10, se_reg11, se_reg12, style = "APSR", title = "Models Testing Hypothesis 3",
          column.labels = c("Logit"), column.separate = c(4), label = "tab:hyp_3") # Logit analyses


pdf("./results/2sls_binaryoutcome_totalPKs.pdf", height = 5, width = 7)
plot_summs(se_reg1, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                                   "PK Lag", "Violence 6 Months Before"))
dev.off()

pdf("./results/2sls_countoutcome_totalPKs.pdf")
plot_summs(se_reg2, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                                   "PK Lag", "Violence 6 Months Before"))
dev.off()

pdf("./results/2sls_binaryoutcome_propPKs.pdf")
plot_summs(se_reg3, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                                   "PK Lag", "Violence 6 Months Before"))
dev.off()

pdf("./results/2sls_countoutcome_propPKs.pdf")
plot_summs(se_reg4, omit.coefs = c("Avg. Mountain", "Travel Time Nearest City", "Perc. Urban",
                                   "PK Lag", "Violence 6 Months Before"))
dev.off()




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


##############
## Appendix ##
##############

# post_treatment is coded as 1 after treatment

a = a %>%
  filter(post_treatment==0)

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

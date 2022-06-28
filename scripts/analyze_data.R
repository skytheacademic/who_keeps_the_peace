# Conflict Grids & Peacekeeping #
# By: Adam Kunkel

### load libraries ###
library(tidygeocoder)
library(tidyverse); library(viridis)
library(gdata); library(designmatch) 

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(gurobi); library(MASS); library(lme4); library(vtable)
library(sensitivitymw); library(lme4)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
a = readRDS("./data/kunkel_cg.rds")

##########################################
        ### Regression Analysis ###
##########################################

### Logit Model Aggregated###
logit1 = glm.nb(formula = a$event.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$pko_lag)
summary(logit1)

logit2 = glm.nb(formula = a$death ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$pko_lag)
summary(logit2)

logit3 = glm.nb(formula = a$event.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                  a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit3)

logit4 = glm.nb(formula = a$death ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                  a$pop.dens + a$pko_lag)
summary(logit4)

########### testing w/ t_ind ###############

logit5 = glm.nb(a$event ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
            a$f_unpol + a$f_unmob + a$pko_lag)
summary(logit5)

logit6 = glm.nb(a$fatalities ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
            a$f_unpol + a$f_unmob + a$pko_lag)
summary(logit6)

logit7 = glm.nb(a$event ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                  a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit7)

logit8 = glm.nb(a$fate.5 ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum +
                  a$pop.dens + a$pko_lag)

# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens

summary(logit8)



###########################################
### Logit Model Dis-aggregated by actor ###
###########################################

# Gov OSV #
logit9 = glm.nb(a$gov_event ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                  a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit9)

logit10 = glm.nb(a$gov_death ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                   a$pop.dens + a$pko_lag)
summary(logit10)

########### testing w/ t_ind ###############
logit11 = glm.nb(a$gov_event.b ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                  a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit11)

logit12 = glm.nb(a$gov_death.b ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + 
                   a$f_untrp + a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                   a$pop_gpw_sum + a$pop.dens + a$pko_lag)
# switching to 5 death threshold for this model since regular continuous variable won't work

# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens

summary(logit12)

# results from both of these indicate a possible selection effect; i.e., peacekeepers go where there is violence, so identifying it 
# by just the treatment biases the estimate of violence higher
# try running again after card match


# Rebel OSV #
logit13 = glm.nb(a$reb_event.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                  a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit13)

logit14 = glm.nb(a$reb_death.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                   a$pop.dens + a$pko_lag)
summary(logit14)

########### testing w/ t_ind ###############
logit15 = glm.nb(a$reb_event.b ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                   a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit15)

# should there be an interaction effect between t_ind and other peacekeeping variables?
logit16 = glm.nb(a$reb_death.b ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + 
                   a$f_untrp + a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                   a$pop_gpw_sum + a$pop.dens + a$pko_lag)
# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens

summary(logit16)



# Gov OSV #
logit9.1 = glm.nb(a$gov_event.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                   a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit9.1)

logit10.1 = glm.nb(a$gov_death.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                     a$pop.dens + a$pko_lag)
summary(logit10.1)

# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens


# Rebel OSV #
logit13.1 = glm.nb(a$reb_event.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                   a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(logit13.1) # logit13.1 doesn't run, likely not enough data

logit14.1 = glm.nb(a$reb_death.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                   a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                     a$pop.dens + a$pko_lag)
summary(logit14.1)

stargazer(logit9.1, logit10.1, logit14.1, title = "Pre-matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/pre_matched_logit_5.txt")


##########################################
        ### Regression Figures ###
##########################################

stargazer(logit9, logit10, logit13, logit14, title = "Pre-matched Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/pre_matched_logit.txt")

# descriptive statistics table #

labs = c("Total PKs deployed", "Troops Total", "Female Troops")
sum = c("mean(x)", "sd(x)", "min(x)", "max(x)")
st(a, group = "mission", vars =c("pko_deployed", "untrp", "f_untrp"), group.long = TRUE,
   col.breaks = 3, labels = labs, summ = sum, out = "latex")


##########################################
        ### Matching Analysis ###
##########################################

a = a[order(a$t_ind, decreasing=TRUE), ]

control.variables = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                          a$pop_gpw_sum, a$pop.dens, a$prec_gpcp)

# keep map stuff at the end of data.frame #
a = a %>% relocate(c("xcoord", "ycoord", "col", "row", "geometry"), .after = last_col())

t_ind = a$t_ind
mom_covs = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                 a$pop_gpw_sum, a$pop.dens, a$prec_gpcp)

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
                        a$pop_gpw_sum, a$pop.dens, a$prec_gpcp)
tab2 = meantab(covs, t_ind, t_id_1, c_id_1)

# Save matched sample 
b = a[c(t_id_1, c_id_1), ]

# this might be for matching: "Moreover, we match for grid characteristics identified 
# in the disaggregate literature of civil war: INFANT MORTALITY RATE, POPULATION, 
# AVERAGE MOUNTAINS, and AVERAGE RAIN PRECIPITATION"


# number of observations before and after matching 

table((a$t_ind))
table(table(t_id_1))
table(table(c_id_1))


# ATE effect using difference in means #

# first, gov forces
mean(b$gov_death.b[b$t_ind==1])
mean(b$gov_death.b[b$t_ind==0])
mean(b$gov_death.b[b$t_ind==1]) - mean(b$gov_death.b[b$t_ind==0])

mean(b$gov_event.b[b$t_ind==1])
mean(b$gov_event.b[b$t_ind==0])
mean(b$gov_event.b[b$t_ind==1]) - mean(b$gov_event.b[b$t_ind==0])

mean(b$gov_death[b$t_ind==1])
mean(b$gov_death[b$t_ind==0])
mean(b$gov_death[b$t_ind==1]) - mean(b$gov_death[b$t_ind==0])

mean(b$gov_event[b$t_ind==1])
mean(b$gov_event[b$t_ind==0])
mean(b$gov_event[b$t_ind==1]) - mean(b$gov_event[b$t_ind==0])

# now, reb forces
mean(b$reb_death.b[b$t_ind==1])
mean(b$reb_death.b[b$t_ind==0])
mean(b$reb_death.b[b$t_ind==1]) - mean(b$reb_death.b[b$t_ind==0])

mean(b$reb_event.b[b$t_ind==1])
mean(b$reb_event.b[b$t_ind==0])
mean(b$reb_event.b[b$t_ind==1]) - mean(b$reb_event.b[b$t_ind==0])

mean(b$reb_death[b$t_ind==1])
mean(b$reb_death[b$t_ind==0])
mean(b$reb_death[b$t_ind==1]) - mean(b$reb_death[b$t_ind==0])

mean(b$reb_event[b$t_ind==1])
mean(b$reb_event[b$t_ind==0])
mean(b$reb_event[b$t_ind==1]) - mean(b$reb_event[b$t_ind==0])


# Sensitivity tests
# gamma = 1
test_d_match1 = data.frame(b$gov_death.b[b$t_ind==1],b$gov_death.b[b$t_ind==0])
colnames(test_d_match1) = c("treated","control")

senmw(test_d_match1,gamma=1,method="t")$pval


# regressions w/ matched sample

# Gov OSV #
logit17 = glm.nb(gov_event.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data = b)
summary(logit17)

logit18 = glm.nb(gov_death.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens,
                 data = b)
summary(logit18)

# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens


# Rebel OSV #
logit19 = glm.nb(reb_event.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data = b)
summary(logit19)

logit20 = glm.nb(reb_death.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens,
                 data = b)
summary(logit20)
# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens

# regression table #
stargazer(logit17, logit18, logit19, logit20, title = "Matched Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/matched_logit.txt")



# same regressions but using 5 death threshold after Fjelde et al. (2019) #

# Gov OSV #
logit21 = glm.nb(gov_event.5 ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data = b)
summary(logit21) # also didn't run, calculated deviance probably became infinite

logit22 = glm.nb(gov_death.5 ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens,
                 data = b)
summary(logit22)

# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens


# Rebel OSV #
logit23 = glm.nb(reb_event.5 ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data = b)
summary(logit23) # same as pre-matched, still won't run

logit24 = glm.nb(reb_death.5 ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens,
                 data = b)
summary(logit24)


stargazer(logit22, logit24, title = "Matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_logit_5.txt")



############################
# 3-level Multilevel Model #
############################

# is treatment binary? if so, use the following.

# GOV OSV #
mlm_3_reg1 = glmer.nb(gov_event.b ~ time * t_ind + (time | ccode:prio.grid) +
       (time | country) + (0 + t_ind + time:t_ind | country), 
     data=a)

# subjects = prio.grid
# therapist = country
# time = time

# use this to figure out code https://rpsychologist.com/r-guide-longitudinal-lme-lmer#three-level-models
# if not, send Shawn an email

gov_event.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens +
  (1 | ccode)
# Rebel OSV #

# 2-Level Multilevel model #

###########################
# random intercept binary #
###########################

# Gov OSV #
ran.int1 = lmer(gov_event.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens +
                  (1 | ccode), data = b)
summary(ran.int1)

ran.int2 = lmer(gov_death.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                   (1 | ccode), data = b)
summary(ran.int2)


# Rebel OSV #
ran.int3 = lmer(reb_event.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + 
                   (1 | ccode), data = b)
summary(ran.int3)

ran.int4 = lmer(reb_death.b ~ units_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + 
                   (1 | ccode), data = b)
summary(ran.int4)


stargazer(ran.int1, ran.int2, ran.int3, ran.int4, title = "Matched MLM Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_b.txt")



###########################
# random intercept continuous #
###########################

# Gov OSV #
ran.int5 = lmer(gov_event ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens +
                  (1 | ccode), data = b)
summary(ran.int5)

ran.int6 = lmer(gov_death ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens +
                  (1 | ccode), data = b)
summary(ran.int6)


# Rebel OSV #
ran.int7 = lmer(reb_event ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + 
                  (1 | ccode), data = b)
summary(ran.int7)

ran.int8 = lmer(reb_death ~ units_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + 
                  (1 | ccode), data = b)
summary(ran.int8)


stargazer(ran.int5, ran.int6, ran.int7, ran.int8, title = "Matched MLM Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_c.txt")





#########################
# Robustness Checks #
#########################

### Clustering Standard Errors at the grid level ###
# create function to cluster SEs
vcovCluster <- function(
    model,
    cluster
)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}

# Gov OSV #
se_reg1 <- lm(gov_event ~ units_deployed + untrp + unpol + unmob + f_untrp +
               f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + urban_gc + 
               nlights_calib_mean + pop_gpw_sum + pop.dens, data=b)
se_reg_c1 <- round(coeftest(se_reg1, vcov = vcovCluster(se_reg1, cluster = b$prio.grid)),4)

se_reg2 <- lm(gov_death ~ units_deployed + untrp + unpol + unmob + f_untrp +
                f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + 
                pop.dens, data=b)
se_reg_c2 <- round(coeftest(se_reg2, vcov = vcovCluster(se_reg2, cluster = b$prio.grid)),4)

# Rebel OSV #
se_reg3 <- lm(greb_event ~ units_deployed + untrp + unpol + unmob + f_untrp +
                f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data=b)
se_reg_c3 <- round(coeftest(se_reg3, vcov = vcovCluster(se_reg3, cluster = b$prio.grid)),4)

se_reg4 <- lm(reb_death ~ units_deployed + untrp + unpol + unmob + f_untrp +
                f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + 
                pop.dens, data=b)
se_reg_c4 <- round(coeftest(se_reg4, vcov = vcovCluster(se_reg4, cluster = b$prio.grid)),4)


#### Measuring Spatial Autocorrelation ####
# Spatial Durbin Model #

prio = st_read(dsn = "./data/prio/priogrid_cellshp",
                layer = "priogrid_cell",
                stringsAsFactors = F) %>%
   mutate(gid = as.character(gid))

 names(prio)[1] = "prio.grid" # rename for merging
prio$prio.grid = as.numeric(prio$prio.grid) # transform the column into numeric so we can join the data

 prio.sp = as(prio, Class = "Spatial")
 nb = poly2nb(prio.sp,
              queen = TRUE,
              row.names = prio.sp$prio.grid)

 # store as list (most modeling packages require this)
 lw = nb2listw(nb, style = "W", zero.policy = TRUE)
 print(lw, zero.policy = TRUE) ## to look at lw contents


 ### summarize by prio grid first ###
 #https://r-spatial.github.io/spatialreg/reference/SLX.html
 sp.durb1 = lmSLX(logit1, data = a, listw = lw)
 # Spatial durbin model says the DV is a function of three things:
   # neighbor DV values
   # our own IV values
   # neighbor IV values

 moran.test(prio.sp$pop_gpw_sum, listw = lw, zero.policy = TRUE) # Moran's I (eye) test
 # the closer the result is to 1, the more spatial dependence there is
 # for more info on this, see here: https://www.youtube.com/watch?v=6qZgchGCMds&ab_channel=BurkeyAcademy
   # and here: https://sites.google.com/site/econometricsacademy/econometrics-models/spatial-econometrics



 
 #### UCDP Robustness Check ####
 

 
 

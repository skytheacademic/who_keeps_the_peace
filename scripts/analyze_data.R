# Conflict Grids & Peacekeeping #
# By: Sky Kunkel

#### load libraries, read data ####
library(tidygeocoder)
library(tidyverse); library(viridis)
library(gdata); library(designmatch) 

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(gurobi); library(MASS); library(lme4); library(vtable)
library(sensitivitymw); library(lme4); library(lmtest); library(sandwich)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
a = readRDS("./data/kunkel_cg.rds")

##########################################################
### Naive Logit Models, no distinction between actors ###
##########################################################

#### Logit Model, continuous treatment####
# naive model
logit1 = glm.nb(event.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag, data = a)
summary(logit1)

logit2 = glm.nb(death ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag, data = a)
summary(logit2)

# model with controls #
logit3 = glm.nb(event.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                data = a)
summary(logit3)

logit4 = glm.nb(death ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + 
                  pop.dens + pko_lag, data = a)
summary(logit4)


#### Logit Model, binary treatment####
logit5 = glm.nb(event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag, data = a)
summary(logit5)

logit6 = glm.nb(death ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag, data = a)
summary(logit6)

# model with controls #
logit7 = glm.nb(event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                data = a)
summary(logit7)

logit8 = glm.nb(death ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + 
                  pop.dens + pko_lag, data = a)
summary(logit8)


##########################################################
### Logit Models, disaggregated by actors ###
##########################################################

#### GOV OSV - Continuous treatment ####
logit9 = glm.nb(gov_event.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                data = a)
summary(logit9)

logit10 = glm.nb(gov_death.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + 
                   pop.dens + pko_lag,
                 data = a)
summary(logit10)

#### GOV OSV - Binary treatment ####
logit11 = glm.nb(gov_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(logit11)

logit12 = glm.nb(gov_death.b ~ t_ind + untrp + unpol + unmob + 
                   f_untrp + f_unpol + f_unmob + mountains_mean + ttime_mean + 
                   pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(logit12)
# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens

#### REB OSV - Continuous Treatment ####
logit13 = glm.nb(reb_event.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(logit13)

logit14 = glm.nb(reb_death.b ~ pko_deployed + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + 
                   pop.dens + pko_lag,
                 data = a)
summary(logit14)

#### REB OSV - Binary Treatment ####
logit15 = glm.nb(reb_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(logit15)

logit16 = glm.nb(reb_death.b ~ t_ind + untrp + unpol + unmob + 
                   f_untrp + f_unpol + f_unmob + mountains_mean + ttime_mean + 
                   pop_gpw_sum + pop.dens + pko_lag,
                 data = a)
summary(logit16)
# not using nlights here because economic activity might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because population density might affect whether a violent event occurs but not whether a death happens



#### Figures and Plots for non-matched regressions ####

stargazer(logit11, logit12, logit15, logit16, title = "Pre-matched Results Pr(Violence)", 
          align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/pre_matched_logit.txt")

# descriptive statistics table #

labs = c("Total PKs deployed", "Troops Total", "Female Troops")
sum = c("mean(x)", "sd(x)", "min(x)", "max(x)")
st(a, group = "mission", vars =c("pko_deployed", "untrp", "f_untrp"), group.long = TRUE,
   col.breaks = 3, labels = labs, summ = sum, out = "latex")

rm(list = setdiff(ls(), "a")) 
gc()

##########################################
        ### Matching Analysis ###
##########################################

a = a[order(a$t_ind, decreasing=TRUE), ]

control.variables = cbind(a$mountains_mean, a$ttime_mean, a$urban_gc, a$nlights_calib_mean, 
                          a$pop_gpw_sum, a$pop.dens, a$prec_gpcp)
t_ind = a$t_ind #treatment
t_id = which(t_ind==1) #treated
c_id = which(t_ind==0) #control
tab1 = meantab(control.variables, t_ind, t_id, c_id)
tab1
# keep map stuff at the end of data.frame #
a = a %>% relocate(c("xcoord", "ycoord", "col", "row", "geometry"), .after = last_col())

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

#### Negative binomial regression with matched sample and clustered standard errors ####

# Gov OSV #
logit17 = glm.nb(gov_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
summary(logit17)
se_reg_c1 <- round(coeftest(logit17, vcov = vcovPL(logit17, cluster = b$prio.grid)),4)
se_reg_c1

logit18 = glm.nb(gov_death.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = b)
summary(logit18)
se_reg_c2 <- round(coeftest(logit18, vcov = vcovPL(logit18, cluster = b$prio.grid)),4)
se_reg_c2

# Rebel OSV #
logit19 = glm.nb(reb_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
summary(logit19)
se_reg_c3 <- round(coeftest(logit19, vcov = vcovPL(logit19, cluster = b$prio.grid)),4)
se_reg_c3

logit20 = glm.nb(reb_death.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = b)
summary(logit20)
se_reg_c4 <- round(coeftest(logit20, vcov = vcovPL(logit20, cluster = b$prio.grid)),4)

# regression table #
stargazer(logit17, logit18, logit19, logit20, title = "Matched Results Pr(Violence)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/matched_logit.txt")



# same regressions but using 5 death threshold after Fjelde et al. (2019) #

# Gov OSV #
logit21 = glm.nb(gov_event.5 ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
summary(logit21) 
se_reg_c5 <- round(coeftest(logit21, vcov = vcovPL(logit21, cluster = b$prio.grid)),4)
se_reg_c5

logit22 = glm.nb(gov_death.5 ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = b)
summary(logit22)
se_reg_c6 <- round(coeftest(logit22, vcov = vcovPL(logit22, cluster = b$prio.grid)),4)
se_reg_c6


# Rebel OSV #
logit23 = glm.nb(reb_event.5 ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data = b)
summary(logit23) # same as pre-matched, still won't run
se_reg_c7 <- round(coeftest(logit23, vcov = vcovPL(logit23, cluster = b$prio.grid)),4)
se_reg_c7

logit24 = glm.nb(reb_death.5 ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag,
                 data = b)
summary(logit24)
se_reg_c8 <- round(coeftest(logit24, vcov = vcovPL(logit24, cluster = b$prio.grid)),4)
se_reg_c8


stargazer(logit21, logit22, logit23, logit24, title = "Matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_logit_5.txt")



##############################################
# 3-level Multilevel Model w/ matched sample#
##############################################

# is treatment binary? if so, use the following.

a = sample_n(b, 1000)
# GOV OSV #
mlm_3_reg1 = glmer.nb(gov_event.b ~ time * t_ind + (time | ccode:prio.grid) +
       (time | country) + (0 + t_ind + time:t_ind | country) +
         untrp + unpol + unmob + f_untrp +
         f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
         urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data=b)
summary(mlm_3_reg1)

mlm_3_reg2 = glmer.nb(gov_death.b ~ time * t_ind + (time | ccode:prio.grid) +
                        (time | country) + (0 + t_ind + time:t_ind | country) +
                        untrp + unpol + unmob + f_untrp +
                        f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                        urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data=b)
summary(mlm_3_reg2)

# Rebel OSV #
mlm_3_reg3 = glmer.nb(reb_event.b ~ time * t_ind + (time | ccode:prio.grid) +
                        (time | country) + (0 + t_ind + time:t_ind | country) +
                        untrp + unpol + unmob + f_untrp +
                        f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                        urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data=b)
summary(mlm_3_reg3)

mlm_3_reg4 = glmer.nb(reb_death.b ~ time * t_ind + (time | ccode:prio.grid) +
                        (time | country) + (0 + t_ind + time:t_ind | country) +
                        untrp + unpol + unmob + f_untrp +
                        f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                        urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data=b)
summary(mlm_3_reg4)

stargazer(mlm_3_reg1, mlm_3_reg2, mlm_3_reg3, mlm_3_reg4, title = "Matched MLM 3-Level Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_3_b.txt")

# 2-Level Multilevel model #

###########################
# random intercept binary #
###########################

# Gov OSV #
ran.int1 = glmer.nb(gov_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                  f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                  urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag +
                  (1 | ccode), data = b)
summary(ran.int1)

ran.int2 = glmer.nb(gov_death.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag +
                   (1 | ccode), data = b)
summary(ran.int2)


# Rebel OSV #
ran.int3 = glmer.nb(reb_event.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                   urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag +
                   (1 | ccode), data = b)
summary(ran.int3)

ran.int4 = glmer.nb(reb_death.b ~ t_ind + untrp + unpol + unmob + f_untrp +
                   f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + pop.dens + pko_lag +
                   (1 | ccode), data = b)
summary(ran.int4)


stargazer(ran.int1, ran.int2, ran.int3, ran.int4, title = "Matched MLM 2-Level Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/Matched_mlm_b.txt")





#########################
# Robustness Checks #
#########################

#### Re-testing Fjelde et al. (2019) models w/ my data ####

# Gov OSV #
reg1 = glm.nb(a$gov_event.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                    a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                    a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(reg1)

reg2 = glm.nb(a$gov_death.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                     a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                     a$pop.dens + a$pko_lag)
summary(reg2)

# Rebel OSV #
reg3 = glm.nb(a$reb_event.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                     a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + 
                     a$urban_gc + a$nlights_calib_mean + a$pop_gpw_sum + a$pop.dens + a$pko_lag)
summary(reg3) # logit13.1 doesn't run, likely not enough data

reg4 = glm.nb(a$reb_death.5 ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                     a$f_unpol + a$f_unmob + a$mountains_mean + a$ttime_mean + a$pop_gpw_sum + 
                     a$pop.dens + a$pko_lag)
summary(reg4)

stargazer(reg1, reg2, reg3, reg4, title = "Pre-matched Results (>4)", align = TRUE, digits=3, font.size = "scriptsize",
          out = "./results/pre_matched_logit_5.txt")

#### Clustering Standard Errors at the grid level ####
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
se_reg1 <- glm.nb(gov_event ~ t_ind + untrp + unpol + unmob + f_untrp +
               f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + urban_gc + 
               nlights_calib_mean + pop_gpw_sum + pop.dens + pko_lag, data=b)
se_reg_c1 <- round(coeftest(se_reg1, vcov = vcovCluster(se_reg1, cluster = b$prio.grid)),4)

se_reg2 <- lm(gov_death ~ t_ind + untrp + unpol + unmob + f_untrp +
                f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + pop_gpw_sum + 
                pop.dens, data=b)
se_reg_c2 <- round(coeftest(se_reg2, vcov = vcovCluster(se_reg2, cluster = b$prio.grid)),4)

# Rebel OSV #
se_reg3 <- lm(greb_event ~ t_ind + untrp + unpol + unmob + f_untrp +
                f_unpol + f_unmob + pko_lag + mountains_mean + ttime_mean + 
                urban_gc + nlights_calib_mean + pop_gpw_sum + pop.dens, data=b)
se_reg_c3 <- round(coeftest(se_reg3, vcov = vcovCluster(se_reg3, cluster = b$prio.grid)),4)

se_reg4 <- lm(reb_death ~ t_ind + untrp + unpol + unmob + f_untrp +
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
 

 
 

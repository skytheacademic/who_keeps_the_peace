# Which Peacekeepers Keep the Peace? # 
# Sky Kunkel #
# 1/16/2023 #

#install.packages('C:/gurobi950/win64/R/gurobi_9.5-0.zip', repos=NULL)
library(gurobi); library(designmatch); library(gdata); library(tidyverse)

# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd("../")
#### Matching by where units with more and less women deployed ####
a = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(t_ind == 1) %>% drop_na(any_of(c("prio_nlights_calib_mean", "prio_pop_gpw_sum", "prio_pop.dens"))) # drop NAs for matching

# match by where women and where only men deployed
a = a[order(a$t_bal, decreasing=TRUE), ]

control.variables = cbind(a$prio_mountains_mean, a$prio_ttime_mean, a$prio_urban_gc, a$prio_nlights_calib_mean, 
                          a$prio_pop_gpw_sum, a$prio_pop.dens, a$viol_6)
t_ind = a$t_bal #treatment
t_id = which(t_ind==1) #treated
c_id = which(t_ind==0) #control
tab1 = meantab(control.variables, t_ind, t_id, c_id)
tab1

mom_covs = cbind(a$prio_mountains_mean, a$prio_ttime_mean, a$prio_urban_gc, a$prio_nlights_calib_mean, 
                 a$prio_pop_gpw_sum, a$prio_pop.dens, a$viol_6)

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
             a$prio_pop_gpw_sum, a$prio_pop.dens, a$viol_6)
tab2 = meantab(covs, t_ind, t_id_1, c_id_1)
tab2
# Save matched sample 
b = a[c(t_id_1, c_id_1), ]
b$radpko_pko_lag_any = 0
b$radpko_pko_lag_any[b$radpko_pko_lag > 0] = 1

#### Make Loveplot ####
num0_lpn05 = abs(mean(a$prio_mountains_mean[a$t_bal==1])- mean(a$prio_mountains_mean[a$t_bal==0]))
den0_lpn05 = sqrt(  ( (sd(a$prio_mountains_mean[a$t_bal==1]))^2 + (sd(a$prio_mountains_mean[a$t_bal==0]))^2 )/2  )
sd0_lpn05 = num0_lpn05/den0_lpn05
sd0_lpn05_r = round(sd0_lpn05,2)
sd0_lpn05_r

num0_pin09 = abs(mean(a$prio_ttime_mean[a$t_bal==1])- mean(a$prio_ttime_mean[a$t_bal==0]))
den0_pin09 = sqrt(  ( (sd(a$prio_ttime_mean[a$t_bal==1]))^2 + (sd(a$prio_ttime_mean[a$t_bal==0]))^2 )/2  )
sd0_pin09 = num0_pin09/den0_pin09
sd0_pin09_r = round(sd0_pin09,2)
sd0_pin09_r

num0_bac05 = abs(mean(a$prio_urban_gc[a$t_bal==1])- mean(a$prio_urban_gc[a$t_bal==0]))
den0_bac05 = sqrt(  ( (sd(a$prio_urban_gc[a$t_bal==1]))^2 + (sd(a$prio_urban_gc[a$t_bal==0]))^2 )/2  )
sd0_bac05 = num0_bac05/den0_bac05
sd0_bac05_r = round(sd0_bac05,2)
sd0_bac05_r

num0_fre09 = abs(mean(a$prio_nlights_calib_mean[a$t_bal==1])- mean(a$prio_nlights_calib_mean[a$t_bal==0]))
den0_fre09 = sqrt(  ( (sd(a$prio_nlights_calib_mean[a$t_bal==1]))^2 + (sd(a$prio_nlights_calib_mean[a$t_bal==0]))^2 )/2  )
sd0_fre09 = num0_fre09/den0_fre09
sd0_fre09_r = round(sd0_fre09,2)
sd0_fre09_r 

num0_non05 = abs(mean(a$prio_pop_gpw_sum[a$t_bal==1])- mean(a$prio_pop_gpw_sum[a$t_bal==0]))
den0_non05 = sqrt(  ( (sd(a$prio_pop_gpw_sum[a$t_bal==1]))^2 + (sd(a$prio_pop_gpw_sum[a$t_bal==0]))^2 )/2  )
sd0_non05 = num0_non05/den0_non05
sd0_non05_r = round(sd0_non05,2)
sd0_non05_r 

num0_non09 = abs(mean(a$prio_pop.dens[a$t_bal==1])- mean(a$prio_pop.dens[a$t_bal==0]))
den0_non09 = sqrt(  ( (sd(a$prio_pop.dens[a$t_bal==1]))^2 + (sd(a$prio_pop.dens[a$t_bal==0]))^2 )/2  )
sd0_non09 = num0_non09/den0_non09
sd0_non09_r = round(sd0_non09,2)
sd0_non09_r

num0_idh5 = abs(mean(a$viol_6[a$t_bal==1])- mean(a$viol_6[a$t_bal==0]))
den0_idh5 = sqrt(  ( (sd(a$viol_6[a$t_bal==1]))^2 + (sd(a$viol_6[a$t_bal==0]))^2 )/2  )
sd0_idh5 = num0_idh5/den0_idh5
sd0_idh5_r = round(sd0_idh5,2)
sd0_idh5_r

##################################################
# Standardized differences after matching
##################################################

num1_lpn05 = abs(mean(b$prio_mountains_mean[b$t_bal==1])- mean(b$prio_mountains_mean[b$t_bal==0]))
sd1_lpn05 = num1_lpn05/den0_lpn05
sd1_lpn05_r = round(sd1_lpn05,2)
sd1_lpn05_r

num1_pin09 = abs(mean(b$prio_ttime_mean[b$t_bal==1])- mean(b$prio_ttime_mean[b$t_bal==0]))
sd1_pin09 = num1_pin09/den0_pin09
sd1_pin09_r = round(sd1_pin09,2)
sd1_pin09_r

num1_bac05 = abs(mean(b$prio_urban_gc[b$t_bal==1])- mean(b$prio_urban_gc[b$t_bal==0]))
sd1_bac05 = num1_bac05/den0_bac05
sd1_bac05_r = round(sd1_bac05,2)
sd1_bac05_r

num1_fre09 = abs(mean(b$prio_nlights_calib_mean[b$t_bal==1])- mean(b$prio_nlights_calib_mean[b$t_bal==0]))
sd1_fre09 = num1_fre09/den0_fre09
sd1_fre09_r = round(sd1_fre09,2)
sd1_fre09_r

num1_non05 = abs(mean(b$prio_pop_gpw_sum[b$t_bal==1])- mean(b$prio_pop_gpw_sum[b$t_bal==0]))
sd1_non05 = num1_non05/den0_non05
sd1_non05_r = round(sd1_non05,2)
sd1_non05_r

num1_non09 = abs(mean(b$prio_pop.dens[b$t_bal==1])- mean(b$prio_pop.dens[b$t_bal==0]))
sd1_non09 = num1_non09/den0_non09
sd1_non09_r = round(sd1_non09,2)
sd1_non09_r 

num1_idh5 = abs(mean(b$viol_6[b$t_bal==1])- mean(b$viol_6[b$t_bal==0]))
sd1_idh5 = num1_idh5/den0_idh5
sd1_idh5_r = round(sd1_idh5,2)
sd1_idh5_r

# Data set with stand. diff. before matchig
colnames = c("Avg. Mountain",
             "Travel Time",
             "Urban Perc.",
             "Night Lights",
             "Population Sum",
             "Population Density",
             "Violence 6 Months Before")
colnames

sd0 = c(sd0_lpn05_r,sd0_pin09_r,sd0_bac05_r,sd0_fre09_r, sd0_non05_r,sd0_non09_r,sd0_idh5_r)

sd1 = c(sd1_lpn05_r,sd1_pin09_r,sd1_bac05_r,sd1_fre09_r, sd1_non05_r,sd1_non09_r,sd1_idh5_r)

before = rep("Before",7)
after = rep("After",7)    

meanbalance0 = data.frame(colnames,sd0,before)
meanbalance0$order <- nrow(meanbalance0):1
meanbalance0 <-meanbalance0[order(meanbalance0$order, decreasing=FALSE),]
meanbalance0$colnames <- factor(meanbalance0$colnames, levels = meanbalance0$colnames[order(meanbalance0$order)])
meanbalance0
names(meanbalance0)

meanbalance1 = data.frame(colnames,sd1,after)
meanbalance1$order <- nrow(meanbalance1):1
meanbalance1 <-meanbalance1[order(meanbalance1$order),]
meanbalance1$colnames <- factor(meanbalance1$colnames, levels = meanbalance1$colnames[order(meanbalance1$order)])
meanbalance1
names(meanbalance1)

print(levels(meanbalance0$colnames))  
print(levels(meanbalance1$colnames))  

colnames(meanbalance0) = c("covariate","sd","Matching","order")
colnames(meanbalance1) = c("covariate","sd","Matching","order")

meanbalance = rbind(meanbalance0,meanbalance1)
meanbalance = data.frame(meanbalance)
meanbalance

# Generate plot
loveplot = ggplot(
  data = meanbalance, mapping = 
                    aes(x = covariate, y = sd, group = Matching, color = Matching)) +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1, linetype="dashed") +
  coord_flip() + theme_pubclean() + theme(legend.key = element_blank()) +
  labs(y = "Average absolute standardized differences in means") + labs(x = "") +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) ) +
  scale_color_manual(values=c("gray20","black")) + 
  geom_point(aes(shape=Matching),size=4) +
  scale_shape_manual(values=c(8,16)) + 
  theme(text = element_text(size=23))

pdf("./results/loveplot.pdf", width = 12, height = 7)
loveplot
dev.off()
#### save data ####
saveRDS(b, "./data/kunkel_wpks_matched_gender.rds")
rm(list = ls())
gc()



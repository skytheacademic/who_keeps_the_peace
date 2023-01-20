# Which Peacekeepers Keep the Peace? #
# Data Cleaning & Merging #
# By: Sky Kunkel

### load libraries ###
library(doSNOW); library(foreach); library(janitor); library(lubridate)
library(sf); library(tidyverse); library(sp); library(CoordinateCleaner)
library(countrycode); library(geosphere)


### Reading in and briefly cleaning the data ###

# set working directory #
setwd("../")

#### read in RADPKO data ####
radpko = read.csv("./data/radpko/radpko_grid.csv")  %>%
  select(-c(west_pko,west_untrp,west_unpol,west_unmob,asian_pko,asian_untrp,asian_unpol,
            asian_unmob,afr_pko,afr_untrp,afr_unpol,afr_unmob)) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date))

# create variables of male troops
radpko$m_untrp = radpko$untrp - radpko$f_untrp 
radpko$m_unpol = radpko$unpol - radpko$f_unpol
radpko$m_unmob = radpko$unmob - radpko$f_unmob
radpko = radpko %>% 
  relocate(m_untrp, .after = f_untrp) %>%
  relocate(m_unpol, .after = f_unpol) %>%
  relocate(m_unmob, .after = f_unmob)

# create variable of male and female PKs total
radpko$m_pko_deployed = radpko$m_untrp + radpko$m_unpol + radpko$m_unmob
radpko$f_pko_deployed = radpko$f_untrp + radpko$f_unpol + radpko$f_unmob
radpko = radpko %>%
  relocate(c(m_pko_deployed, f_pko_deployed), .after = pko_deployed) %>%
  # first female PKs arrive on Sept. 2005, which means no treatment could occur before then
  filter(date >= "2005-09-01")

### Make instrument based off of Ruggeri et al. (2017) and Fjelde et al. (2019)
# pull up data of country capital long and lat
data(countryref)
countryref = countryref %>%
  filter(type == "country", !is.na(capital)) %>%
  group_by(iso2) %>% # check nrow(table(countryref$capital.lon)) vs nrow(table(countryref$iso2)). same number of obs
  summarise(across(capital.lon:capital.lat, mean)) %>% # which means we can just summarize since it's all identical
  ungroup() # that way we can get rid of duplicate observations that would add extra observations upon merging

table(radpko$mission)
# MINURCAT  MINUSCA  MINUSMA    MONUC  MONUSCO     ONUB   UNAMID  UNAMSIL   UNISFA    UNMIL    UNMIS   UNMISS    UNOCI 

# add iso2 to RADPKO (easier for visual confirmation than iso3)
radpko$iso2 = countrycode(radpko$country, origin = "country.name", destination = "iso2c")

# visual confirmation conversion worked
table(radpko$iso2, radpko$country) # in each column, there should only be one row > 0
# also note the 0 observations for Abyei (not an independent country), meaning we'll have to hand code it

before = table(radpko$country)
radpko = left_join(radpko, countryref, by = "iso2")
after = table(radpko$country)
all.equal(before, after) # check to make sure no duplicates were made
radpko = radpko %>%
  select(-c(iso2)) # drop iso2 since it's no longer relevant
# Abyei will be unique since it's not in countryref data, so we'll calculate distance to Abyei Town (capital of Abyei)
sum(is.na(radpko$capital.lon))
table(radpko$country[radpko$country == "Abyei"]) #all missing values are Abyei

# coordinates of Abyei town obtained via Google Maps: 9.59067093295664 (lat), 28.436891724606674 (lon)
radpko$capital.lon[radpko$country== "Abyei"] = 28.436891724606674
radpko$capital.lat[radpko$country== "Abyei"] = 9.59067093295664
sum(is.na(radpko$capital.lon))

# for each mission, use the [mission == "mission_name"] 

# how to calculate grid cells that cover multiple countries?
# calculate each separately, then average

# read in PRIO files for grid coordinates
prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>%
  rename(prio.grid = gid, gid_center_lon = xcoord, gid_center_lat = ycoord) %>% 
  select(-c(col, row))

radpko = left_join(radpko, prio, by = "prio.grid") %>%
  select(-c(geometry))

# calculate distance_to_capital
radpko = 
  radpko %>% 
  rowwise() %>% 
  mutate(distance_to_capital = distHaversine(cbind(capital.lon, capital.lat), 
                                             cbind(gid_center_lon, gid_center_lat))) %>%
  mutate(distance_to_capital = distance_to_capital/1000) %>% # distHaversine gives units in meters, so convert to KMs
  ungroup()

# create pko_supply calculating peacekeepers deployed to Africa by month and year
pko_supply = radpko %>%
  mutate(across(pko_deployed:f_pko_deployed, ~replace_na(.x, 0))) %>%
  select(month, year, pko_deployed, m_pko_deployed, f_pko_deployed) %>%
  group_by(month,year) %>%
  summarise(pko_africa = sum(pko_deployed), m_pko_africa = sum(m_pko_deployed), f_pko_africa = sum(f_pko_deployed))
radpko = left_join(radpko, pko_supply, by = c("month", "year"))
# Fjelde et al. log distance to capital, whereas Ruggeri et al. measure it in kilometers
# also measure PKO UN Africa in ten thousands, hard to tell how Ruggeri et al. measure
rm(prio) # we'll load prio back in later

# Create a "treatment" indicator telling us if PKs existed in a certain grid at a certain time 
radpko$t_ind = 0
radpko$t_ind[radpko$units_deployed >= 1] = 1

# make total proportion indicator
radpko$f_prop = 0
radpko$f_prop = (radpko$f_untrp + radpko$f_unpol + radpko$f_unmob) /
                (radpko$untrp + radpko$unpol + radpko$unmob)
radpko = radpko %>%
  relocate(f_prop, .after = pko_deployed)

# make female PKs proportion variable
radpko = radpko %>%
  mutate(f_untrp.p = f_untrp/untrp, f_unpol.p = f_unpol/unpol, f_unmob.p = f_unmob/unmob) %>%
  relocate(f_untrp.p, .after = untrp) %>%
  relocate(f_unpol.p, .after = unpol) %>%
  relocate(f_unmob.p, .after = unmob)

# Replace NAs w/ 0s
radpko <- radpko %>% 
  mutate(across(units_deployed:m_unmob, 
                ~replace_na(.x, 0)))

# add proportions of each type to get total gender balance and then split treatment by balance
radpko$t_bal = 0 # make balanced treatment indicator
radpko$t_bal[radpko$f_prop > quantile(radpko$f_prop[radpko$t_ind == 1], prob=.75, type=1)] = 1
radpko$t_unbal = 0 # make un_balanced treatment indicator
radpko$t_unbal[radpko$f_prop <= quantile(radpko$f_prop[radpko$t_ind == 1], prob=.75, type=1) & radpko$t_ind ==1] = 1

##### Merge UCDP data #####
# read in data
dd = read.csv("./data/ucdp_ged/ucdp-actor-221.csv") %>%
  rename(a_id = ActorId) %>%
  select(c(a_id, Org)) # grab data so we can classify actors during OSV
df = read.csv("./data/ucdp_ged/GEDEvent_v22_1.csv") %>%
  # make the date variable a date type
  mutate(date = ymd_hms(date_end)) %>%
  mutate(month = month(date)) %>%
  filter(type_of_violence == 3) %>%
  select(c(date, month, year, side_a_new_id, priogrid_gid, deaths_civilians)) %>%
  # rename variable for ease of merging
  rename(prio.grid = priogrid_gid, ucdp_deaths = deaths_civilians) %>%
  select(-c("date"))

df = left_join(df, dd, by = c("side_a_new_id" = "a_id"))

df = df %>%
  group_by(prio.grid, year, month, Org) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths)) %>%
  drop_na(Org) %>% 
  ungroup()

df$ucdp_gov_vac_5 = 0
df$ucdp_gov_vac_5[df$Org == 4 & df$ucdp_deaths >= 5] = 1
df$ucdp_gov_vac_all = 0
df$ucdp_gov_vac_all[df$Org == 4] = df$ucdp_deaths[df$Org == 4]
df$ucdp_reb_vac_5 = 0
df$ucdp_reb_vac_5[df$Org == 1 & df$ucdp_deaths >= 5] = 1
df$ucdp_reb_vac_all = 0 
df$ucdp_reb_vac_all[df$Org==1] = df$ucdp_deaths[df$Org==1]

df = df %>%
  group_by(prio.grid, year, month) %>%
  summarize(across(ucdp_gov_vac_5:ucdp_reb_vac_all, sum))

a = left_join(radpko, df, by = c("prio.grid", "year", "month"))

a <- a %>% 
  mutate(across(ucdp_gov_vac_5:ucdp_reb_vac_all, 
                ~replace_na(.x, 0)))

rm(dd, df, countryref, pko_supply, before, after, radpko)

#### read in PRIO data ####
prio.static = read_csv("./data/prio/PRIO-GRID Static Variables - 2022-06-03.csv")
prio.yearly = read_csv("./data/prio/PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv")
names(prio.static)[1] = "prio.grid" # rename for merging
names(prio.yearly)[1] = "prio.grid" # rename for merging
prio.static$prio.grid = as.character(prio.static$prio.grid)
prio.yearly$prio.grid = as.character(prio.yearly$prio.grid)

# merge prio variables
prio.var = left_join(prio.yearly, prio.static, by = c("prio.grid"))
prio.var$prio.grid = as.numeric(prio.var$prio.grid)
rm(prio.static, prio.yearly)

## merge ##
merged.data = left_join(a, prio.var, by = c("prio.grid", "year"))

# rearrange the columns to put the geo-locational data last
merged.data = merged.data %>% 
  relocate(c("gid_center_lon", "gid_center_lat"), .after = last_col())

# clear everything except the merged data
rm(list = setdiff(ls(), "merged.data")) 
a = merged.data
rm(merged.data)
gc()

##### Impute Covariates #####
# data imputation of control variables
a$mountains_mean<-ave(a$mountains_mean,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$ttime_mean<-ave(a$ttime_mean,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$urban_gc<-ave(a$urban_gc,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$nlights_calib_mean<-ave(a$nlights_calib_mean,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$pop_gpw_sum<-ave(a$pop_gpw_sum,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$prec_gpcp<-ave(a$prec_gpcp,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$landarea<-ave(a$landarea,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

# make control variable population density
a$pop.dens = a$pop_gpw_sum / a$landarea 

# can't impute pop density until after it's created
a$pop.dens<-ave(a$pop.dens,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

# mountains are static, so any grids with missing values are 0
a$mountains_mean[is.na(a$mountains_mean)] <- 0

# add summary violence 6 months prior 
a = a %>%
  mutate(ucdp_vac_all = ucdp_reb_vac_all + ucdp_gov_vac_all) %>%
  group_by(prio.grid) %>% 
  mutate(viol_6 = 
           lag(ucdp_vac_all, 6) + lag(ucdp_vac_all, 5) + lag(ucdp_vac_all, 4) + 
           lag(ucdp_vac_all, 3) + lag(ucdp_vac_all, 2) + lag(ucdp_vac_all, 1)) %>%
  relocate(viol_6, .after = ucdp_reb_vac_all) %>%
  ungroup()

##### add a lagged variable of PKs deployed #####

# sort again to make sure it works
a = a[order(a$year, decreasing=FALSE), ] 
a = a[order(a$month, decreasing=FALSE), ] 
a = a[order(a$prio.grid, decreasing=FALSE), ]

a <- a %>%                            # Add lagged column
  group_by(prio.grid) %>%
  dplyr::mutate(pko_lag = dplyr::lag(pko_deployed, n = 1, default = NA)) %>% 
  as.data.frame() %>%
  relocate(pko_lag, .after = pko_deployed)

a$pko_lag[is.na(a$pko_lag)] <- 0 #we have all missions from their start, so any NAs are actually 0s



### reorganize and rename
a <- a %>% 
  select(-c(xcoord, ycoord)) %>% 
  rename_at(vars(units_deployed:m_unmob), function(x) paste0("radpko_", x)) %>% 
  rename_at(vars(agri_ih:pop.dens), function(x) paste0("prio_", x))

# remove useless columns
a$acled_fatalities = NULL
a$acled_event = NULL

# save data #
saveRDS(a, file = "./data/kunkel_which_pks.rds")

# data fully merged, cleaned, and exported #

rm(list = ls())
gc()

# Data Cleaning & Merging #
# By: Sky Kunkel

### load libraries ###
library(doSNOW); library(foreach); library(janitor); library(lubridate)
library(sf); library(tidyverse); library(sp); library(CoordinateCleaner)
library(countrycode); library(geosphere)


### Reading in and briefly cleaning the data ###

# set working directory #
setwd("../")

#### Read in ACLED data ####
acled  = read.csv("./data/acled/1999-01-01-2021-12-31.csv") %>%
  select(-c(data_id, event_id_cnty, event_id_no_cnty, actor1, assoc_actor_1,actor2,assoc_actor_2,region,
            admin3,location,source,source_scale,notes,timestamp, iso, iso3, time_precision, admin1,
            admin2)) %>%  
  mutate(event_date = dmy(event_date), month = month(event_date), year = year(event_date))

# subset ACLED data to violence against civilians and dates from before 2019 and after 
# 1999 to match RADPKO data (and to make analysis faster)
acled = subset(acled, event_date < "2019-01-01" & event_date > "1999-01-01" & 
                 event_type == "Violence against civilians" | event_type == "Explosions/Remote violence")
acled = subset(acled, inter2 == 7 & inter1 == 1 | inter1 == 2 | inter1 == 3)

### add VAC events and deaths by actor
acled$event = 1
acled$vac_gov_death_all = 0
acled$vac_gov_death_all[acled$interaction == 17 | acled$interaction == 37] = 
  acled$fatalities[acled$interaction == 17 | acled$interaction == 37]
acled$vac_gov_death_any = 0
acled$vac_gov_death_any[acled$interaction == 17 | acled$interaction == 37] = 1
acled$vac_reb_death_all = 0
acled$vac_reb_death_all[acled$interaction == 27] = acled$fatalities[acled$interaction == 27]
acled$vac_reb_death_any = 0
acled$vac_reb_death_any[acled$interaction == 27] = 1
acled$vac_gov_event_all = 0
acled$vac_gov_event_all[acled$interaction == 17 | acled$interaction == 37] = 
  acled$event[acled$interaction == 17 | acled$interaction == 37]
acled$vac_gov_event_any = 0
acled$vac_gov_event_any[acled$interaction == 17 | acled$interaction == 37] = 1
acled$vac_reb_event_all = 0
acled$vac_reb_event_all[acled$interaction == 27] = acled$event[acled$interaction == 27]
acled$vac_reb_event_any = 0
acled$vac_reb_event_any[acled$interaction == 27] = 1
acled$vac_gov_death_5 = 0
acled$vac_gov_death_5[acled$interaction == 17 | acled$interaction == 37 & acled$fatalities >= 5] = 1
acled$vac_reb_death_5 = 0
acled$vac_reb_death_5[acled$interaction == 27 & acled$fatalities >= 5] = 1

### add VAC aggregated by actor
acled$vac_death_all = 0
acled$vac_death_all = acled$vac_gov_death_all + acled$vac_reb_death_all
acled$vac_death_any = 0
acled$vac_death_any[acled$vac_gov_death_any == 1 | acled$vac_reb_death_any == 1] = 1
acled$vac_event_all = 0
acled$vac_event_all = acled$vac_gov_event_all + acled$vac_reb_event_all
acled$vac_event_any = 0
acled$vac_event_any[acled$vac_gov_event_any == 1 | acled$vac_reb_event_any == 1] = 1


# change ACLED's classification of Abyei from admin1 unit to country unit to match RADPKO
acled$country[acled$admin1=="Abyei"] = "Abyei"

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
  relocate(c(m_pko_deployed, f_pko_deployed), .after = pko_deployed)

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
  mutate(across(pko_deployed, 
                ~replace_na(.x, 0))) %>%
  select(month, year, pko_deployed) %>%
  group_by(month,year) %>%
  summarise(pko_africa = sum(pko_deployed))
radpko = left_join(radpko, pko_supply, by = c("month", "year"))
# Fjelde et al. log distance to capital, whereas Ruggeri et al. measure it in kilometers
# also measure PKO UN Africa in ten thousands, hard to tell how Ruggeri et al. measure
rm(prio) # we'll load prio back in later

# radpko has duplicate grid-months (different missions), so aggregate by grid
# radpko <- radpko %>% 
#   select(-c(mission, date)) %>% 
#   relocate(c(year, month), .after = prio.grid) %>% 
#   group_by(prio.grid, year, month) %>% 
#   summarise(across(units_deployed:m_unmob, sum)) %>% 
#   ungroup()

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
radpko$t_bal[radpko$f_prop > median(radpko$f_prop[radpko$t_ind==1])] = 1
radpko$t_unbal = 0 # make un_balanced treatment indicator
radpko$t_unbal[radpko$f_prop <= median(radpko$f_prop[radpko$t_ind==1]) & radpko$t_ind == 1] = 1
# add variable denoting pk type by binary measure of treatment
radpko$untrp_maj = 0
radpko$untrp_maj[radpko$untrp > radpko$unpol & radpko$untrp > radpko$unmob] = 1
radpko$unpol_maj = 0
radpko$unpol_maj[radpko$unpol > radpko$untrp & radpko$unpol > radpko$unmob] = 1
radpko$unmob_maj = 0
radpko$unmob_maj[radpko$unmob > radpko$unpol & radpko$unmob > radpko$untrp] = 1

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


#### adding PRIO-grid numbers to ACLED data ####
# let's start by aggregating into month-level units # 
# first we need to aggregate data into PRIO-Grid's

# read in PRIO shape files from their website
# prio uses WGS84 CRS
prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F)
names(prio)[1] = "prio.grid" # rename for merging

# save the CRS
proj_crs <- st_crs(prio)

# convert to sf
acled <- st_as_sf(acled, coords = c("longitude", "latitude"), crs = proj_crs)
acled <- st_join(acled, prio)
a = acled %>%
  as.data.frame() %>% # convert to dataframe
  select(-c(xcoord, ycoord, col, row, geometry, event_date)) %>% 
  relocate(c("year", "month"), .after = last_col()) %>% 
  group_by(prio.grid, year, month) %>% 
  summarise(across(fatalities:vac_event_any, sum)) %>% # aggregate violence by prio grid, month, and year
  ungroup()

# make binaries with counts > 0 as 1
a$vac_gov_event_any[a$vac_gov_event_any>=1] = 1
a$vac_reb_event_any[a$vac_reb_event_any>=1] = 1
a$vac_gov_death_any[a$vac_gov_death_any>=1] = 1
a$vac_reb_death_any[a$vac_reb_death_any>=1] = 1
a$vac_death_any[a$vac_death_any >= 1] = 1
a$vac_event_any[a$vac_event_any >= 1] = 1

## merge ##
merged.data = left_join(radpko, a, by = c("prio.grid", "year", "month"))

# add various data to merged dataset
merged.data = left_join(merged.data, prio.var, by = c("prio.grid", "year"))

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

# replace NAs w/ 0
a$event[is.na(a$event)] <- 0
a$mountains_mean[is.na(a$mountains_mean)] <- 0
a <- a %>% 
  mutate(across(fatalities:vac_event_any, 
                ~replace_na(.x, 0)))

# add summary violence 6 months prior 
a = a %>% group_by(prio.grid) %>% 
  mutate(viol_6 = 
           lag(event, 6) + lag(event, 5) + lag(event, 4) + 
           lag(event, 3) + lag(event, 2) + lag(event, 1)) %>%
  relocate(viol_6, .after = event)

##### add a post treated variable #####
a <- a %>% 
  mutate(time = (year-1999)*(12) + month)
dd <- a %>% as.data.frame() %>% select(prio.grid, time, t_ind)
dd <- split(dd, f = dd$prio.grid)
dd <- lapply(dd, FUN = function(x){
  y <- x[which(x$t_ind == 1),]
  # create a "first treated" variable. needs to be 0 for untreated
  x$first_treated <- ifelse(nrow(y) == 0, 0, min(y$time))
  # create a "post treated" variable. needs to be 0 until treatment then 1
  x$post_treatment <- ifelse(x$first_treated != 0 & x$time >= x$first_treated, 
                             1, 0)
  # create a "treated" variable. needs to be 0 if control and 1 if treated
  x$treated <- ifelse(sum(x$t_ind, na.rm = T) > 0, 1, 0)
  x
})
dd <- do.call(rbind, dd)
dd <- dd[,c("prio.grid", "time", "first_treated", "post_treatment")]
# merge back to main df
a <- left_join(a, dd, by = c("prio.grid", "time"))

##### add a lagged variable #####

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

##### Merge UCDP data #####
# read in data
# df = read.csv("./data/ucdp_ged/ged211.csv") %>%
#   # make the date variable a date type
#   mutate(date = ymd_hms(date_end)) %>% 
#   mutate(month = month(date)) %>%
#   filter(type_of_violence == 3) %>%
#   select(-c(1:2, 4:32, 34:40, 41,42,46:50)) %>% 
# 
#   # rename variable for ease of merging
#   rename(prio.grid = priogrid_gid, ucdp_deaths = deaths_civilians) %>%
#   mutate(date = ymd(date), month = month(date), year = year(date)) %>%
#   select(-c("date_end", "date"))
# 
# df$ucdp_event = 1
# df = df %>%
#   group_by(prio.grid, year, month) %>%
#   summarize(ucdp_deaths = sum(ucdp_deaths), ucdp_event = sum(ucdp_event))
# 
# a = left_join(a, df, by = c("prio.grid", "year", "month"))
# a$ucdp_deaths[is.na(a$ucdp_deaths)] <- 0
# a$ucdp_event[is.na(a$ucdp_event)] <- 0
# 
# rm(dd, df)

### reorganize and rename
a <- a %>% 
  select(-c(xcoord, ycoord)) %>% 
  rename_at(vars(fatalities:vac_event_any), 
            function(x) paste0("acled_", x)) %>% 
  rename_at(vars(units_deployed:m_unmob), function(x) paste0("radpko_", x)) %>% 
  rename_at(vars(agri_ih:pop.dens), function(x) paste0("prio_", x)) 

# remove useless columns
a$acled_fatalities = NULL
a$acled_event = NULL

### create an "any fatalities" variable for ACLED
a <- a %>% 
  mutate(acled_fatalities_any = case_when(rowSums(across(
    acled_vac_gov_death_all:acled_vac_reb_death_5)) > 0 ~ 1,
    TRUE ~ 0)) %>%
  relocate(acled_fatalities_any, .after = acled_vac_reb_death_5) %>% 
  mutate(acled_fatalities_all = rowSums(across(
    acled_vac_gov_death_all:acled_vac_reb_death_all))) %>%
  relocate(acled_fatalities_all, .after = acled_fatalities_any)

# save RDS #

saveRDS(a, file = "./data/kunkel_cg.rds")

# data fully merged, cleaned, and exported #

rm(list = ls())
gc()

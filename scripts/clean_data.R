# Data Cleaning & Merging #
# By: Sky Kunkel

### load libraries ###
library(doSNOW); library(foreach); library(janitor); library(lubridate)
library(sf); library(tidyverse); library(sp); library(spatialEco)

### Reading in and briefly cleaning the data ###

# set working directory #
setwd("../")

# load the data #
acled  = read.csv("./data/acled/1999-01-01-2021-12-31.csv") %>%
  select(-c(event_id_cnty, event_id_no_cnty, actor1, assoc_actor_1,actor2,assoc_actor_2,region,
            admin3,location,source,source_scale,notes,timestamp))
radpko = read.csv("./data/radpko/radpko_grid.csv")  %>%
  select(-c(west_pko,west_untrp,west_unpol,west_unmob,asian_pko,asian_untrp,asian_unpol,
            asian_unmob,afr_pko,afr_untrp,afr_unpol,afr_unmob)) %>%
  mutate(date = ymd(date),
         month = month(date),
         year = year(date))
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
# change date-time
acled$event_date = lubridate::dmy(acled$event_date)
radpko$date = lubridate::ymd(radpko$date) 

# subset ACLED data to violence against civilians and dates from before 2019 and after 
# 1999 to match RADPKO data (and to make analysis faster)
acled = subset(acled, acled$event_date < "2019-01-01" & acled$event_date > "1999-01-01" & 
                 acled$event_type == "Violence against civilians")

# change ACLED's classification of Abyei from admin1 unit to country unit to match RADPKO
acled$country[acled$admin1=="Abyei"] = "Abyei"



### adding PRIO-grid numbers to ACLED data ###
# let's start by aggregating into month-level units # 
# first we need to aggregate data into PRIO-Grid's

# read in PRIO shape files from their website
# prio uses WGS84 CRS
prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>% 
  mutate(gid = as.character(gid))

names(prio)[1] = "prio.grid" # rename for merging
prio$prio.grid = as.numeric(prio$prio.grid) # transform the column into numeric so we can join the data

## join data together ##
prio.rad = left_join(radpko, prio, by = "prio.grid") 

# transform both datasets into  spatial objects
prio.sp = as(prio, Class = "Spatial") # 

# assign crs system for ACLED data #
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

acled.sp <- SpatialPointsDataFrame(acled[15:14],         # reading in the dataframe as a spatial object
                                   acled,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 


# run point.in.poly, which determines if a coordinate (ACLED data) exists within a polygon (PRIO)
# if the coordinate exists inside of the grid, it applies the characteristics of the polygon
# to the coordinate; i.e., the PRIO-grid number + coordinates + geometry is added to each 
# event in the ACLED data
b = point.in.poly(acled.sp, prio.sp, sp = TRUE, duplicate = TRUE)
a = as.data.frame(b) # convert to dataframe



### Now that R knows which points of violence occur in which grids, let's aggregate the data ###

# start by adding a dummy column so we know how many violent events occur in a grid per month
a$event = 1

# round daily event data down to the 1st of the month to match ACLED data
a$event_date <- floor_date(a$event_date, "month")

# change variable types to prepare for merging
prio.rad$date = as.Date(prio.rad$date, "%m/%d/%Y") # format dates
a$prio.grid = as.character(a$prio.grid)
prio.rad$prio.grid = as.character(prio.rad$prio.grid)

# group fatalities and events by PRIO-grid & date
a.ag = a %>%
  group_by(prio.grid, event_date, inter1) %>%
  summarize(fatalities = sum(fatalities), event = sum(event))

a.ag$prio.grid = as.character(a.ag$prio.grid)
prio$prio.grid = as.character(prio$prio.grid)
a.ag.prio = left_join(a.ag, prio, by = "prio.grid") 

# add id to ACLED data
a.ag.prio <- tibble::rowid_to_column(a.ag.prio, "id")

# rename event_date to date for merging
names(a.ag.prio)[3] = "date"

## merge ##
merged.data = left_join(prio.rad, a.ag.prio, by = c("prio.grid", "date",
                                                    "geometry", "xcoord",
                                                    "ycoord", "col", "row"))

# extract year to merge prio yearly data
merged.data$year = year(merged.data$date)
merged.data$prio.grid = as.numeric(merged.data$prio.grid)

# add various data to merged dataset
merged.data = left_join(merged.data, prio.var, by = c("prio.grid", "row", "col",
                                                         "xcoord", "ycoord", "year"))

# rearrange the columns to put the geo-locational data last
merged.data = merged.data %>% 
  relocate(c("xcoord", "ycoord", "col", "row", "geometry"), .after = last_col())

# verify there are no missing observations
sum(is.na(merged.data$mission)) 

# clear everything except the merged data
rm(list = setdiff(ls(), "merged.data")) 
a = merged.data
rm(merged.data)
gc()

# Create a "treatment" indicator telling us if PKs existed in a certain grid at a certain time 
a$t_ind = 0
a$t_ind[a$units_deployed >= 1] = 1
a$event.b = 0
a$event.b[a$event>0] = 1
a$death = 0
a$death[a$fatalities>0] = 1
a$pop.dens = a$pop_gpw_sum / a$landarea 
a$fate.5 = 0
a$fate.5[a$fatalities > 4] = 1
a$event.5 = 0
a$event.5[a$event > 4] = 1
# replace NAs w/ 0
a$units_deployed[is.na(a$units_deployed)] <- 0
a$countries_deployed[is.na(a$countries_deployed)] <- 0
a$pko_deployed[is.na(a$pko_deployed)] <- 0
a$untrp[is.na(a$untrp)] <- 0
a$unpol[is.na(a$unpol)] <- 0
a$unmob[is.na(a$unmob)] <- 0
a$f_untrp[is.na(a$f_untrp)] <- 0
a$f_unpol[is.na(a$f_unpol)] <- 0
a$f_unmob[is.na(a$f_unmob)] <- 0
a$fatalities[is.na(a$fatalities)] <- 0
a$event[is.na(a$event)] <- 0
a$mountains_mean[is.na(a$mountains_mean)] <- 0

### add OSV by distinct actors variables ###
# violent events + binaries #
a$inter1[is.na(a$inter1)] <- 0
a$gov_event = 0
a$gov_event[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$event[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$gov_event.b = 0
a$gov_event.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$event.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$reb_event = 0
a$reb_event[a$inter1 == 2] = a$event[a$inter1 == 2]
a$reb_event.b = 0
a$reb_event.b[a$inter1 == 2] = a$event.b[a$inter1 == 2]

a$gov_event.5 = 0
a$gov_event.5[a$gov_event.b == 1 & a$event.5 == 1] = 1
a$reb_event.5 = 0
a$reb_event.5[a$reb_event.b == 1 & a$event.5 == 1] = 1

# fatalities + binaries #
a$gov_death = 0
a$gov_death[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$fatalities[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$gov_death.b = 0
a$gov_death.b[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4] = a$death[a$inter1 == 1 | a$inter1 == 3 | a$inter1 == 4]
a$reb_death = 0
a$reb_death[a$inter1 == 2] = a$fatalities[a$inter1 == 2]
a$reb_death.b = 0
a$reb_death.b[a$inter1 == 2] = a$death[a$inter1 == 2]

a$gov_death.5 = 0
a$gov_death.5[a$gov_death.b == 1 & a$fate.5 == 1] = 1
a$reb_death.5 = 0
a$reb_death.5[a$reb_death.b == 1 & a$fate.5 == 1] = 1

# turn countries into numbers for mlm #
a$ccode = 0
a$ccode[a$country == "Abyei"] = 1
a$ccode[a$country == "Burundi"] = 2
a$ccode[a$country == "Central African Republic"] = 3
a$ccode[a$country == "Chad"] = 4
a$ccode[a$country == "Cote d'Ivoire"] = 5
a$ccode[a$country == "Democratic Republic of Congo"] = 6
a$ccode[a$country == "Liberia"] = 7
a$ccode[a$country == "Mali"] = 8
a$ccode[a$country == "Sierra Leone"] = 9
a$ccode[a$country == "South Sudan"] = 10
a$ccode[a$country == "sudan"] = 11


##### add a post treated variable #####
a <- a %>% 
  mutate(time = (year-1999)*(12) + month)
dd <- a %>% as.data.frame() %>% select(prio.grid, time, t_ind, mission)
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
dd <- dd[,c("prio.grid", "time", "first_treated", "post_treatment", "mission")]
# merge back to main df
a <- left_join(a, dd, by = c("prio.grid", "time", "mission"))

##### add a lagged variable #####

# sort again to make sure it works
a = a[order(a$date, decreasing=FALSE), ] 
a = a[order(a$prio.grid, decreasing=FALSE), ]

a <- a %>%                            # Add lagged column
  group_by(prio.grid) %>%
  dplyr::mutate(pko_lag = dplyr::lag(pko_deployed, n = 1, default = NA)) %>% 
  as.data.frame() %>%
  relocate(pko_lag, .after = pko_deployed)

##### Impute Covariates #####

# data imputation of control variables by Prio Grid
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
a$pop.dens<-ave(a$pop.dens,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))
a$prec_gpcp<-ave(a$prec_gpcp,a$prio.grid,FUN=function(x) 
  ifelse(is.na(x), mean(x,na.rm=TRUE), x))

# check how many variables are still missing
sum(is.na(a$mountains_mean))
sum(is.na(a$ttime_mean))
sum(is.na(a$urban_gc))
sum(is.na(a$nlights_calib_mean))
sum(is.na(a$pop_gpw_sum))
sum(is.na(a$pop.dens))
sum(is.na(a$prec_gpcp))

##### Merge UCDP data #####
# read in data
df = read.csv("./data/ucdp_ged/ged211.csv") %>%
  select(-c(1:32, 34:39, 41,42,44:49)) %>% 
  # make the date variable a date type
  mutate(date = ymd_hms(date_end)) %>% 
  # rename variable for ease of merging
  rename(prio.grid = priogrid_gid, ucdp_deaths = deaths_civilians) %>%
  select(-"date_end")

df$date = floor_date(df$date, "month")
df$ucdp_event = 1
df = df %>%
  group_by(prio.grid, date) %>%
  summarize(ucdp_deaths = sum(ucdp_deaths), ucdp_event = sum(ucdp_event))

a = left_join(a, df, by = c("prio.grid", "date"))
a$ucdp_deaths[is.na(a$ucdp_deaths)] <- 0
a$ucdp_event[is.na(a$ucdp_event)] <- 0

# save RDS #

saveRDS(a, file = "./data/kunkel_cg.rds")

# data fully merged, cleaned, and exported #

rm(list = ls())

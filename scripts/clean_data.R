# Data Cleaning & Merging #
# By: Adam Kunkel

### load libraries ###
library(exactextractr); library(raster); library(rasterVis); library(rgdal)
library(rgeos); library(sf); library(sp); library(tidygeocoder)
library(tidyverse); library(tmap); library(viridis); library(lubridate)

# need to install Terra 1.5 or higher to install spatialeco, need to adjust image
library(spatialEco); library(gdata); library(designmatch) 

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep)

### Reading in and briefly cleaning the data ###

# set working directory #
setwd("../data")

# load the data #
acled  = read.csv("acled_data.csv") # because some RADPKO grids cross over borders, 
# must use violence data from neighboring countries too
radpko = read.csv("radpko_grid.csv") 

afrogrid = read.csv("afro_subset.csv")

#######################
# add in Afro.Grid data
#######################

# first, clean the afro data and get rid of useless variables
afrogrid[6:37] = NULL
afrogrid[10:37] = NULL
afrogrid[23:32] = NULL
afrogrid[11:12] = NULL
afrogrid[13:15] = NULL
names(afrogrid)[5] = "date"
names(afrogrid)[2] = "prio.grid"

# change date-time
acled$event_date = lubridate::dmy(acled$event_date)
radpko$date = lubridate::mdy(radpko$date)
afrogrid$date = lubridate::ymd(afrogrid$date)

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
prio = st_read(dsn = "./priogrid_cellshp", 
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

acled.sp <- SpatialPointsDataFrame(acled[24:23],         # reading in the dataframe as a spatial object
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

#############
# to analyze by government & rebel forces, need to keep inter1 code from ACLED data here
# could group by inter1 but would then need to re-combine I think?
#############

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

## verify data analyzed correctly ##
acled.sf = st_as_sf(prio.rad)
all.sf = st_as_sf(a.ag.prio)
pdf("../results/violence.pdf")
ggplot(data = all.sf) + geom_sf(data = acled.sf, fill = "grey") + geom_sf(aes(fill = event)) +
  scale_fill_viridis_b(option = "plasma") + labs(fill = "Violent Events Against Civilians")
dev.off()

## merge ##
merged.data = left_join(prio.rad, a.ag.prio, by = c("prio.grid", "date",
                                                    "geometry", "xcoord",
                                                    "ycoord", "col", "row"))

# test to see what data was merged
test = subset(merged.data, id > 0)

test2 = merged.data
test2[is.na(test2)] <- 0
test2 = test2 %>%
  group_by(prio.grid) %>%
  summarize(fatalities = sum(fatalities), event = sum(event))

test2 = subset(test2, event > 0)

test3 = merged.data
test3[is.na(test3)] <- 0
test3 = test3 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(pko_deployed), event = sum(event))

test3 = subset(test3, v > 0)

# let's clean the merged data frame
# start by getting rid of useless data
merged.data[14:25] = NULL # no need for this project to disaggregate PKs by region origination, so remove it

# add in control variables
prio.static = read.csv("prio-grid_static.csv")
prio.yearly = read.csv("prio-grid_yearly.csv")
names(prio.static)[1] = "prio.grid" # rename for merging
names(prio.yearly)[1] = "prio.grid" # rename for merging
prio.static$prio.grid = as.character(prio.static$prio.grid)
prio.yearly$prio.grid = as.character(prio.yearly$prio.grid)
afrogrid$prio.grid = as.character(afrogrid$prio.grid)
# extract year to merge prio yearly data
merged.data$year = year(merged.data$date)

# add various data to merged dataset
merged.data = left_join(merged.data, prio.static, by = c("prio.grid", "row", "col",
                                                         "xcoord", "ycoord"))
merged.data = left_join(merged.data, prio.yearly, by = c("prio.grid", "year"))

merged.data = left_join(merged.data, afrogrid, by = c("prio.grid", "date"))

# next rearrange the columns to put the geo-locational data last
merged.data = merged.data %>% 
  relocate(c("xcoord", "ycoord", "col", "row", "geometry"), .after = last_col())

# verify there are no missing observations
sum(is.na(merged.data$mission)) 


# finally, get rid of some of the useless variables 
merged.data[44:46] = NULL

# summary statistics
stargazer(a.ag, title = "ACLED Summary Statistics", align = TRUE, digits=1, font.size = "scriptsize",
          out = "../results/acled_stats.txt")


# clear everything except the merged data
rm(list = setdiff(ls(), "merged.data")) 
a = merged.data
rm(list = setdiff(ls(), "a")) # clear everything except the merged data
gc()
tapply(a$fatalities, a$country, sum, na.rm = TRUE)
tapply(a$event, a$country, sum, na.rm = TRUE)




# save RDS #

saveRDS(a, file = "merged_data.rds")

# # export merged data #

write.table(a, "../data/merged_data.txt", row.names = FALSE, sep = "|")
# this works, but to reload the data you'll need to load it in and transform the columns
# data fully merged, cleaned, and exported #

rm(list = ls())

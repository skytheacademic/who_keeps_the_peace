# 2nd order administrative unit merge #
# By: Adam Kunkel 

# Load Libraries
library(exactextractr); library(raster); library(rasterVis); library(rgdal)
library(rgeos); library(sf); library(sp); library(tidygeocoder)
library(tidyverse); library(tmap); library(viridis); library(lubridate)
library(spatialEco); library(gdata); library(designmatch) 
library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer)
library(janitor)

# turn off scientific notation
options(scipen = 999)

### set working directory ###
setwd("../data")

acled = read.csv("acled_precise_data.csv")
radadm = read.csv("radpko_adm2.csv", encoding="UTF-8") # solved encoding issue
radadm[14:25] = NULL # get rid of extra data

# change radadms's classification of Abyei from country unit to Sudan unit to match radadm
radadm$country[radadm$country=="abyei"] = "sudan"

# first let's clean the column names #
radadm = janitor::clean_names(radadm)
acled = janitor::clean_names(acled)

# change country and admin names to lowercase #
radadm$country = tolower(radadm$country)
acled$country = tolower(acled$country)
radadm$id = tolower(radadm$id)
acled$admin2 = tolower(acled$admin2)

## using gsub to get rid of underscores and numbers in names ##  
radadm$id = gsub("_.+", "", radadm$id)


# it looks like some regions change hands (i.e., South Sudan breaking off from Sudan,
# causing regions to change hands and messing up identifiers)
# admin2 levels do not match exactly #

# for example, see Sierra Leone
acled_sl = subset(acled, country == "sierra leone")
radadm_sl = subset(radadm, country == "sierra leone")
sl.sf = readRDS("./country_sf/sierraleone_sf.rds")

z = (unique(acled_sl$admin2)) %>%
  sort()
zz = (unique(radadm_sl$id)) %>%
  sort()
zzz = (unique(sl.sf$NAME_2)) %>%
  tolower() %>%
  sort()
z
zz
zzz
# extra regions, such as the Falaba district exist in the ACLED data but not the 
# radpko or GADM data due to it's recent creation: https://en.wikipedia.org/wiki/Falaba_District


#############################################################
## Use point.in.poly to determine when ACLED violence occurs 
  # within GADM adm2 units ##
#############################################################

# start by adding a dummy column so we know how many violent events occur in a grid per month
acled$event = 1

# change date-time
acled$event_date = lubridate::dmy(acled$event_date)
radadm$date = lubridate::mdy(radadm$date)

# subset ACLED data to violence against civilians and dates from before 2019 and after 
# 1999 to match RADPKO data (and to make analysis faster)
acled = subset(acled, acled$event_date < "2019-01-01" & acled$event_date > "1999-01-01" & 
                 acled$event_type == "Violence against civilians")

# round daily event data down to the 1st of the month to match ACLED data
acled$event_date <- floor_date(acled$event_date, "month")

wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

acled.sp <- SpatialPointsDataFrame(acled[24:23],         # reading in the dataframe as a spatial object
                                   acled,                 # the R object to convert
                                   proj4string = wgs84)   # assign a CRS 

# read in shapefiles
bu.sf = readRDS("./country_sf/burundi_sf.rds")
car.sf = readRDS("./country_sf/car_sf.rds")
ch.sf = readRDS("./country_sf/chad_sf.rds")
drc.sf = readRDS("./country_sf/drc_sf.rds")
ic.sf = readRDS("./country_sf/ivorycoast_sf.rds")
lb.sf = readRDS("./country_sf/liberia_sf.rds")
ma.sf = readRDS("./country_sf/mali_sf.rds")
sl.sf = readRDS("./country_sf/sierraleone_sf.rds")
ss.sf = readRDS("./country_sf/southsudan_sf.rds")
su.sf = readRDS("./country_sf/sudan_sf.rds")

all.sf = rbind(bu.sf, car.sf, ch.sf, drc.sf, ic.sf, lb.sf,
               ma.sf, sl.sf, ss.sf, su.sf)
# wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #common crs
# 
# all.sf = st_transform(all.sf, wgs84) # assign more recent CRS
all.sf[3] = NULL
all.sf[3:5] = NULL
all.sf[4:9] = NULL
names(all.sf)[2] = "country"
names(all.sf)[3] = "adm2"
all.sf$country = tolower(all.sf$country)
all.sf$adm2 = tolower(all.sf$adm2)
all.sf$area_sqkm = st_area(all.sf) # add mean of polygon to data as control variable

all.sp = as(all.sf, Class = "Spatial")

# run point.in.poly, which determines if a coordinate (ACLED data) exists within a polygon (PRIO)
# if the coordinate exists inside of the grid, it applies the characteristics of the polygon
# to the coordinate; i.e., the PRIO-grid number + coordinates + geometry is added to each 
# event in the ACLED data
b = point.in.poly(acled.sp, all.sp, sp = TRUE, duplicate = TRUE)
a = as.data.frame(b) # convert to dataframe
a[36:37] = NULL


# group fatalities and events by admin level & date
a.ag = a %>%
  group_by(adm2, event_date) %>%
  summarize(fatalities = sum(fatalities), event = sum(event))

# rename event_date to date for merging
names(a.ag)[2] = "date"

a.ag$adm2 = tolower(a.ag$adm2)
a.ag <- tibble::rowid_to_column(a.ag, "test") # add an ID column so we can verify what merged

adm2.merge = left_join(radadm, a.ag, by = c("date", "id" = "adm2"))

# merge the data
merged.data = left_join(adm2.merge, all.sf, by = c("country", "id" = "adm2"))

# let's see if it merged
pdf("../results/adm2_violence_again.pdf")
ggplot(data = merged.data) + geom_sf(aes(fill = fatalities, geometry = geometry)) +
  scale_fill_viridis_b(option = "plasma") + labs(fill = "adm2")
dev.off()

# I think the shapefiles admin names are not merging, which explains the missing map data
# could try full_join
# this is only an issue for mapping, as violence and pko_data merged

pdf("../results/adm2_peacekeeping.pdf")
ggplot(data = merged.data) + geom_sf(aes(fill = pko_deployed, geometry = geometry)) +
  scale_fill_viridis_b(option = "plasma") + labs(fill = "adm2")
dev.off()

test = subset(adm2.merge, test > 0)
test2 = adm2.merge
test2[is.na(test2)] <- 0
test2 = test2 %>%
  group_by(id) %>%
  summarize(fatalities = sum(fatalities), event = sum(event))
test2 = subset(test2, event > 0)


# remove everything but the merged data #
rm(list = setdiff(ls(), "merged.data")) 
a = merged.data
rm(list = setdiff(ls(), "a")) # clear everything except the merged data
gc()

# radadm uses GADM IDs, see: https://gadm.org/metadata.html


##########################################
### Regression Analysis ###
##########################################

# Create a "treatment" indicator telling us if PKs existed in a certain grid at a certain time 
a$t_ind = 0
a$t_ind[a$units_deployed >= 1] = 1
a$event.b = 0
a$event.b[a$event>0] = 1
a$death = 0
a$death[a$fatalities>0] = 1
b = a

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

### Logit Model ###
logit1 = glm.nb(formula = a$event.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob)
summary(logit1)

logit2 = glm.nb(formula = a$death ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob)
summary(logit2)

logit3 = glm.nb(formula = a$event.b ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$area_sqkm)
summary(logit3)

logit4 = glm.nb(formula = a$death ~ a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$area_sqkm)
summary(logit4)

########### testing w/ t_ind ###############

logit5 = glm.nb(a$event ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob)
summary(logit5)

logit6 = glm.nb(a$fatalities ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob)
summary(logit6)

logit7 = glm.nb(a$event ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$area_sqkm)
summary(logit7)

logit8 = glm.nb(a$fatalities ~ a$t_ind + a$units_deployed + a$untrp + a$unpol + a$unmob + a$f_untrp +
                  a$f_unpol + a$f_unmob + a$area_sqkm)
# not using nlights here because it might affect whether a violent event occurs but not whether a death happens
# not using urban_gc here because it might affect whether a violent event occurs but not whether a death happens

summary(logit8)

stargazer(logit3, logit4, logit7, logit8, title = "Results", align = TRUE, digits=3, font.size = "scriptsize",
          out = "../results/adm2_logit_table.txt")


# let's add a plot of Mali to check #
a.min = subset(a, country == "mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(id) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("id", "geometry")], y = a.min1, by = "id")
#aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")

pdf("../results/mali_violent_events.pdf")
plot_1
dev.off()

a.min2 = a.min %>%
  group_by(id) %>%
  summarize(v = sum(pko_deployed))

aa2 = merge(x = a.min[, c("id", "geometry")], y = a.min2, by = "id")
# aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/mali_pks.pdf")
plot_3
dev.off()

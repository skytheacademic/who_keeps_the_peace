# Who Keeps the Peace: Figures and Plots #

library(tidygeocoder)
library(tidyverse); library(viridis)
library(gdata)
library(magrittr); library(lubridate)

library(ggpubr); library(ggiraphExtra); library(coefplot); library(stargazer) # need to add these to dockerfile
library(spdep); library(gurobi); library(lme4); library(vtable)
library(sensitivitymw); library(lmtest); library(sandwich); library(magick)
library(ggeffects)


# turn off scientific notation
options(scipen = 999)

# reading in cleaned data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set to source file location
setwd("../") # back out to main folder

# plot proportion of women deployed to PKOs over time
b = readRDS("./data/kunkel_which_pks.rds") %>%
  group_by(date, country) %>%
  summarize(pks = sum(radpko_pko_deployed), women = sum(radpko_f_pko_deployed), men = sum(radpko_m_pko_deployed),
            death = max(ucdp_reb_vac_5), fatalities = sum(ucdp_reb_vac_all)) %>%
  mutate(prop_women = (women/(women + men)))

ggplot(b, aes(x = date, y = country, height = scales::rescale(prop_women))) + geom_ridgeline()
ggplot(b, aes(x = date, y = country, height = scales::rescale(women))) + geom_ridgeline()
ggplot(b, aes(x = date, y = country, height = scales::rescale(fatalities))) + geom_ridgeline()

####### Plots of naive models ####### 
a = readRDS("./data/kunkel_which_pks.rds")
a$radpko_pko_deployed = a$radpko_pko_deployed/100 # rescale variable

### Plots ###
library(jtools); library(broom.mixed)
# logits
reg0 = glm(ucdp_reb_vac_5 ~ radpko_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
             radpko_pko_lag + viol_6,
           data = a, family = negative.binomial(theta = 1))
names(reg0$coefficients) = c("(Intercept)", "Peacekeepers Deployed",
                             "Avg. Mountain", "Travel Time Nearest City", "Percent Urban",
                             "Peacekeeper Lag", "Violence 6 Months Before")
se_reg0 <- round(coeftest(reg0, vcov = vcovPL(reg0, cluster = a$prio.grid)),4)
se_reg0

reg00 = glm(ucdp_reb_vac_all ~ radpko_pko_deployed + prio_mountains_mean + prio_ttime_mean + prio_urban_gc + 
              radpko_pko_lag + viol_6,
            data = a, family = negative.binomial(theta = 1))
names(reg00$coefficients) = c("(Intercept)", "Peacekeepers Deployed",
                              "Avg. Mountain", "Travel Time Nearest City", "Percent Urban",
                              "Peacekeeper Lag", "Violence 6 Months Before")
se_reg00 <- round(coeftest(reg00, vcov = vcovPL(reg00, cluster = a$prio.grid)),4)
se_reg00

svg("./results/naive_models.svg", height = 8, width = 10)
plot_summs(se_reg0, se_reg00, model.names = c("Binary", "Count"), legend.title = "Model by Outcome",
           omit.coefs = c("(Intercept)", "Avg. Mountain"))
dev.off()


# make plot of women and men PKs over time

a = readRDS("./data/kunkel_which_pks.rds")
aa = a %>%
  group_by(date) %>%
  summarize(men = sum(radpko_m_pko_deployed), women = sum(radpko_f_pko_deployed)) %>%
  filter(date < "2018-01-01")



pdf("./results/desc_pks.pdf", width = 17, height = 10)
ggplot(aa, aes(x=date)) +
  geom_line( aes(y=men), size=1, color="#EB5307") + 
  geom_line( aes(y=women * 20), size=1, color="#9E314B") +
  scale_y_continuous(
    # Features of the first axis
    name = "Count (Men)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./20, name="Count (Women")) + 
  theme_pubclean() +
  theme(axis.title.y = element_text(color = "#EB5307", size=20),
        axis.title.y.right = element_text(color = "#9E314B", size=20)) +
  xlab("")
dev.off()

# long_aa <- aa %>%
#   pivot_longer(cols = c(men, women), 
#                names_to = "gender", 
#                values_to = "pks")
# 
# long_aa$gender[long_aa$gender=="men"] = "Men"
# long_aa$gender[long_aa$gender=="women"] = "Women"
# long_aa %>%
#   ggplot(aes(x = date, y = pks, color = gender)) +
#   geom_line(lwd = 1) +
#   labs(title = "Peacekeepers Over Time by Gender",
#        y = "Count",
#        color = "Gender") +
#   facet_wrap(~gender, scales = "free_y") +
#   theme_pubclean() +
#   theme(legend.position = "none") +
#   scale_color_manual(values = c("Men" = "#EB5307", "Women" = "#9E314B"))
# 
# dev.off()
# 
# svg("./results/desc_pks.svg", width = 20, height = 10)
# long_aa %>%
#   ggplot(aes(x = date, y = pks, color = gender)) +
#   geom_line(lwd = 1) +
#   labs(title = "Peacekeepers Over Time by Gender",
#        y = "Count",
#        color = "Gender") +
#   facet_wrap(~gender, scales = "free_y") +
#   theme_pubclean() +
#   theme(legend.position = "none") +
#   scale_color_manual(values = c("Men" = "#EB5307", "Women" = "#9E314B"))
# dev.off()




## Descriptive Map ##
library(sf)
library(janitor)
library(lubridate)
library(viridis)

a = readRDS("./data/kunkel_which_pks.rds") %>% 
  as.data.frame()

df = a %>%
  group_by(prio.grid) %>%
  summarize(f_pko_deployed = sum(radpko_f_pko_deployed), m_pko_deployed = sum(radpko_m_pko_deployed), 
            violence = sum(ucdp_reb_vac_all, ucdp_gov_vac_all))

# change 0s to NA to make plot prettier
df$f_pko_deployed[df$f_pko_deployed == 0] <- NA
df$m_pko_deployed[df$m_pko_deployed == 0] <- NA
df$violence[df$violence == 0] <- NA

# restructure the data so grids can be duplicated and pko/violence is on the same scale 
# and named the same variable
df.pk_f = subset(df, f_pko_deployed > 0) %>%
  select(-c("violence", "m_pko_deployed")) %>%
  rename(count = f_pko_deployed)
df.pk_f$ct.type = "Women PKs Deployed"
df.pk_m = subset(df, m_pko_deployed > 0) %>%
  select(-c("violence", "f_pko_deployed")) %>%
  rename(count = m_pko_deployed)
df.pk_m$ct.type = "Men PKs Deployed"
df.vo = subset(df, violence > 0) %>%
  select(-c("m_pko_deployed", "f_pko_deployed")) %>%
  rename(count = violence)
df.vo$ct.type = "Violent Deaths"

# rejoin to same column
dd = rbind(df.pk_f, df.pk_m, df.vo)

rm(list = setdiff(ls(), c("dd", "df"))) 
gc()

### MERGE ACLED DATA WITH PRIO GRID IDS ###
prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", # get prio shapefiles
                    stringsAsFactors = F)
afr_shp <- st_read(dsn = "./data/gadm/africa", layer = "afr_g2014_2013_0", # get Africa shapefiles
                   stringsAsFactors = F)

### save the CRS
proj_crs <- st_crs(prio_shp)
dd$gid = dd$prio.grid
dd$prio.grid = NULL
df$gid = df$prio.grid
df$prio.grid = NULL
# convert, get rid of useless data
df.prio = left_join(df, prio_shp, by = "gid") %>%
  select(-c(2:7))
df = left_join(df, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  select(-c("geometry", "col", "row"))
dd_ac = left_join(dd, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  select(-c("geometry", "col", "row"))
df_ac= df %>%
  drop_na(violence) # drop NAs
df_pk = df %>%
  drop_na(f_pko_deployed, m_pko_deployed)

# plot of variables as different colors and different shape


dsc.wom =
  ggplot(afr_shp) + geom_sf(aes(geometry = geometry), alpha = 0.3,fill = NA) + # e5695b
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=f_pko_deployed, colour = "#9E314B"), alpha=0.5, shape = 19) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 15), name="Count", labels = c("10,000", "20,000"), breaks = c(10000, 20000)) +
  theme_void()

dsc_wom = dsc.wom + 
  labs(colour = "Variable") + 
  scale_color_manual(labels = c("Women PKs Deployed"), values = c("#9E314B")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.2),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.2, 'cm')) + 
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical") +
  xlim(-14,37) + ylim(-12,21)

pdf("./results/women_map.pdf")
dsc_wom
dev.off()

svg("./results/women_map.svg")
dsc_wom
dev.off()

dsc.men =
  ggplot(afr_shp) + geom_sf(aes(geometry = geometry), alpha = 0.3,fill = NA) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=m_pko_deployed, colour = "#EB5307"), alpha=0.5, shape = 19) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 24), name="Count", labels = c("250,000", "500,000"), breaks = c(250000, 500000)) +
  theme_void()

dsc_men = dsc.men + 
  labs(colour = "Variable") + 
  scale_color_manual(labels = c("Men PKs Deployed"), values = c("#EB5307")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.18),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.2, 'cm')) + 
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical") +
  xlim(-14,37) + ylim(-12,21)

svg("./results/men_map.svg")
dsc_men
dev.off()













dsc.1 =
  ggplot(afr_shp) + geom_sf(aes(geometry = geometry), alpha = 0.3,fill = NA) + # e5695b
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=f_pko_deployed, colour = "#9E314B"), alpha=0.7, shape = 19) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=m_pko_deployed, colour = "grey"), alpha=0.4, shape = 19) +
  scale_fill_viridis_c(option="E") +
  # scale_size(range = c(1, 24), name="Count", labels = c("20,000", "40,000"), breaks = c(20000, 40000)) +
  theme_void()

dsc.1 = ggplot(afr_shp) + 
  geom_sf(aes(geometry = geometry), alpha = 0.3, fill = NA) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size = 2 * f_pko_deployed, colour = "#EB5307"), alpha = 0.8, shape = 19) +
  geom_point(data = df, aes(x = xcoord, y = ycoord, size = 2 * m_pko_deployed, colour = "darkgrey"), alpha = 0.4, shape = 19) +
  scale_size_continuous(range = c(1, 24), name = "Count", labels = c("20,000", "40,000"), breaks = c(10000, 20000)) +  # Adjust the range as needed
  scale_fill_viridis_c(option = "E") +
  theme_void()


# dsc =
  dsc.1 + labs(colour = "Variable") + 
  scale_color_manual(labels = c("Women PKs Deployed", "Men PKs Deployed"), values = c("#EB5307", "darkgrey")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.25, 0.3),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(5,5,5,5)), 
        legend.key.size = unit(0.2, 'cm')) + 
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical") +
  xlim(-14,37) + ylim(-12,21)

pdf("./results/desc_plot.pdf")
dsc
dev.off()

# end of plot


####################################################
##### Make map of single country - BEGINNING #######
####################################################

library(ggplot2)
library(tidyverse)
library(sf)
library(ggpubr)
library(tmap)
library(tmaptools)

b = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(country == "Democratic Republic of Congo")

b.ag = b %>%
  group_by(prio.grid) %>%
  summarize(fatalities = sum(ucdp_reb_vac_all), pks = sum(radpko_pko_deployed), t_ind = max(t_ind),
            men = sum(radpko_m_pko_deployed), women = sum(radpko_f_pko_deployed))

b.ag$fatalities[b.ag$fatalities == 0] <- NA
b.ag$pks[b.ag$pks == 0] <- NA
b.ag$t_ind[b.ag$t_ind == 0] <- NA
b.ag$men[b.ag$men == 0] <- NA
b.ag$women[b.ag$women == 0] <- NA

# now join geographic data so we can plot it

# read in PRIO files for grid coordinates
prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>%
  rename(prio.grid = gid, gid_center_lon = xcoord, gid_center_lat = ycoord) %>% 
  select(-c(col, row))

b.join = left_join(b.ag, prio, by = "prio.grid")

# read in DRC shapefiles
drc_00 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_0", 
                  stringsAsFactors = F)
drc_01 <- st_read(dsn = "./data/gadm/drc", layer = "gadm40_COD_1", 
                  stringsAsFactors = F)

### make plot of DRC, then add layers in several images to show the effect ###

# plot of 00 DRC

pdf("./results/drc/drc_00.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(geometry = drc_00$geometry), alpha = 0) +
  theme_void()
dev.off()

# plot of 01 DRC
pdf("./results/drc/drc_01.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) +
  theme_void()
dev.off()

# plot of grids over DRC
pdf("./results/drc/drc_01grids.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(geometry = b.join$geometry), alpha = 0) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) +
  theme_void()
dev.off()

pdf("./results/drc/drc_grids.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(geometry = b.join$geometry), alpha = 0) +
  theme_void()
dev.off()

# plot of violence in DRC
pdf("./results/drc/drc_violence.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(fill = b.join$fatalities, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#ffc4c4", high = "#ff3b3b", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,500)) +
  labs(fill = "Fatalities") +
  theme_void()
dev.off()

# plot of PKs, then men and women
pdf("./results/drc/drc_pks.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(fill = b.join$pks, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#2ABBE8", high = "#2A57E8", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,525000)) +
  labs(fill = "Peacekeepers") +
  theme_void()
dev.off()

pdf("./results/drc/drc_men.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(fill = b.join$men, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#F99A6B", high = "#EB5307", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,503524.3)) +
  labs(fill = "Men") +
  theme_void()
dev.off()

pdf("./results/drc/drc_women.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(fill = b.join$women, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#F96B8C", high = "#9E314B", space = "Lab", na.value = "grey89",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,22010)) +
  labs(fill = "Women") +
  theme_void()
dev.off()


rm(list = ls())



####################################################
##### Make map of single country - END #######
####################################################






## plot of gender balanced and unbalanced units over time ##


a = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(prio.grid == 127858) %>%
  relocate(c(ucdp_reb_vac_all, ucdp_gov_vac_all),
           .after = radpko_pko_deployed)
  filter(t_ind > 0)

#### Puzzle/framing data and plot ####
# Find rebel violence where there is lots of PKs

# sort first by total PKs, then sort by rebels?
# may have to round to nearest 10 or 100 first
pko = round(a$radpko_pko_deployed, -2)
table(pko)
a = subset(a, t_ind > 0 & radpko_f_pko_deployed == 0)
a = a[order(a$radpko_pko_deployed, decreasing=T), ] 
a = a[order(a$ucdp_reb_vac_all, decreasing=T), ] %>%
  relocate(c(ucdp_reb_vac_all, ucdp_gov_vac_all), 
           .after = radpko_pko_deployed) %>%
  relocate(c(radpko_pko_lag, radpko_f_untrp.p, radpko_f_unpol.p, radpko_f_unmob.p, 
             radpko_units_deployed, radpko_countries_deployed), 
           .before = t_ind)

prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>%
  filter(gid == 127858)
print(prio[2:3],)
# in this grid, from Jan. - Sep. 2009 had 533 civilian fatalities and averaged ~530 PKs

df = read.csv("./data/ucdp_ged/GEDEvent_v22_1.csv") %>%
  # make the date variable a date type
  mutate(date = ymd_hms(date_end)) %>%
  mutate(month = month(date)) %>%
  filter(type_of_violence == 3) %>%
  select(c(date, month, year, side_a_new_id, priogrid_gid, deaths_civilians, source_article,
           where_coordinates, where_description)) %>%
  # rename variable for ease of merging
  rename(prio.grid = priogrid_gid, ucdp_deaths = deaths_civilians) %>%
  select(-c("date")) %>%
  filter(prio.grid == 127858 & year == 2009 & month < 10)
# https://www.hrw.org/report/2009/12/13/you-will-be-punished/attacks-civilians-eastern-congo



# View(acled)
# order of operations: 
# sort violence and other variables
# search for grid coordinates w/ prio data
# search for real location in Google maps
# filter location in ACLED data, look for story within
rm(list = ls())
gc()

a = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(radpko_f_prop > 0)

a = a[order(a$radpko_pko_deployed, decreasing=F), ] 
a = a[order(a$radpko_f_prop, decreasing=T), ]
a = a[order(a$ucdp_reb_vac_all, decreasing=F), ] %>%
  relocate(c(ucdp_reb_vac_all, ucdp_gov_vac_all, radpko_f_prop), 
           .after = radpko_pko_deployed) %>%
  relocate(c(radpko_pko_lag, radpko_f_untrp.p, radpko_f_unpol.p, radpko_f_unmob.p, 
             radpko_units_deployed, radpko_countries_deployed), 
           .before = t_ind) %>%
  view()

prio = st_read(dsn = "./data/prio", 
               layer = "priogrid_cell", 
               stringsAsFactors = F) %>%
  filter(gid == 127138)
print(prio[2:3],)
rm(list = ls())
gc()

a = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(prio.grid == 127138) %>%
  relocate(c(ucdp_reb_vac_all, ucdp_gov_vac_all, viol_6), 
           .after = radpko_pko_deployed) %>%
  relocate(c(radpko_pko_lag, radpko_f_untrp.p, radpko_f_unpol.p, radpko_f_unmob.p, 
             radpko_units_deployed, radpko_countries_deployed), 
           .before = t_ind)

# UN contingents in Mushaki and Sake, two UN bases approx. 9 kms away from each other and near lake Kivu
# 324 deaths over the course of Jan. - May 2009
# 400 UN male PKs from June - Oct. 2009
# 42 deaths over that time
# 50 UN female PKs join contingent in Nov. 
# total share of PKs went from 400 to 300
# from Nov. 2009 - Mar. 2013, not a single civilian death by state or rebel actors





# 148741 - El Obeid


# Al Fashir - 150170: 2014-2017
# Population (2009) - ~500,000 
  # source: https://unhabitat.org/sites/default/files/download-manager-files/
  #         El%20Fasher%20and%20Abu%20Shouk%20Profile.pdf
# Number of civilians per PK - 1000
# Bunia pop (2008) - 302,000
# Number of civilians per PK - 63

# al_fashir = readRDS("./data/kunkel_which_pks.rds") %>%
#   dplyr::filter(year >= 2014 & year <= 2016) %>%
#   dplyr::filter(!(month >= 7 & year >= 2016)) %>%
#   dplyr::filter(prio.grid == 150170) %>%
#   relocate(ucdp_gov_vac_all:ucdp_reb_vac_all, .after = radpko_f_prop)
# al_fashir = al_fashir[order(al_fashir$month, decreasing=F), ]
# al_fashir = al_fashir[order(al_fashir$year, decreasing=F), ] 
# 
# # al_fashir$time = al_fashir$month - 6
# al_fashir$City = "Al Fashir"
# al_fashir <- al_fashir %>% 
#   group_by(City) %>% 
#   summarise(across(radpko_pko_deployed:radpko_f_pko_deployed, mean)) %>% 
#   ungroup()
# 
# 
# #### Bunia & Goma ####
# 
# # tell the story of Bunia 2008 (no women) vs Goma 2015 (lots of women)
# 
# # isolate 2008-7 to 2008-12 for Bunia
# # isolate 2016-3 to 2016-8 for Goma
# # plot violence in that area over time, and PKs, disaggregated by gender and comp
# 
# bunia = readRDS("./data/kunkel_which_pks.rds") %>%
#   dplyr::filter(year == 2008) %>%
#   dplyr::filter(month >= 7) %>%
#   dplyr::filter(prio.grid == 132181)
#   # dplyr::select(-c(2, 4:5, radpko_pko_lag,
#   #                  17:24, 33:34, 37:137))
# bunia$time = bunia$month - 6
# bunia$City = "Bunia"
# 
# goma = readRDS("./data/kunkel_which_pks.rds") %>%
#   dplyr::filter(year == 2016) %>%
#   dplyr::filter(month >= 3 & month <= 8) %>%
#   dplyr::filter(prio.grid == 127139)
#   # dplyr::select(-c(2, 4:5, radpko_pko_lag,
#   #                  17:24, 33:34, 37:137))
# goma$time = goma$month - 2
# goma$City = "Goma"
# 
# bunia <- bunia %>% 
#   group_by(prio.grid, City) %>% 
#   summarise(across(radpko_pko_deployed:radpko_f_pko_deployed, mean)) %>% 
#   ungroup()
# 
# goma <- goma %>% 
#   group_by(prio.grid, City) %>% 
#   summarise(across(radpko_pko_deployed:radpko_f_pko_deployed, mean)) %>% 
#   ungroup()

# need to convert data to long instead of wide form and classify by PKs and violence
bunia = pivot_longer(bunia, cols = c(radpko_untrp:radpko_f_unmob), 
                     names_prefix = "radpko_", names_to = "PKs")
goma = pivot_longer(goma, cols = c(radpko_untrp:radpko_f_unmob), 
                     names_prefix = "radpko_", names_to = "PKs")

# grouped barplot
# rbind by prio.grid

bunia_goma = rbind(bunia, goma)

bunia_goma %>%
  ggplot( aes(x=City, y=value, fill=PKs)) +
  geom_bar(stat="identity", width = 0.5, position="fill") +
  scale_fill_viridis(discrete=TRUE, name="") +
#  theme_ipsum() +
  ylab("Number of baby")


bunia %>% 
  ggplot( aes(x=time, y=value, fill=PKs)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Peacekeepers over a six month span in Bunia, DRC.")

goma %>% 
  ggplot( aes(x=time, y=value, fill=PKs)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Peacekeepers over a six month span in Bunia, DRC.")


### Could try this if Bunia vs. Goma story is not convincing
# tapply(a$radpko_f_untrp, a$prio.grid, range)
# 
# z = a %>% 
#   group_by(prio.grid) %>% 
#   summarise(FirsDate=min(radpko_f_untrp),LastDate=max(radpko_f_untrp))
# z$diff = z$FirsDate - z$LastDate


#### Make plot of violence and PKs deployed over time ####

# summarize by group #
a.ag = a %>%
  group_by(mission, date, country) %>%
  summarize(fatalities = sum(fatalities), event = sum(event), pks = sum(radpko_pko_deployed),
            g_death = sum(gov_death), r_death = sum(reb_death))
rm(a)
# plot over time

# let's try making joyplot still need to log deaths?
library(ggridges) # geom_density_ridges_gradient
library(viridis)  # scale_fill_viridis
library(hrbrthemes) # theme_ipsum

a.ag$log.pks = log(a.ag$pks + 1)
a.ag$log.fatalities = log(a.ag$fatalities + 1)
a.ag$log.g = log(a.ag$g_death + 1)
a.ag$log.r = log(a.ag$r_death + 1)

ggplot(data=a.ag, aes(x=date,y=pks, group=1)) +
  geom_line(colour = "blue") +
#  geom_line(aes(x=date,y=log.fatalities, group=1), colour = "red") +
  facet_wrap(~country)

# let's zoom in to Liberia

mali = subset(a.ag, country == "Mali")
ggplot(data=mali, aes(x=date,y=log.pks, group=1)) +
  geom_line(colour = "blue") +
  geom_line(aes(x=date,y=log.fatalities, group=1), colour = "red")
  
# during relatively stable level of pks, large jump in violence in July 2016 - February 2017
acled  = read.csv("./data/acled/1999-01-01-2021-12-31.csv")
library(lubridate)
acled$event_date = lubridate::dmy(acled$event_date)
acled = subset(acled, country == "Mali" & event_date > "2016-05-01" & event_date < "2018-01-01")
gc()

# make plot of violence and PKs over time to show where pks are and violence are
# might be useful to group by quarter (or aggregate over whole time period)



ggplot(data=a.ag, aes(x=date,y=log.pks, group=1)) +
  geom_line(colour = "black") +
  geom_line(aes(x=date,y=log.g, group=1), colour = "red") +
  geom_line(aes(x=date,y=log.r, group=1), colour = "blue") +
  facet_wrap(~mission)

# add violence and pks leads and lags #
a.ag = a.ag %>% 
  group_by(mission) %>% 
  mutate(viol_6.lag = 
           lag(fatalities, 6) + lag(fatalities, 5) + lag(fatalities, 4) + 
           lag(fatalities, 3) + lag(fatalities, 2) + lag(fatalities, 1)) %>%
  relocate(viol_6.lag, .after = event)

a.ag = a.ag %>% 
  group_by(mission) %>% 
  mutate(viol_6.lead = 
           lead(fatalities, 6) + lead(fatalities, 5) + lead(fatalities, 4) + 
           lead(fatalities, 3) + lead(fatalities, 2) + lead(fatalities, 1)) %>%
  relocate(viol_6.lead, .after = viol_6.lag)

a.ag = a.ag %>% 
  group_by(mission) %>% 
  mutate(pks_6.lag = 
           lag(pks, 6) + lag(pks, 5) + lag(pks, 4) + 
           lag(pks, 3) + lag(pks, 2) + lag(pks, 1)) %>%
  relocate(pks_6.lag, .after = viol_6.lead)

a.ag = a.ag %>% 
  group_by(mission) %>% 
  mutate(pks_6.lead = 
           lead(pks, 6) + lead(pks, 5) + lead(pks, 4) + 
           lead(pks, 3) + lead(pks, 2) + lead(pks, 1)) %>%
  relocate(pks_6.lead, .after = pks_6.lag)

round(sort(tapply(a.ag$pks, a.ag$mission, mean)), 2)
round(sort(tapply(a.ag$fatalities, a.ag$mission, mean)), 2)
round(table(a.ag$pks, a.ag$fatalities), 2)

# exclude 0 pks, find when the numbers were the closest
summary(lm(fatalities ~ pks, data = a.ag))
a.ag$pks[a.ag$pks == 0] = NA
a.ag$log.pks[a.ag$log.pks == 0] = NA

a.ag$pks = round(a.ag$pks, 2)

a.ag$diff = a.ag$log.pks - a.ag$log.fatalities 
a.ag = a.ag %>%
  relocate(diff, .after = pks)

unmiss = subset(a.ag, mission == "UNMISS")
ggplot(data=unmiss, aes(x=date,y=log.pks, group=1)) +
  geom_line(colour = "blue") +
  geom_line(aes(x=date,y=log.fatalities, group=1), colour = "red")

# UNMIS, UNMISS, and MONUSCO could be good candidates for this
a.sub = subset(a.ag, mission == "UNMIS" | mission == "UNMISS" | mission == "MONUSCO")
# unmiss = subset(a.ag, mission == "UNMISS")
# monusco = subset(a.ag, mission == "MONUSCO")
ggplot(data=a.sub, aes(x=date,y=pks, group=1)) +
  geom_line(colour = "blue") +
  geom_line(aes(x=date,y=fatalities, group=1), colour = "red") +
  facet_grid(mission ~ .)













##### Plot of Mali w/ PRIO grids #####
# let's add a plot of Mali to check #
a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
#aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent events")

pdf("../results/mali_violent_events_prio.pdf")
plot_1
dev.off()

a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
# aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/mali_pks_prio.pdf")
plot_3
dev.off()



### Aggregate PK & PKO locations w/ violence and violent events ###

a.min = subset(a, country == "Mali")
table(a.min$fatalities)

a.min1 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min1, by = "prio.grid")
aa$v[aa$v == 0] <- NA

plot_1 = ggplot(data = aa) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_1.pdf")
plot_1
dev.off()



table(a.min$t_ind)
a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(t_ind))

aa1 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa1$v[aa1$v == 0] <- NA

plot_2 = ggplot(data = aa1) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "PKO")

pdf("../results/plot_2.pdf")
plot_2
dev.off()


a.min2 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa2 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min2, by = "prio.grid")
aa2$v[aa2$v == 0] <- NA

plot_3 = ggplot(data = aa2) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_3.pdf")
plot_3
dev.off()


a.min3 = a.min %>%
  group_by(prio.grid) %>%
  summarize(v = sum(event))

aa3 = merge(x = a.min[, c("prio.grid", "geometry")], y = a.min3, by = "prio.grid")
aa3$v[aa3$v == 0] <- NA

plot_4 = ggplot(data = aa3) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Violent Events")

pdf("../results/plot_4.pdf")
plot_4
dev.off()


##### Time-series plots of MINUSMA #####
#subset
a.una = subset(a, country == "Democratic Republic of Congo") 


tapply(a.una$fatalities, a.una$date, sum)

# dates to subset for MINUSMA: 2017-06-01 2017-07-01 2017-08-01 2017-09-01
a.min.02 = subset(a.una, date == "2017-02-01")
a.min.03 = subset(a.una, date == "2017-03-01")
a.min.04 = subset(a.una, date == "2017-04-01")
a.min.05 = subset(a.una, date == "2017-05-01")


## 2017-02-01 ##
a.min.02.ft = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa4 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.ft, by = "prio.grid")
aa4$v[aa4$v == 0] <- NA

plot_5 = ggplot(data = aa4) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_5.pdf")
plot_5
dev.off()


a.min.02.pk = a.min.02 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa5 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.02.pk, by = "prio.grid")
aa5$v[aa5$v == 0] <- NA

plot_6 = ggplot(data = aa5) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_6.pdf")
plot_6
dev.off()


## 2017-03-01 ##

a.min.03.ft = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa6 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.ft, by = "prio.grid")
aa6$v[aa6$v == 0] <- NA

plot_7 = ggplot(data = aa6) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_7.pdf")
plot_7
dev.off()


a.min.03.pk = a.min.03 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa7 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.03.pk, by = "prio.grid")
aa7$v[aa7$v == 0] <- NA

plot_8 = ggplot(data = aa7) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_8.pdf")
plot_8
dev.off()


## 2017-04-01 ##

a.min.04.ft = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa8 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.ft, by = "prio.grid")
aa8$v[aa8$v == 0] <- NA

plot_9 = ggplot(data = aa8) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_9.pdf")
plot_9
dev.off()


a.min.04.pk = a.min.04 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa9 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.04.pk, by = "prio.grid")
aa9$v[aa9$v == 0] <- NA

plot_10 = ggplot(data = aa9) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_10.pdf")
plot_10
dev.off()


## 2017-05-01 ##

a.min.05.ft = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(fatalities))

aa10 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.ft, by = "prio.grid")
aa10$v[aa10$v == 0] <- NA

plot_11 = ggplot(data = aa10) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Fatalities")

pdf("../results/plot_11.pdf")
plot_11
dev.off()


a.min.05.pk = a.min.05 %>%
  group_by(prio.grid) %>%
  summarize(v = sum(radpko_pko_deployed))

aa11 = merge(x = a.una[, c("prio.grid", "geometry")], y = a.min.05.pk, by = "prio.grid")
aa11$v[aa11$v == 0] <- NA

plot_12 = ggplot(data = aa11) + geom_sf(aes(fill = v, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") + labs(fill = "Peacekeepers")

pdf("../results/plot_12.pdf")
plot_12
dev.off()




# marginal effects on gender balance #
reg2.bal = ggpredict(reg2, terms = "t_bal", condition = c(t_unbal = 0))
reg2.bal$group = "Incumbent Deaths, Gender Balanced PKs"
reg2.unbal = ggpredict(reg2, terms = "t_unbal", condition = c(t_bal = 0))
reg2.unbal$group = "Incumbent Deaths, Gender Unbalanced PKs"
reg2_gg = rbind(reg2.bal, reg2.unbal)
reg4.bal = ggpredict(reg4, terms = "t_bal", condition = c(t_unbal = 0))
reg4.bal$group = "Rebels Deaths, Gender Balanced PKs"
reg4.unbal = ggpredict(reg4, terms = "t_unbal", condition = c(t_bal = 0))
reg4.unbal$group = "Rebel Deaths, Gender Unbalanced PKs"
reg4_gg = rbind(reg4.bal, reg4.unbal)

gen_death = rbind(reg2_gg, reg4_gg)

pdf("./results/pks_pred_gen.pdf")
ggplot(gen_death) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  ylim(-0.01, 0.15) + theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Faction and Gender Balance of PK Unit")) +
  scale_x_continuous(breaks = seq(0,1,1))
dev.off()
##### Make Odds Ratio Plots and Turn into gif ####
# exp the coefficients and confidence intervals, then turn into a dataframe
# from there, plot the line and the confidence bands
### REG 1 ###
reg1.cf = exp(reg1$coefficients) %>%
  as.data.frame()
reg1.ci = exp(confint(reg1)) %>%
  as.data.frame()
reg1.cf = cbind(reg1.cf, reg1.ci)
names(reg1.cf)[1] = "Government_event"
names(reg1.cf)[2] = "ci_low"
names(reg1.cf)[3] = "ci_high"
reg1.cf = reg1.cf[-c(12:15),]
reg1.cf$row_names = row.names(reg1.cf)
y_labs = rev(c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountains","Avg. Travel Time", 
               "Percent Urban", "Avg. Night Lights", "Sum Population", "Population Density", 
               "PKO Lag", "Total Violence 6 Months Before", "Intercept"))
level_order = rev(c("t_bal", "t_unbal", "mountains_mean", "ttime_mean", "urban_gc", "nlights_calib_mean",
                    "pop_gpw_sum", "pop.dens", "pko_lag", "viol_6", "(Intercept)"))
svg("./results/or_gov_event_b.svg")
ggplot(reg1.cf, aes(y = factor(row_names, level = level_order), x = Government_event)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") + geom_point(size = 2.5, color = "#99325D") +
  xlim(0,2.35) + theme_pubclean() + scale_y_discrete(labels = y_labs) + ylab("") +
  xlab("Odds ratio") + ggtitle("Gender Balance of Peacekeeping Unit and Risk of Violence by Government")
dev.off()
# export via r viewer at 1220 x 600 as SVG
### REG 2 ###
reg2.cf = exp(reg2$coefficients) %>%
  as.data.frame()
reg2.ci = exp(confint(reg2)) %>%
  as.data.frame()
reg2.cf = cbind(reg2.cf, reg2.ci)
names(reg2.cf)[1] = "Government_death"
names(reg2.cf)[2] = "ci_low"
names(reg2.cf)[3] = "ci_high"
reg2.cf = reg2.cf[-c(10:13),]
y_labs_d = rev(c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Mountains","Avg. Travel Time", 
                 "Sum Population", "Population Density", "PKO Lag", "Total Violence 6 Months Before", 
                 "Intercept"))
level_order_d = rev(c("t_bal", "t_unbal", "mountains_mean", "ttime_mean", "pop_gpw_sum", "pop.dens", 
                      "pko_lag", "viol_6", "(Intercept)"))
reg2.cf$row_names = row.names(reg2.cf)
pdf("./results/or_gov_death_b.pdf")
ggplot(reg2.cf, aes(y = factor(row_names, level = level_order_d), x = Government_death)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2.5, color = "#99325D") +
  xlim(0,2.35) +
  theme_pubclean() +
  scale_y_discrete(labels = y_labs_d) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Gender Balance of Peacekeeping Unit and Risk of Death by Government")
dev.off()
### REG 3 ###
reg3.cf = exp(reg3$coefficients) %>%
  as.data.frame()
reg3.ci = exp(confint(reg3)) %>%
  as.data.frame()
reg3.cf = cbind(reg3.cf, reg3.ci)
names(reg3.cf)[1] = "Rebel_event"
names(reg3.cf)[2] = "ci_low"
names(reg3.cf)[3] = "ci_high"
reg3.cf = reg3.cf[-c(4,7,12:15),] # removing int. effects and avg mountains and night 
# lights because it doesn't look good
y_labs = rev(c("Balanced PK Unit", "Unbalanced PK Unit", "Avg. Travel Time", 
               "Percent Urban", "Sum Population", "Population Density", 
               "PKO Lag", "Total Violence 6 Months Before", "Intercept"))
level_order = rev(c("t_bal", "t_unbal", "ttime_mean", "urban_gc",
                    "pop_gpw_sum", "pop.dens", "pko_lag", "viol_6", "(Intercept)"))
reg3.cf$row_names = row.names(reg3.cf)
ggplot(reg3.cf, aes(y = factor(row_names, level = level_order), x = Rebel_event)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2.5, color = "#239929") +
  xlim(0,1.32) +
  theme_pubclean() +
  scale_y_discrete(labels = y_labs) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Gender Balance of Peacekeeping Unit and Risk of Violence by Rebels")

### REG 4 ###
reg4.cf = exp(reg4$coefficients) %>%
  as.data.frame()
reg4.ci = exp(confint(reg4)) %>%
  as.data.frame()
reg4.cf = cbind(reg4.cf, reg4.ci)
names(reg4.cf)[1] = "Rebel_death"
names(reg4.cf)[2] = "ci_low"
names(reg4.cf)[3] = "ci_high"
reg4.cf = reg4.cf[-c(4,10:13),]
y_labs_d = rev(c("Balanced PK Unit", "Unbalanced PK Unit","Avg. Travel Time", 
                 "Sum Population", "Population Density", "PKO Lag", "Total Violence 6 Months Before", 
                 "Intercept"))
level_order_d = rev(c("t_bal", "t_unbal", "ttime_mean", "pop_gpw_sum", "pop.dens", 
                      "pko_lag", "viol_6", "(Intercept)"))
reg4.cf$row_names = row.names(reg4.cf)
pdf("./results/or_reb_death_b.pdf")
ggplot(reg4.cf, aes(y = factor(row_names, level = level_order_d), x = Rebel_death)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 2.5, color = "#239929") +
  xlim(0,1.32) +
  theme_pubclean() +
  scale_y_discrete(labels = y_labs_d) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Gender Balance of Peacekeeping Unit and Risk of Death by Rebels")
dev.off()

# now read them in and make into gif
or_p1 <- image_read("./results/or_gov_death_b.svg")
or_p2 <- image_read("./results/or_gov_event_b.svg")
or_p3 <- image_read("./results/or_reb_death_b.svg")
or_p4 <- image_read("./results/or_reb_event_b.svg")


# You can repeat/replicate an image just like you can in other vectors, with rep(). 
mc_gif = rep(or_p1, 4)

# You can recode/replace just like data vectors. 
mc_gif[2] = or_p2
mc_gif[3] = or_p3
mc_gif[4] = or_p4
mc_gif
mc_gif = image_animate(mc_gif, delay = 1500)

# To write/save your new GIFs, use the image_write function. 
image_write_gif(mc_gif, "./results/gender_violence_or.gif", delay = 15)



### plots 5 and 7
reg5.cf = exp(reg5$coefficients) %>%
  as.data.frame()
reg5.ci = exp(confint(reg5)) %>%
  as.data.frame()
reg5.cf = cbind(reg5.cf, reg5.ci)
names(reg5.cf)[1] = "Government event"
names(reg5.cf)[2] = "ci_low"
names(reg5.cf)[3] = "ci_high"
reg5.cf = reg5.cf[-c(12:15),]

reg7.cf = exp(reg7$coefficients) %>%
  as.data.frame()
reg7.ci = exp(confint(reg7)) %>%
  as.data.frame()
reg7.cf = cbind(reg7.cf, reg7.ci)
names(reg7.cf)[1] = "Rebel death"
names(reg7.cf)[2] = "ci_low"
names(reg7.cf)[3] = "ci_high"
reg7.cf = reg7.cf[-c(10:13),]









#### ME effects and plot ####
# marginal effects on peacekeeper composition

# 0 violent events in the last 6 months
reg6.trp = ggpredict(reg6, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 0))
reg6.trp$group = "Incumbent Deaths, Majority Troop PKs"
reg8.trp = ggpredict(reg8, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 0))
reg8.trp$group = "Rebels Deaths, Majority Troop PKs"
death_trp_pred = rbind(reg6.trp, reg8.trp)
ggplot(death_trp_pred) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  ggtitle("Predicted Probability of violence based on treatment, 0 violent events in the last 6 months")

# 5 violent events in the last 6 months
reg6.trp = ggpredict(reg6, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 5))
reg6.trp$group = "Incumbent Deaths, Majority Troop PKs"
reg8.trp = ggpredict(reg8, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 5))
reg8.trp$group = "Rebels Deaths, Majority Troop PKs"
death_trp_pred = rbind(reg6.trp, reg8.trp)
ggplot(death_trp_pred) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  ggtitle("Predicted Probability of violence based on treatment, 5 violent events in the last 6 months")

# 10 
reg6.trp = ggpredict(reg6, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 10))
reg6.trp$group = "Incumbent Deaths, Majority Troop PKs"
reg8.trp = ggpredict(reg8, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 10))
reg8.trp$group = "Rebels Deaths, Majority Troop PKs"
death_trp_pred = rbind(reg6.trp, reg8.trp)
ggplot(death_trp_pred) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  ggtitle("Predicted Probability of violence based on treatment, 10 violent events in the last 6 months")

# 20
reg6.trp = ggpredict(reg6, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 20))
reg6.trp$group = "Incumbent Deaths, Majority Troop PKs"
reg8.trp = ggpredict(reg8, terms = "untrp_maj", condition = c(unpol_maj = 0, unmob_maj = 0, viol_6 = 20))
reg8.trp$group = "Rebels Deaths, Majority Troop PKs"
death_trp_pred = rbind(reg6.trp, reg8.trp)
ggplot(death_trp_pred) +
  geom_line(aes(x, predicted, colour = group)) +
  geom_ribbon(aes(x, ymin = conf.low, ymax = conf.high, colour = group, 
                  fill = group), linetype = "dashed", alpha = 0.1, show.legend = F) +
  ylab("Predicted Pr(Civilian Deaths)") + theme_pubclean() +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  ggtitle("Predicted Probability of violence based on treatment, 20 violent events in the last 6 months")

# now read them in and make into gif
or_p1 <- image_read("./results/pr_vac_0.svg")
or_p2 <- image_read("./results/pr_vac_5.svg")
or_p3 <- image_read("./results/pr_vac_10.svg")
or_p4 <- image_read("./results/pr_vac_20.svg")


# You can repeat/replicate an image just like you can in other vectors, with rep(). 
mc_gif = rep(or_p1, 4)

# You can recode/replace just like data vectors. 
mc_gif[2] = or_p2
mc_gif[3] = or_p3
mc_gif[4] = or_p4
mc_gif
mc_gif = image_animate(mc_gif, delay = 1500)

image_write_gif(mc_gif, "./results/pr_vac.gif", delay = 5)


reg6.pol = ggpredict(reg6, terms = "unpol_maj", condition = c(untrp_maj = 0, unmob_maj = 0))
reg6.pol$group = "Incumbent Deaths, Majority Police PKs"
reg8.pol = ggpredict(reg8, terms = "unpol_maj", condition = c(untrp_maj = 0, unmob_maj = 0))
reg8.pol$group = "Rebels Deaths, Majority Police PKs"

reg6.mob = ggpredict(reg6, terms = "unmob_maj", condition = c(untrp_maj = 0, unpol_maj = 0))
reg6.mob$group = "Incumbent Deaths, Majority Observers PKs"
reg8.mob = ggpredict(reg8, terms = "unmob_maj", condition = c(untrp_maj = 0, unpol_maj = 0))
reg8.mob$group = "Rebels Deaths, Majority Observers PKs"


rm(list = setdiff(ls(), "a")) 
gc()

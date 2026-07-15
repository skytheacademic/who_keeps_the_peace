# make_slides.R -- reproduces the JOB-TALK presentation figures for "Who Keeps the Peace?".
#
# This script lives on the `job-talk` branch ONLY. The paper's figures are produced by the
# replication pipeline on main (analyze_data.R writes map_prop, the 2SLS plots, match_OR, and the
# cutoff plots; match_data.R writes the loveplot) -- nothing here duplicates a paper-figure output.
# make_slides.R is self-contained: it carries its own groundhog preamble and re-fits/re-reads the
# models and spatial data it needs, so the branch stands alone.
#
# Run top-to-bottom from the repo root, after clean_data.R + match_data.R.

library(groundhog)
groundhog.day <- "2026-06-01"
pkgs <- c("tidyverse", "magrittr", "lubridate", "sf", "viridis", "ggridges",
          "ggpubr", "lfe", "fixest", "MASS", "sandwich", "lmtest",
          "marginaleffects", "jtools", "broom", "ggeffects", "stargazer", "janitor")
groundhog.library(pkgs, groundhog.day)

# This script co-loads MASS (negbin models) with dplyr-heavy plotting code, so make sure the
# dplyr verbs win over MASS::select and stats::filter/lag.
select <- dplyr::select; filter <- dplyr::filter; lag <- dplyr::lag

options(scipen = 999)
setwd("Z:/who_keeps")   # run from repo root; edit if your path differs


# ==================== matched/unmatched odds-ratio + prediction slide figures ====================
# Slide-only companions to the paper's match_OR figure (unmatch_OR, match_OR_binary, and the
# matched-prediction plots). Models are re-fit here so this script stands alone.

a = readRDS("./data/kunkel_which_pks.rds")
c = readRDS("./data/kunkel_wpks_matched_gender.rds")

# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019))
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)

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

coef_unmatch = c("Gender-mixed PK Unit", "Unbalanced PK Unit", "Avg. Mountain",
                 "Travel Time Nearest City", "Perc. Urban","PK Lag",
                 "Violence 6 Months Before")
coef_match = c("Gender-mixed PK Unit", "Travel Time Nearest City", #excluding perc. urban and PK lag (aesthetics)
               "Population Sum", "Population Density", "Violence 6 Months Before")

pdf("./results/unmatch_OR.pdf")
plot_summs(se_reg9, se_reg10, exp = T, coefs = coef_unmatch, model.names =
             c("Binary", "Count"), legend.title = "Model by Outcome")
dev.off()

pdf("./results/match_OR_binary.pdf", width = 10, height = 8)
plot_summs(se_reg11, exp = T, coefs = coef_match) +
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18),
        axis.title.x.bottom = element_text(size = 22), legend.text=element_text(size=18),
        legend.title = element_text(size=18)) +
  xlab("Odds Ratios") + scale_x_continuous(limits = c(0.1, 1.1))
dev.off()

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


# ==================== marginal-effects prediction plots ====================
a = readRDS("./data/kunkel_which_pks.rds")
# Re-scale PK variable for statistical analyses (per Fjelde et al. (2019))
a$radpko_m_pko_deployed = a$radpko_m_pko_deployed/100
a$radpko_f_pko_deployed = a$radpko_f_pko_deployed/100

a = a %>% # re-scale proportion so that results make sense
  mutate(radpko_f_prop = 10*radpko_f_prop, radpko_m_prop = 10*radpko_m_prop)

reg1 = feols(fml = (ucdp_reb_vac_all ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid),
             data = a, cluster = "prio.grid")
reg2 = feols(fml = (ucdp_reb_vac_5 ~ radpko_f_pko_deployed + radpko_m_pko_deployed | time + prio.grid),
             data = a, cluster = "prio.grid")
reg3 = feols(fml = (ucdp_reb_vac_all ~ radpko_f_prop | time + prio.grid), data = a, cluster = "prio.grid")
reg4 = feols(fml = (ucdp_reb_vac_5 ~ radpko_f_prop | time + prio.grid), data = a, cluster = "prio.grid")
reg5 = feols(fml = (ucdp_reb_vac_all ~ radpko_m_prop | time + prio.grid), data = a, cluster = "prio.grid")
reg6 = feols(fml = (ucdp_reb_vac_5 ~ radpko_m_prop | time + prio.grid), data = a, cluster = "prio.grid")

### side-by-side plots ###
# predicted total violence when women PKs deploy #
pdf("./results/total_women_fatalities_pred.pdf", height = 10, width = 10)
plot_predictions(reg1, condition = c("radpko_f_pko_deployed"), vcov = FALSE) +  # vcov=FALSE: newer marginaleffects refuses SEs on FE models
  xlab("Total Female Peacekeepers Deployed") + ylab("Predicted Civilian Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()

# predicted total violence when men PKs deploy #
pdf("./results/total_men_fatalities_pred.pdf", height = 10, width = 10)
plot_predictions(reg1, condition = c("radpko_m_pko_deployed"), vcov = FALSE) +
  xlab("Total Male Peacekeepers Deployed") + ylab("Predicted Civilian Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()

# predicted Pr(violence) when Female PKs deployed #
pdf("./results/total_women_pr_death_pred.pdf", height = 10, width = 10)
plot_predictions(reg2, condition = "radpko_f_pko_deployed", vcov = FALSE) +
  xlab("Total Female Peacekeepers Deployed") + ylab("Predicted Pr(Civilian) Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()

# predicted Pr(violence) when male PKs deployed #
pdf("./results/total_men_pr_death_pred.pdf", height = 10, width = 10)
plot_predictions(reg2, condition = "radpko_m_pko_deployed", vcov = FALSE) +
  xlab("Total Male Peacekeepers Deployed") + ylab("Predicted Pr(Civilian) Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()

## predicted total violence when prop deployed increases
# women
pdf("./results/prop_women_fatalities_pred.pdf", height = 10, width = 10)
plot_predictions(reg3, condition = "radpko_f_prop", vcov = FALSE) +
  xlab("Proportion Female Peacekeepers Deployed") + ylab("Predicted Civilian Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24)) +
  xlim(c(0, 0.20001)) +
  ylim(c(-0.35, 0.3))
dev.off()

# men
pdf("./results/prop_men_fatalities_pred.pdf", height = 10, width = 10)
plot_predictions(reg5, condition = "radpko_m_prop", vcov = FALSE) +
  xlab("Proportion Male Peacekeepers Deployed") + ylab("Predicted Civilian Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()

## predicted Pr(violence) when prop deployed increases
# women
pdf("./results/prop_women_pr_death_pred.pdf", height = 10, width = 10)
plot_predictions(reg4, condition = "radpko_f_prop", vcov = FALSE) +
  xlab("Proportion Female Peacekeepers Deployed") + ylab("Predicted Pr(Civilian) Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24)) +
  xlim(c(0, 0.20001)) +
  ylim(c(-0.0075, 0.012))
dev.off()

# men
pdf("./results/prop_men_pr_death_pred.pdf", height = 10, width = 10)
plot_predictions(reg6, condition = "radpko_m_prop", vcov = FALSE) +
  xlab("Proportion Male Peacekeepers Deployed") + ylab("Predicted Pr(Civilian) Deaths by Rebels") +
  theme_pubclean() +
  theme(axis.text.y = element_text(size=22), axis.text.x =element_text(size=22),
        axis.title.x.bottom = element_text(size = 24), axis.title.y.left = element_text(size=24))
dev.off()


# ==================== female-PK ridgelines by country ====================
# plot proportion of women deployed to PKOs over time
b = readRDS("./data/kunkel_which_pks.rds") %>%
  group_by(date, country) %>%
  summarize(pks = sum(radpko_pko_deployed), women = sum(radpko_f_pko_deployed), men = sum(radpko_m_pko_deployed),
            death = max(ucdp_reb_vac_5), fatalities = sum(ucdp_reb_vac_all)) %>%
  mutate(prop_women = (women/(women + men)))
b$country[b$country=="sudan"] = "Sudan"

## ridgline plots ##
# prop #
pdf("./results/fem_pks_prop_deployed_country.pdf", height = 10, width = 15)
ggplot(b, aes(x = date, y = country, height = scales::rescale(prop_women))) + geom_ridgeline() + theme_pubclean() +
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18),
  axis.title.x.bottom = element_text(size = 22), axis.title.y.left = element_text(size=22))
dev.off()

# count #
pdf("./results/fem_pks_count_deployed_country.pdf", height = 10, width = 15)
ggplot(b, aes(x = date, y = country, height = scales::rescale(women))) + geom_ridgeline() + theme_pubclean() +
  theme(axis.text.y = element_text(size=18), axis.text.x =element_text(size=18),
  axis.title.x.bottom = element_text(size = 22), axis.title.y.left = element_text(size=22))
dev.off()


# ==================== descriptive maps: women / men peacekeepers deployed ====================
# (map_prop -- the paper's descriptive map -- is produced by analyze_data.R on main)
a = readRDS("./data/kunkel_which_pks.rds") %>%
  as.data.frame()

df = a %>%
  group_by(prio.grid) %>%
  summarize(f_pko_deployed = sum(radpko_f_pko_deployed), m_pko_deployed = sum(radpko_m_pko_deployed),
            violence = sum(ucdp_reb_vac_all, ucdp_gov_vac_all))

prio_shp <- st_read(dsn = "./data/prio", layer = "priogrid_cell", # get prio shapefiles
                    stringsAsFactors = F)
afr_shp <- st_read(dsn = "./data/gadm/africa", layer = "afr_g2014_2013_0", # get Africa shapefiles
                   stringsAsFactors = F)

df$gid = df$prio.grid
df$prio.grid = NULL
df = left_join(df, prio_shp, by = "gid") %>%
  as.data.frame() %>%
  select(-c("geometry", "col", "row"))

dsc.wom =
  ggplot(afr_shp) + geom_sf(aes(geometry = geometry), alpha = 0.3,fill = NA) + # e5695b
  geom_point(data = df, aes(x = xcoord, y = ycoord, size=f_pko_deployed, colour = "#9E314B"), alpha=0.5, shape = 19) +
  scale_fill_viridis_c(option="E") +
  scale_size(range = c(.1, 15), name="Count", labels = c("10,000", "20,000"), breaks = c(10000, 20000)) +
  theme_void()

dsc_wom = dsc.wom +
  labs(colour = "Variable") +
  scale_color_manual(labels = c("Women PKs Deployed"), values = c("#9E314B")) +
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.35, 0.28),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(10,10,10,10)),
        legend.key.size = unit(0.2, 'cm')) +
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical") +
  xlim(-14,37) + ylim(-12,21)

pdf("./results/women_map.pdf")
dsc_wom
dev.off()

svg("./results/women_map.svg", height = 10, width = 15)
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
  theme(legend.background = element_rect(color = "black"), legend.position = c(0.35, 0.28),
        plot.margin = unit(c(0,0,0,0), "cm"), legend.margin=margin(c(10,10,10,10)),
        legend.key.size = unit(0.2, 'cm')) +
  guides(shape = guide_legend(order = 1),col = guide_legend(order = 2), legend.direction="vertical") +
  xlim(-14,37) + ylim(-12,21)

svg("./results/men_map.svg", height = 10, width = 15)
dsc_men
dev.off()

pdf("./results/men_map.pdf", height = 10, width = 15)
dsc_men
dev.off()


# ==================== DRC single-country map suite + IV illustration ====================
##### Make map of single country - BEGINNING #######
b = readRDS("./data/kunkel_which_pks.rds") %>%
  filter(country == "Democratic Republic of Congo")

radpko = read.csv("./data/radpko/radpko_bases.csv") %>%
  filter(mission == "MONUC" | mission == "MONUSCO") %>%
  select(latitude, longitude, pko_deployed)

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
proj_crs <- st_crs(drc_01)

radpko <- st_as_sf(radpko, coords = c("longitude", "latitude"), crs = proj_crs)

radpko <- st_join(drc_01, radpko)
radpko = radpko %>%
  as.data.frame() %>%
  group_by(NAME_1) %>%
  summarize(pks = sum(pko_deployed))

drc_01_pks = left_join(drc_01, radpko)

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

## try it a different way ##

# Calculate the coordinate limits based on your data
coord_limits <- st_bbox(b.join$geometry)

# Extract coordinate limits
xlim <- c(coord_limits["xmin"], coord_limits["xmax"])
ylim <- c(coord_limits["ymin"], coord_limits["ymax"])

# Define a standard theme for all plots
standard_theme <- theme_void() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Adjust margins as needed
    legend.position = "right"
  )

# Create the plot with the legend
violence_plot <- ggplot() +
  geom_sf(data = b.join, aes(fill = fatalities, geometry = geometry)) +
  scale_fill_gradient(
    low = "#ffc4c4", high = "#ff3b3b",
    na.value = "white", limits = c(0, 500),
    guide = "colourbar"
  ) +
  labs(fill = "Fatalities") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Extract the legend using get_legend() from cowplot
violence_legend <- get_legend(violence_plot)

# Save the plot without the legend
ggsave(
  filename = "./results/drc/drc_violence.pdf",
  plot = violence_plot + theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Save the legend as a separate PDF
ggsave(
  filename = "./results/drc/drc_violence_legend.pdf",
  plot = as_ggplot(violence_legend),
  height = 2, width = 2, units = "in"
)

# Create the plot with the legend
pks_plot <- ggplot() +
  geom_sf(data = b.join, aes(fill = pks, geometry = geometry)) +
  scale_fill_gradient(
    low = "#2ABBE8", high = "#2A57E8",
    na.value = "white", limits = c(0, 525000),
    guide = "colourbar"
  ) +
  labs(fill = "Peacekeepers") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Extract the legend
pks_legend <- get_legend(pks_plot)

# Save the plot without the legend
ggsave(
  filename = "./results/drc/drc_pks.pdf",
  plot = pks_plot + theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Save the legend
ggsave(
  filename = "./results/drc/drc_pks_legend.pdf",
  plot = as_ggplot(pks_legend),
  height = 2, width = 2, units = "in"
)

# Create the plot with the legend
men_plot <- ggplot() +
  geom_sf(data = b.join, aes(fill = men, geometry = geometry)) +
  scale_fill_gradient(
    low = "#F99A6B", high = "#EB5307",
    na.value = "white", limits = c(0, 503524.3),
    guide = "colourbar"
  ) +
  labs(fill = "Men") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Extract the legend
men_legend <- get_legend(men_plot)

# Save the plot without the legend
ggsave(
  filename = "./results/drc/drc_men.pdf",
  plot = men_plot + theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Save the legend
ggsave(
  filename = "./results/drc/drc_men_legend.pdf",
  plot = as_ggplot(men_legend),
  height = 2, width = 2, units = "in"
)

# Create the plot with the legend
women_plot <- ggplot() +
  geom_sf(data = b.join, aes(fill = women, geometry = geometry)) +
  scale_fill_gradient(
    low = "#F96B8C", high = "#9E314B",
    na.value = "white", limits = c(0, 22010),
    guide = "colourbar"
  ) +
  labs(fill = "Women") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Extract the legend
women_legend <- get_legend(women_plot)

# Save the plot without the legend
ggsave(
  filename = "./results/drc/drc_women.pdf",
  plot = women_plot + theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Save the legend
ggsave(
  filename = "./results/drc/drc_women_legend.pdf",
  plot = as_ggplot(women_legend),
  height = 2, width = 2, units = "in"
)

# Plot of DRC Level 0 (drc_00.pdf)
ggsave(
  filename = "./results/drc/drc_00.pdf",
  plot = ggplot() +
    geom_sf(data = drc_00, aes(geometry = geometry), alpha = 0) +
    coord_sf(xlim = xlim, ylim = ylim) +
    standard_theme +
    theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Create the plot with the legend
pks_plot <- ggplot() +
  geom_sf(aes(fill = drc_01_pks$pks, geometry = drc_01_pks$geometry), alpha = 1) +
  scale_fill_gradient(
    low = "#b3e6ff", high = "#0040ff",
    space = "Lab", na.value = "white", limits = c(0, 926008),
    guide = "colourbar"
  ) +
  labs(fill = "Peacekeepers") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Save the plot without the legend
ggsave(
  filename = "./results/drc/drc_01_pks.pdf",
  plot = pks_plot + theme(legend.position = "none"),
  height = 8, width = 8, units = "in"
)

# Extract the legend from the plot
pks_legend <- get_legend(pks_plot)

# Save the legend as a separate PDF
ggsave(
  filename = "./results/drc/drc_01_pks_legend.pdf",
  plot = as_ggplot(pks_legend),
  height = 2, width = 2, units = "in"
)

# Create the map and adjust both text labels
map_tshopo <- ggplot() +
  # Fill the Tshopo region with dark grey
  geom_sf(data = drc_01, aes(geometry = geometry, fill = ifelse(NAME_1 == "Tshopo", "Tshopo", NA)), color = "black") +
  scale_fill_manual(values = c("Tshopo" = "darkgrey"), na.value = "white", guide = "none") +

  # Label the Tshopo region
  geom_sf_text(data = drc_01[drc_01$NAME_1 == "Tshopo", ],
               aes(geometry = geometry, label = NAME_1),
               size = 5, color = "black", fontface = "bold",
               nudge_y = 1.25, nudge_x = -0.5) +  # Adjust vertical position for "Tshopo"

  # Add the second label for the area with superscript using annotate()
  annotate("text", x = st_coordinates(st_centroid(st_geometry(drc_01[drc_01$NAME_1 == "Tshopo", ])))[1],
           y = st_coordinates(st_centroid(st_geometry(drc_01[drc_01$NAME_1 == "Tshopo", ])))[2] - 0.1,
           label = expression("199,567 km"^2),
           size = 4, color = "black", fontface = "plain") +
  coord_sf(xlim = xlim, ylim = ylim) +
  standard_theme

# Save the map as a PDF
ggsave(
  filename = "./results/drc/drc_tshopo.pdf",
  plot = map_tshopo,
  height = 8, width = 8, units = "in"
)


#################
# IV Map for JMP#
#################

# math for this grid #

b = b %>%
  filter(prio.grid==111297) %>%
  select(prio.grid, pko_africa, distance_to_capital, m_pko_africa, f_pko_africa, year, month)

b.join$highlight <- ifelse(b.join$prio.grid == 111297, "highlight", "other")

# Get the centroid of the highlighted grid
highlighted_grid <- b.join %>% filter(prio.grid == 111297)
highlight_centroid <- st_centroid(highlighted_grid$geometry)

# Coordinates of the red star in decimal format
star_coords <- data.frame(lon = 15.312, lat = -4.322)

# Midpoint coordinates for the label (roughly halfway along the arrow)
mid_lon <- (st_coordinates(highlight_centroid)[1] + star_coords$lon) / 2
mid_lat <- (st_coordinates(highlight_centroid)[2] + star_coords$lat) / 2


# Create a plot for the specific grid
pdf("./results/drc/111297.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(fill = b.join$highlight, geometry = b.join$geometry), alpha = 0.4) +  # Set alpha to make grids lighter
  scale_fill_manual(values = c("highlight" = "#E69F00", "other" = "grey89"), na.value = "grey89") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

# Create a plot for the specific grid
pdf("./results/drc/111297_capital.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(fill = b.join$highlight, geometry = b.join$geometry), alpha = 0.4) +  # Set alpha to make grids lighter
  geom_point(data = star_coords, aes(x = lon, y = lat), color = "red", shape = 8, size = 3) +  # Add red star
  scale_fill_manual(values = c("highlight" = "#E69F00", "other" = "grey89"), na.value = "grey89") +
  theme_void() +
  theme(legend.position = "none")
dev.off()

# Create a plot for the specific grid
pdf("./results/drc/111297_arrow_distance.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(fill = b.join$highlight, geometry = b.join$geometry), alpha = 0.4) +  # Set alpha to make grids lighter
  geom_point(data = star_coords, aes(x = lon, y = lat), color = "red", shape = 8, size = 3) +  # Add red star
  geom_segment(aes(x = star_coords$lon, y = star_coords$lat,
                   xend = st_coordinates(highlight_centroid)[1], yend = st_coordinates(highlight_centroid)[2]),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +  # Reverse arrow direction and make it thinner
  geom_text(aes(x = mid_lon, y = mid_lat, label = "1705 km"), vjust = 4, hjust = 1, size = 4, color = "black") +  # Add label below the arrow
  scale_fill_manual(values = c("highlight" = "#E69F00", "other" = "grey89"), na.value = "grey89") +
  theme_void() +
  theme(legend.position = "none")
dev.off()


# Define a common theme with identical margins for all plots
common_theme <- theme_void() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "cm"))  # Adjust the values as needed

# Function to extract the legend
extract_legend <- function(plot) {
  gg_legend <- as_ggplot(get_legend(plot))
  return(gg_legend)
}

pdf("./results/drc/drc_00.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(geometry = drc_00$geometry), alpha = 0) +
  common_theme
dev.off()

# plot of 01 DRC
pdf("./results/drc/drc_01.pdf", height = 8, width = 8)
ggplot() +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) +
  common_theme
dev.off()

# plot of grids over DRC
pdf("./results/drc/drc_01grids.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(geometry = b.join$geometry), alpha = 0) +
  geom_sf(aes(geometry = drc_01$geometry), alpha = 0) +
  common_theme
dev.off()

pdf("./results/drc/drc_grids.pdf", height = 8, width = 8)
ggplot() + geom_sf(aes(geometry = b.join$geometry), alpha = 0) +
  common_theme
dev.off()

# Plot of violence in DRC with updated color range
plot_violence <- ggplot() +
  geom_sf(aes(fill = b.join$fatalities, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#ffe5e5", high = "#ff0000", space = "Lab", na.value = "white",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,500)) +
  labs(fill = "Fatalities") +
  common_theme

# Save violence plot without legend
pdf("./results/drc/drc_violence_no_legend.pdf", height = 8, width = 8)
plot_violence + theme(legend.position = "none")
dev.off()

# Save only the legend
pdf("./results/drc/drc_violence_legend.pdf", height = 8, width = 8)
gg_legend <- extract_legend(plot_violence)
print(gg_legend)
dev.off()

# Plot of peacekeepers (PKs) with updated color range
plot_pks <- ggplot() +
  geom_sf(aes(fill = b.join$pks, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#b3e6ff", high = "#0040ff", space = "Lab", na.value = "white",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,525000)) +
  labs(fill = "Peacekeepers") +
  common_theme

# Save PKs plot without legend
pdf("./results/drc/drc_pks_no_legend.pdf", height = 8, width = 8)
plot_pks + theme(legend.position = "none")
dev.off()

# Save only the legend
pdf("./results/drc/drc_pks_legend.pdf", height = 8, width = 8)
gg_legend <- extract_legend(plot_pks)
print(gg_legend)
dev.off()

# Plot of men peacekeepers with updated color range
plot_men <- ggplot() +
  geom_sf(aes(fill = b.join$men, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#ffd9b3", high = "#ff6600", space = "Lab", na.value = "white",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,503524.3)) +
  labs(fill = "Men") +
  common_theme

# Save men PKs plot without legend
pdf("./results/drc/drc_men_no_legend.pdf", height = 8, width = 8)
plot_men + theme(legend.position = "none")
dev.off()

# Save only the legend
pdf("./results/drc/drc_men_legend.pdf", height = 8, width = 8)
gg_legend <- extract_legend(plot_men)
print(gg_legend)
dev.off()

# Plot of women peacekeepers with updated color range
plot_women <- ggplot() +
  geom_sf(aes(fill = b.join$women, geometry = b.join$geometry)) +
  scale_fill_gradient(low = "#ffb3cc", high = "#ff0066", space = "Lab", na.value = "white",
                      guide = "colourbar", aesthetics = "fill", limits=c(0,22010)) +
  labs(fill = "Women") +
  common_theme

# Save women PKs plot without legend
pdf("./results/drc/drc_women_no_legend.pdf", height = 8, width = 8)
plot_women + theme(legend.position = "none")
dev.off()

# Save only the legend
pdf("./results/drc/drc_women_legend.pdf", height = 8, width = 8)
gg_legend <- extract_legend(plot_women)
print(gg_legend)
dev.off()

####################################################


# ==================== WPS timeline ====================
# events + full-year data frames (sat just above the extracted range in plot_data.R)
events_data <- data.frame(
  event = c("UNTAC Established\n(Feb. 1992)",
            "Boys will be boys\n(Early 1994)",
            "UNSCR 1325\n(Oct. 2000)",
            "UNISFA Established\n(Jun. 2011)",
            "India Deploys Largest\nEver Contingent\nof Women Peacekeepers \n(Jan. 2023)",
            "Captain Cecilia Erzuah chosen\nas 2022 UN Military Gender\nAdvocate of the Year\n(May 2023)"),
  date = as.Date(c("1992-02-01", "1994-01-01", "2000-10-31",
                   "2011-06-27", "2023-01-06", "2023-05-25")),
  position = c(0.01, -0.01, 0.01, -0.01, 0.01, -0.01)
)
timeline_data <- data.frame(year = seq(1990, 2025, by = 1))

# Add 1990 and 2025 to the tick marks data
ticks_data <- data.frame(
  year = seq(1990, 2025, by = 5)
)

# Plot the timeline with events
pdf("./results/timeline.pdf", width = 10, height = 6)
ggplot(timeline_data, aes(x = year, y = 0)) +
  # Add the main timeline from 1990 to 2025
  geom_segment(aes(x = 1990, xend = 2025, y = 0, yend = 0), linewidth = 1, color = "black") +

  # Add tick marks every 5 years, including 1990 and 2025
  geom_segment(data = ticks_data, aes(x = year, xend = year, y = -0.001, yend = 0.001),
               color = "black", linewidth = 0.8) +

  # Add year labels directly below the line, including 1990 and 2025
  geom_text(data = ticks_data, aes(x = year, y = -0.0005, label = year), size = 3, vjust = 1.5) +

  # Dashed lines from boxes to the year line
  geom_segment(data = events_data, aes(x = as.numeric(format(date, "%Y")),
                                       xend = as.numeric(format(date, "%Y")),
                                       y = 0, yend = position), linetype = "dashed", color = "gray") +

  # Event points
  geom_point(data = events_data, aes(x = as.numeric(format(date, "%Y")), y = 0), color = "red", size = 3) +

  # Boxed labels
  geom_label(data = events_data, aes(x = as.numeric(format(date, "%Y")), y = position, label = event),
             fill = "#5b92e5", color = "white", size = 3, vjust = ifelse(events_data$position > 0, 0, 1),
             label.size = 0) + # remove label border

  # Customizing the x-axis and plot appearance
  scale_x_continuous(breaks = seq(1990, 2025, by = 5), limits = c(1990, 2025)) +
  coord_cartesian(ylim = c(-0.05, 0.05)) +  # Increase vertical margins
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 5, 5, 5))  # Add margin around the plot
dev.off()

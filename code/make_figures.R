#' =============================================================================
#' Project: ECHO Aim1
#' Date created: February 19, 2019
#' Date updated: August 6, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' This script makes the figures for the manuscript
#' =============================================================================

library(sf)
library(raster)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(ggsn)
library(ggthemes)
library(GGally)
library(ggcorrplot)
library(ggpubr)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)
library(viridis)
library(caret)
library(reshape2)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 14, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=12),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank(),
  plot.title = element_text(size = 12, face = "bold")
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 16, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border = element_blank(),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

register_google(key = google_api_key)

lur_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Subprojects/ECHO_Aim1_LUR/Data/"

#' Viridis colors for the maps (5 levels - the highest is set to avoid yellow)
colors <- c("#440154FF", "#31688EFF", "#35B779FF", "#B8DE29FF")

#' Filter data
lur_data <- read_csv(here::here("Data", "Analysis_Data_Set.csv")) %>% 
  mutate(smoke_fac = factor(area_smoke_2sd,
                            labels = c("No Smoke", "Smoke-Impacted"))) %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), "Winter", 
                         ifelse(month %in% c(3, 4, 5), "Spring", 
                                ifelse(month %in% c(6, 7, 8), "Summer",
                                       ifelse(month %in% c(9, 10, 11), "Fall", NA))))) %>% 
  mutate(bc_ug_m3 = bc_ug_m3_dem) %>% 
  filter(campaign %in% c(paste0("Campaign", c(1, 2, 3))))

#' -----------------------------------------------------------------------------
#' Make a plot of the sampling locations
#' -----------------------------------------------------------------------------

#' Read in the geocoded locations
locations_file_name <- "Filter_Locations_AEA.csv"

locations_aea <- read_csv(paste0(lur_path, "Filter_Data/", locations_file_name)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  st_transform(crs = ll_wgs84) %>% 
  filter(campaign %in% paste0("Campaign", 1:3))
head(locations_aea)

names(locations_aea)
sum(duplicated(locations_aea$filter_id))

locations1 <- select(locations_aea, Location, participant, lon, lat) %>% 
  st_set_geometry(NULL) %>% 
  distinct(lon, lat, participant) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = ll_wgs84)

#' Map of campaign sampling locations 
#' NOTE: residences are jittered to protect privacy but not distinguished in the
#' legend
#' Map in ggmap
base_map <- get_map(location = "Commerce City, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

ggmap(base_map) +
  #ggplot() +
  geom_sf(data = filter(locations1, participant == 0),
          aes(fill = "com", color = "com", shape = "com"),
          inherit.aes = F, show.legend = "point", size = 3) +
  geom_sf(data = st_jitter(filter(locations1, participant == 1), 0.01), 
          aes(fill = "com", color = "com", shape = "com"),
          inherit.aes = F, show.legend = "point", size = 3) +
  scale_color_manual(name = NA, guide= "legend",
                     values = c("com" = "#3B528BFF"),
                     labels = c("com" = "Sampling Locations")) +
  scale_fill_manual(name = NA, guide= "legend",
                    values = c("com" = "#3B528BFF"),
                    labels = c("com" = "Sampling Locations")) +
  scale_shape_manual(name = NA, guide= "legend",
                     values = c("com" = 19),
                     labels = c("com" = "Sampling Locations")) +
  north(x.min = -105.3726, x.max = -104.4937,
        y.min =  39.5, y.max = 40.14455,
        symbol = 12, location = "bottomright", scale = 0.075) +
  ggsn::scalebar(x.min = -105.3726, x.max = -104.4937,
                 y.min =  39.5, y.max = 40.14455, transform = T, dist_unit = "km",
                 dist = 10, model="WGS84", st.bottom = F, st.size = 4,
                 height = 0.01) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.75, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(family="Calibri",size = 16, color = 'black')) +
  guides(shape = guide_legend(override.aes = list(color = "#3B528BFF",
                                                  shape = 19))) +
  map_theme

fig_name <- "LUR_Sampling_Sites.jpeg"
ggsave(filename = here::here("Figs", fig_name),
       device = "jpeg", dpi=500, units = "in", height = 5, width = 5)

#' -----------------------------------------------------------------------------
#' Make a plot of the monitoring sites (PM, BC, and NO2)
#' -----------------------------------------------------------------------------

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

data_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Subprojects/ECHO_Aim1_LUR/Data/"

#' PM2.5 concentrations
pm_data <- read_csv(paste0(data_path, "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  filter(County_Code %in% counties) %>% 
  filter(Sample_Duration != "1 HOUR") %>% 
  select(monitor_id) %>% 
  distinct(monitor_id)
pm_data <- st_transform(pm_data, crs = ll_wgs84)

#' Black carbon
bc_data <- read_csv(paste0(data_path, "Monitor_BC_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  st_as_sf(wkt = "WKT", crs = albers) %>%
  filter(County_Code %in% counties) %>%
  select(monitor_id) %>%
  distinct(monitor_id)
bc_data <- st_transform(bc_data, crs = ll_wgs84)

#' NO2
no2_data <- read_csv(paste0(data_path, "Monitor_NO2_Data_AEA.csv")) %>%
  filter(!is.na(Arithmetic_Mean)) %>%
  st_as_sf(wkt = "WKT", crs = albers)%>%
  filter(County_Code %in% counties) %>%
  select(monitor_id) %>%
  distinct(monitor_id)
no2_data <- st_transform(no2_data, crs = ll_wgs84)

#' Base map of the area
base_map <- get_map(location = "Denver, CO", zoom = 10)
ggmap(base_map)
attr(base_map, "bb")

ggmap(base_map) +
  #ggplot() +
  geom_sf(data = pm_data,
          aes(fill = "pm", color = "pm", shape = "pm"),
          inherit.aes = F, show.legend = "point", size = 4) +
  geom_sf(data = no2_data,
          aes(fill = "no2", color = "no2", shape = "no2"),
          inherit.aes = F, show.legend = "point", size = 4) +
  geom_sf(data = bc_data,
          aes(fill = "bc", color = "bc", shape = "bc"),
          inherit.aes = F, show.legend = "point", size = 4) +
  scale_color_manual(name = "Pollutant", #guide= "legend",
                     values = c("pm" = "#440154FF",
                                "bc" = "#FDE725FF",
                                "no2" = "#20A387FF"),
                     labels = c("pm" = "PM\u2082.\u2085",
                                "bc" = "Black carbon",
                                "no2" = "NO\u2082")) +
  scale_fill_manual(name = "Pollutant", #guide= "legend",
                    values = c("pm" = "#440154FF",
                               "bc" = "#FDE725FF",
                               "no2" = "#20A387FF"),
                    labels = c("pm" = "PM\u2082.\u2085",
                               "bc" = "Black carbon",
                               "no2" = "NO\u2082")) +
  scale_shape_manual(name = "Pollutant", #guide= "legend",
                     values = c("pm" = 15,
                                "bc" = 16,
                                "no2" = 17),
                     labels = c("pm" = "PM\u2082.\u2085",
                                "bc" = "Black carbon",
                                "no2" = "NO\u2082")) +
  annotation_scale(data = pm_data, #plot_unit = "m", 
                   location = "br", width_hint = 0.5,
                   pad_x = unit(1, "cm"), pad_y = unit(1.1, "cm"),
                   text_cex = 1.5,
                   line_col = "black", text_col = "black", text_face = "bold") +
  annotation_north_arrow(data = pm_data, 
                         location = "br", which_north = "grid", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_minimal(line_col = "black", 
                                                     fill = "black", 
                                                     line_width = 1.6,
                                                     text_col = "black",
                                                     text_face = "bold",
                                                     text_size = 12)) +
  xlab("") + ylab("") +
  theme(legend.position = c(0.75, 0.85),
        legend.text = element_text(family="Calibri",size = 16, color = 'black')) +
  # guides(shape = guide_legend(override.aes = list(color = "#3B528BFF",
  #                                                 shape = 19))) +
  map_theme

fig_name <- "Monitoring_Locations.jpeg"
ggsave(filename = here::here("Figs", fig_name),
       device = "jpeg", dpi=500, units = "in", height = 6, width = 6)

#' -----------------------------------------------------------------------------
#' Time series of BC 
#' -----------------------------------------------------------------------------

lur_data$plot_date <- as.Date(lur_data$EndDateLocal, "%m-%Y", format = "%m-%Y")

ggplot(data = lur_data) +
  geom_rect(aes(xmin = as.Date("06/27/2018", format = "%m/%d/%Y"), 
                xmax = as.Date("09/10/2018", format = "%m/%d/%Y"), 
                ymin=-Inf, ymax=Inf, fill = "fire"), color = "gray90") +
  geom_point(aes(x = plot_date, y = bc_ug_m3,
                 color = as.factor(campaign), 
                 shape = as.factor(area_smoke_2sd)),
             size = 2) +
  scale_x_date(date_breaks = "1 month") +
  scale_shape_manual(name = "WFS impacted?",
                     values = c(1, 19),
                     labels = c("0" = "No", "1" = "Yes")) +
  # scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  xlab("") + ylab("BC (\u03BCg/m\u00B3)") +
  scale_color_viridis(name = "Campaign", discrete = T,
                      labels = c("Campaign1" = "Campaign 1",
                                 "Campaign2" = "Campaign 2",
                                 "Campaign3" = "Campaign 3")) +
  scale_fill_manual(name = "Fire", values = c("fire" = "gray90"), 
                    labels = c("fire" = "Spring Creek")) +      
  theme(axis.text.x = element_text(family="Calibri", size = 14, color = 'black',
                                   angle = 45, hjust = 1, face = "bold")) +
  simple_theme
ggsave(filename = here::here("Figs", "BC_Smoke_Filters_Timeline.jpeg"),
       device = "jpeg", dpi=500, units = "in", height = 4, width = 7)


#' -----------------------------------------------------------------------------
#' CDF plots
#' -----------------------------------------------------------------------------

ggplot() +
  stat_ecdf(data = filter(lur_data, area_smoke_2sd == 1),
            aes(x = bc_ug_m3, color = as.factor(area_smoke_2sd)), 
            geom = "line") +
  stat_ecdf(data = filter(lur_data, area_smoke_2sd == 0),
            aes(x = bc_ug_m3, color = as.factor(area_smoke_2sd)), 
            geom = "line") +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  scale_color_viridis(name = "WFS impacted?", discrete = T,
                      labels = c("1" = "Yes", "0" = "No")) +
  theme(legend.position = c(0.8, 0.2)) +
  ylab("Cumulative probability") + xlab("BC (\u03BCg/m\u00B3)") +
  simple_theme

ggsave(here::here("Figs", "ECDF_Plots_BC_by_Smoke.jpeg"),
       device = "jpeg", height = 4, width = 4, units = "in", dpi = 600)


#' -----------------------------------------------------------------------------
#' Continuous BC vs. continuous GIS measures
#' -----------------------------------------------------------------------------

ggplot(data = arrange(lur_data, dist_to_highway_m)) +
  geom_point(aes(x = dist_to_highway_m, y = bc_ug_m3, 
                 color = as.factor(area_smoke_2sd))) +
  geom_smooth(aes(x = dist_to_highway_m, y = bc_ug_m3,
                  color = as.factor(area_smoke_2sd)), 
              method = "loess") +
  scale_color_viridis(name = "WFS impacted?", discrete = T,
                      labels = c("1" = "Yes", "0" = "No")) +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, round(max(lur_data$dist_to_highway_m), 0))) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab("BC (\u03BCg/m\u00B3)") + xlab("Distance to closest highway (m)") +
  simple_theme
ggsave(here::here("Figs", "BC_vs_Dist_to_Highway_By_Smoke.jpeg"),
       device = "jpeg", height = 5, width = 7, units = "in", dpi = 600)

ggplot(data = arrange(lur_data, length_m_roads)) +
  geom_point(aes(x = length_m_roads, y = bc_ug_m3, 
                 color = as.factor(area_smoke_2sd))) +
  geom_smooth(aes(x = length_m_roads, y = bc_ug_m3,
                  color = as.factor(area_smoke_2sd)), 
              method = "loess") +
  scale_color_viridis(name = "WFS impacted?", discrete = T,
                      labels = c("1" = "Yes", "0" = "No")) +
  scale_x_reverse(breaks = c(0, 500, 1000, 1500, 2000, round(max(lur_data$length_m_roads), 0))) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab("BC (\u03BCg/m\u00B3)") + xlab("Length of major roads (m) within 300 m") +
  simple_theme
ggsave(here::here("Figs", "BC_vs_Length_Roads_By_Smoke.jpeg"),
       device = "jpeg", height = 5, width = 7, units = "in", dpi = 600)

ggplot(data = arrange(lur_data, aadt_buffer)) +
  geom_point(aes(x = aadt_buffer, y = bc_ug_m3, 
                 color = as.factor(area_smoke_2sd))) +
  geom_smooth(aes(x = aadt_buffer, y = bc_ug_m3,
                  color = as.factor(area_smoke_2sd)), 
              method = "loess") +
  scale_color_viridis(name = "WFS impacted?", discrete = T,
                      labels = c("1" = "Yes", "0" = "No")) +
  scale_x_reverse(breaks = c(0, 25000, 50000, 75000, 100000, round(max(lur_data$aadt_buffer), 0))) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab("BC (\u03BCg/m\u00B3)") + xlab("Average AADT (vehicles per day) within 300 m") +
  simple_theme
ggsave(here::here("Figs", "BC_vs_AADT_By_Smoke.jpeg"),
       device = "jpeg", height = 5, width = 7, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' Boxplots of BC by season
#' -----------------------------------------------------------------------------

season_data <- select(lur_data, season, bc_ug_m3, s_k_ratio) %>% 
  gather("metric", "value", -season) %>% 
  mutate(metric_fac = factor(metric, labels = c("BC (\u03BCg/m\u00B3)", "S:K Ratio")))

season_plot <- ggplot(data = season_data) +
  geom_boxplot(aes(x = as.factor(season), y = value, col=as.factor(season)),
               show.legend = F) +
  scale_color_manual(name = "Season", values = colors) +
  facet_wrap(~ metric_fac, ncol = 2, scales = "free_y") +
  scale_x_discrete(limits = c("Spring", "Summer", "Fall")) +
  xlab("") + ylab("") +
  simple_theme
season_plot

ggsave(here::here("Figs", "Seasonal_BC_Boxplots.jpeg"),
       device = "jpeg", height = 4, width = 6, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' Boxplots of BC by length_cat and smoke
#' -----------------------------------------------------------------------------

quantile(lur_data$length_m_roads, c(0.25, 0.50, 0.75))

bc_plot_length_smoke <- ggplot(data = lur_data) +
  geom_boxplot(aes(x = as.factor(area_smoke_2sd), y = bc_ug_m3, 
                   col=as.factor(length_cat))) +
  scale_x_discrete(labels = c("No smoke", "Smoke-impacted")) +
  scale_color_manual(name = "Road Length\nQuartile",
                     values = colors,
                     labels = c("0 m", ">0 to \u2264590 m", 
                                ">590 to \u2264984 m", ">984 m")) +
  xlab("") + ylab("BC (\u03BCg/m\u00B3)") +
  simple_theme 
bc_plot_length_smoke

ggsave(here::here("Figs", "BC_Smoke_Length_Boxplots.jpeg"),
       device = "jpeg", height = 4, width = 6, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' Boxplots of BC by dist_cat and smoke
#' -----------------------------------------------------------------------------

quantile(lur_data$dist_to_highway_m, c(0.25, 0.50, 0.75))

bc_plot_distance_smoke <- ggplot(data = lur_data) +
  geom_boxplot(aes(x = as.factor(area_smoke_2sd), y = bc_ug_m3, 
                   col=as.factor(distance_cat))) +
  scale_x_discrete(labels = c("No smoke", "Smoke-impacted")) +
  scale_color_manual(name = "Distance to highway\nQuartile",
                     values = colors,
                     labels = c("> 1530 m", "> 754 m to \u2264 1530 m",
                                "> 299 to \u2264 754 m", "\u2264 299 m")) +
  xlab("") + ylab("BC (\u03BCg/m\u00B3)") +
  simple_theme 
bc_plot_distance_smoke

ggsave(here::here("Figs", "BC_Smoke_Distance_Boxplots.jpeg"),
       device = "jpeg", height = 4, width = 6, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' Boxplots of BC by aadt_cat and smoke
#' -----------------------------------------------------------------------------

quantile(lur_data$aadt_buffer, c(0.25, 0.50, 0.75))

bc_plot_aadt_smoke <- ggplot(data = lur_data) +
  geom_boxplot(aes(x = as.factor(area_smoke_2sd), y = bc_ug_m3, 
                   col=as.factor(aadt_cat))) +
  scale_x_discrete(labels = c("No smoke", "Smoke-impacted")) +
  scale_color_manual(name = "AADT\nQuartile",
                     values = colors,
                     labels = c("0 veh/d", ">0 to \u226411,937 veh/d", 
                                ">11,937 to \u226433,000 veh/d", ">33,000 veh/d")) +
  xlab("") + ylab("BC (\u03BCg/m\u00B3)") +
  simple_theme 
bc_plot_aadt_smoke

#' -----------------------------------------------------------------------------
#' Faceted boxplots of BC by smoke and the GIS indicators
#' -----------------------------------------------------------------------------

lur_data_bc_long <- lur_data %>% 
  dplyr::select(area_smoke_2sd, bc_ug_m3, distance_cat, length_cat, aadt_cat) %>% 
  gather(key = "gis_met", value = "quart", -c(area_smoke_2sd, bc_ug_m3))

met_names <- c(
  "aadt_cat" = "AADT (veh/d)\nin 300 m buffer",
  "distance_cat" = "Distance to\nroadways (m)",
  "length_cat" = "Length of roadways (m)\nin 300 m buffer"
)

bc_plot_gis_smoke <- ggplot(data = lur_data_bc_long) +
  geom_boxplot(aes(x = as.factor(area_smoke_2sd), y = bc_ug_m3, 
                   col=as.factor(quart))) +
  scale_x_discrete(labels = c("No smoke", "WFS-impacted")) +
  scale_color_manual(name = "GIS Metric Quartile",
                     values = colors,
                     labels = c("Q1 (lowest TRAP exposure)",
                                "Q2", "Q3",
                                "Q4 (highest TRAP exposure)")) +
  facet_wrap(gis_met ~ ., ncol = 3, labeller = as_labeller(met_names)) +
  xlab("") + ylab("BC (\u03BCg/m\u00B3)") +
  theme(legend.position = "bottom") +
  simple_theme 
bc_plot_gis_smoke

ggsave(here::here("Figs", "BC_Smoke_GIS_Boxplots.jpeg"),
       device = "jpeg", height = 5, width = 8, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' Agreement between bc_cat and GIS-based indicator quartiles
#' All filters, without WFS, with WFS
#' -----------------------------------------------------------------------------

lur_data_smoke <- filter(lur_data, area_smoke_2sd == 1)
lur_data_no_smoke <- filter(lur_data, area_smoke_2sd == 0)

library(reshape2)

n_all <- nrow(lur_data)
n_smoke <- nrow(lur_data_smoke)
n_no_smoke <- nrow(lur_data_no_smoke) 

#' Distance
bc_mat_dist <- as.matrix(table(lur_data$bc_cat, lur_data$distance_cat,
                               dnn = c("bc_cat", "dist_cat")))
colnames(bc_mat_dist) <- c("dist_cat_1", "dist_cat_2", "dist_cat_3", "dist_cat_4")
rownames(bc_mat_dist) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")

bc_mat_long_dist <- melt(bc_mat_dist)

bc_cat_plot_dist <- ggplot(bc_mat_long_dist, aes(x = bc_cat, y = dist_cat)) + 
  ggtitle("All filters") +
  geom_tile(aes(fill=(value/n_all)*100)) + 
  geom_label(aes(label = round((value/n_all)*100, 1), fill = round((value/n_all)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("Distance") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_dist

#' Length
bc_mat_length <- as.matrix(table(lur_data$bc_cat, lur_data$length_cat,
                                 dnn = c("bc_cat", "length_cat")))
colnames(bc_mat_length) <- c("length_cat_1", "length_cat_2", "length_cat_3", "length_cat_4")
rownames(bc_mat_length) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")

bc_mat_long_length <- melt(bc_mat_length)

bc_cat_plot_length <- ggplot(bc_mat_long_length, aes(x = bc_cat, y = length_cat)) + 
  geom_tile(aes(fill=(value/n_all)*100)) + 
  geom_label(aes(label = round((value/n_all)*100, 1), fill = round((value/n_all)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("Length") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_length

#' AADT
bc_mat_aadt <- as.matrix(table(lur_data$bc_cat, lur_data$aadt_cat,
                               dnn = c("bc_cat", "aadt_cat")))
colnames(bc_mat_aadt) <- c("aadt_cat_1", "aadt_cat_2", "aadt_cat_3", "aadt_cat_4")
rownames(bc_mat_aadt) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")

bc_mat_long_aadt <- melt(bc_mat_aadt)

bc_cat_plot_aadt <- ggplot(bc_mat_long_aadt, aes(x = bc_cat, y = aadt_cat)) + 
  geom_tile(aes(fill=(value/n_all)*100)) + 
  geom_label(aes(label = round((value/n_all)*100, 1), fill = round((value/n_all)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("AADT") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_aadt

#' Distance and smoke
bc_mat_smoke_distance <- as.matrix(table(lur_data_smoke$bc_cat, lur_data_smoke$distance_cat,
                                         dnn = c("bc_cat", "distance_cat")))
colnames(bc_mat_smoke_distance) <- c("distance_cat_1", "distance_cat_2", "distance_cat_3", "distance_cat_4")
rownames(bc_mat_smoke_distance) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_smoke_long_distance <- melt(bc_mat_smoke_distance)

bc_mat_no_smoke_distance <- as.matrix(table(lur_data_no_smoke$bc_cat, lur_data_no_smoke$distance_cat,
                                            dnn = c("bc_cat", "distance_cat")))
colnames(bc_mat_no_smoke_distance) <- c("distance_cat_1", "distance_cat_2", "distance_cat_3", "distance_cat_4")
rownames(bc_mat_no_smoke_distance) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_no_smoke_long_distance <- melt(bc_mat_no_smoke_distance)

bc_cat_plot_smoke_dist <- ggplot(bc_mat_smoke_long_distance, 
                                 aes(x = bc_cat, y = distance_cat)) + 
  ggtitle("WFS-impacted filters") +
  geom_tile(aes(fill=(value/n_smoke)*100)) + 
  geom_label(aes(label = round((value/n_smoke)*100, 1), fill = round((value/n_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_smoke_dist

bc_cat_plot_nosmoke_dist <- ggplot(bc_mat_no_smoke_long_distance, 
                                   aes(x = bc_cat, y = distance_cat)) + 
  ggtitle("Non-impacted filters") +
  geom_tile(aes(fill=(value/n_no_smoke)*100)) + 
  geom_label(aes(label = round((value/n_no_smoke)*100, 1), fill = round((value/n_no_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_nosmoke_dist

#' Length and smoke
bc_mat_smoke_length <- as.matrix(table(lur_data_smoke$bc_cat, lur_data_smoke$length_cat,
                                       dnn = c("bc_cat", "length_cat")))
colnames(bc_mat_smoke_length) <- c("length_cat_1", "length_cat_2", "length_cat_3", "length_cat_4")
rownames(bc_mat_smoke_length) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_smoke_long_length <- melt(bc_mat_smoke_length)

bc_mat_no_smoke_length <- as.matrix(table(lur_data_no_smoke$bc_cat, lur_data_no_smoke$length_cat,
                                          dnn = c("bc_cat", "length_cat")))
colnames(bc_mat_no_smoke_length) <- c("length_cat_1", "length_cat_2", "length_cat_3", "length_cat_4")
rownames(bc_mat_no_smoke_length) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_no_smoke_long_length <- melt(bc_mat_no_smoke_length)

bc_cat_plot_smoke_length <- ggplot(bc_mat_smoke_long_length, 
                                   aes(x = bc_cat, y = length_cat)) + 
  geom_tile(aes(fill=(value/n_smoke)*100)) + 
  geom_label(aes(label = round((value/n_smoke)*100, 1), fill = round((value/n_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_smoke_length

bc_cat_plot_nosmoke_length <- ggplot(bc_mat_no_smoke_long_length, 
                                     aes(x = bc_cat, y = length_cat)) + 
  geom_tile(aes(fill=(value/n_no_smoke)*100)) + 
  geom_label(aes(label = round((value/n_no_smoke)*100, 1), fill = round((value/n_no_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_nosmoke_length

#' AADT and smoke
bc_mat_smoke_aadt <- as.matrix(table(lur_data_smoke$bc_cat, lur_data_smoke$aadt_cat,
                                     dnn = c("bc_cat", "aadt_cat")))
colnames(bc_mat_smoke_aadt) <- c("aadt_cat_1", "aadt_cat_2", "aadt_cat_3", "aadt_cat_4")
rownames(bc_mat_smoke_aadt) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_smoke_long_aadt <- melt(bc_mat_smoke_aadt)

bc_mat_no_smoke_aadt <- as.matrix(table(lur_data_no_smoke$bc_cat, lur_data_no_smoke$aadt_cat,
                                        dnn = c("bc_cat", "aadt_cat")))
colnames(bc_mat_no_smoke_aadt) <- c("aadt_cat_1", "aadt_cat_2", "aadt_cat_3", "aadt_cat_4")
rownames(bc_mat_no_smoke_aadt) <- c("bc_cat_1", "bc_cat_2", "bc_cat_3", "bc_cat_4")
bc_mat_no_smoke_long_aadt <- melt(bc_mat_no_smoke_aadt)

bc_cat_plot_smoke_aadt <- ggplot(bc_mat_smoke_long_aadt, 
                                 aes(x = bc_cat, y = aadt_cat)) + 
  geom_tile(aes(fill=(value/n_smoke)*100)) + 
  geom_label(aes(label = round((value/n_smoke)*100, 1), fill = round((value/n_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("Black Carbon") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_smoke_aadt 

bc_cat_plot_nosmoke_aadt  <- ggplot(bc_mat_no_smoke_long_aadt, 
                                    aes(x = bc_cat, y = aadt_cat)) + 
  geom_tile(aes(fill=(value/n_no_smoke)*100)) + 
  geom_label(aes(label = round((value/n_no_smoke)*100, 1), fill = round((value/n_no_smoke)*100, 1)), 
             label.size = 0, label.r = unit(0, "lines")) +
  scale_fill_viridis(name = "% of filters") +
  ylab("") + xlab("") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  scale_y_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  simple_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 1))
bc_cat_plot_nosmoke_aadt

cat_plot <- ggarrange(
  bc_cat_plot_dist, bc_cat_plot_smoke_dist, bc_cat_plot_nosmoke_dist,
  bc_cat_plot_length, bc_cat_plot_smoke_length, bc_cat_plot_nosmoke_length,
  bc_cat_plot_aadt, bc_cat_plot_smoke_aadt, bc_cat_plot_nosmoke_aadt,
  ncol = 3, nrow = 3, common.legend = T, legend = "bottom")
cat_plot
ggsave(here::here("Figs", "BC_Agreement_By_Smoke.jpeg"),
       device = "jpeg", height = 6.5, width = 6, units = "in", dpi = 600)

#' -----------------------------------------------------------------------------
#' ER distributions from simulations
#' -----------------------------------------------------------------------------

beta_df <- read_csv(here::here("Results", "Beta_Simulation_Study_Linear.csv"))

beta_0_plot <- ggplot(filter(beta_df, true_beta == 0)) +
  geom_density(aes(x = wfs0_beta, color = "wfs0", fill = "wfs0", alpha = "wfs0"),
               size = 1) +
  geom_density(aes(x = wfs1_beta, color = "wfs1", fill = "wfs1", alpha = "wfs1"),
               size = 1) +
  geom_vline(aes(xintercept = 0), color = "black", linetype = 2) +
  scale_color_viridis(name = "WFS-impacted?", discrete = T,
                      labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_fill_viridis(name = "WFS-impacted?", discrete = T,
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_alpha_manual(name = "WFS-impacted?",
                     values = c("wfs1" = 0.25, "wfs0" = 0.25),
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  # xlab("BC \u03b2 value (True \u03b2 = 0)") + 
  # ylab("Density") +
  xlab(NULL) + ylab(NULL) +
  simple_theme +
  theme(legend.position = c(0.15, 0.8),
        plot.margin = unit(c(5,5,5,5), "mm"))
beta_0_plot
ggsave(filename = here::here("Figs", "ER_Sim_Beta_0.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 5, width = 5)

beta_5_plot <- ggplot(filter(beta_df, true_beta == -5)) +
  geom_density(aes(x = wfs0_beta, color = "wfs0", fill = "wfs0", alpha = "wfs0"),
               size = 1) +
  geom_density(aes(x = wfs1_beta, color = "wfs1", fill = "wfs1", alpha = "wfs1"),
               size = 1) +
  geom_vline(aes(xintercept = -5), color = "black", linetype = 2) +
  scale_color_viridis(name = "WFS-impacted?", discrete = T,
                      labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_fill_viridis(name = "WFS-impacted?", discrete = T,
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_alpha_manual(name = "WFS-impacted?",
                     values = c("wfs1" = 0.25, "wfs0" = 0.25),
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  # xlab("BC \u03b2 value (True \u03b2 = 5)") + 
  # ylab("Density") +
  xlab(NULL) + ylab(NULL) +
  simple_theme +
  theme(legend.position = c(0.15, 0.8),
        plot.margin = unit(c(5,5,5,5), "mm"))
beta_5_plot
ggsave(filename = here::here("Figs", "ER_Sim_Beta_5.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 5, width = 5)

beta_10_plot <- ggplot(filter(beta_df, true_beta == -10)) +
  geom_density(aes(x = wfs0_beta, color = "wfs0", fill = "wfs0", alpha = "wfs0"),
               size = 1) +
  geom_density(aes(x = wfs1_beta, color = "wfs1", fill = "wfs1", alpha = "wfs1"),
               size = 1) +
  # geom_line(aes(x = wfs0_beta, color = "wfs0"), stat = "density") +
  # geom_line(aes(x = wfs1_beta, color = "wfs1"), stat = "density") +
  geom_vline(aes(xintercept = -10), color = "black", linetype = 2) +
  scale_color_viridis(name = "WFS-impacted?", discrete = T,
                      labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_fill_viridis(name = "WFS-impacted?", discrete = T,
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_alpha_manual(name = "WFS-impacted?",
                     values = c("wfs1" = 0.25, "wfs0" = 0.25),
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  # xlab("BC \u03b2 value (True \u03b2 = 10)") + 
  # ylab("Density") +
  xlab(NULL) + ylab(NULL) +
  simple_theme +
  theme(legend.position = c(0.15, 0.8),
        plot.margin = unit(c(5,5,5,5), "mm"))
beta_10_plot
ggsave(filename = here::here("Figs", "ER_Sim_Beta_10.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 5, width = 5)

beta_20_plot <- ggplot(filter(beta_df, true_beta == -20)) +
  geom_density(aes(x = wfs0_beta, color = "wfs0", fill = "wfs0", alpha = "wfs0"),
               size = 1) +
  geom_density(aes(x = wfs1_beta, color = "wfs1", fill = "wfs1", alpha = "wfs1"),
               size = 1) +
  # geom_line(aes(x = wfs0_beta, color = "wfs0"), stat = "density") +
  # geom_line(aes(x = wfs1_beta, color = "wfs1"), stat = "density") +
  geom_vline(aes(xintercept = -20), color = "black", linetype = 2) +
  scale_color_viridis(name = "WFS-impacted?", discrete = T,
                      labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_fill_viridis(name = "WFS-impacted?", discrete = T,
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_alpha_manual(name = "WFS-impacted?",
                     values = c("wfs1" = 0.25, "wfs0" = 0.25),
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  # xlab("BC \u03b2 value (True \u03b2 = 20)") + 
  # ylab("Density") +
  xlab(NULL) + ylab(NULL) +
  simple_theme +
  theme(legend.position = c(0.15, 0.8),
        plot.margin = unit(c(5,5,5,5), "mm"))
beta_20_plot
ggsave(filename = here::here("Figs", "ER_Sim_Beta_20.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 5, width = 5)

beta_50_plot <- ggplot(filter(beta_df, true_beta == -50)) +
  geom_density(aes(x = wfs0_beta, color = "wfs0", fill = "wfs0", alpha = "wfs0"),
               size = 1) +
  geom_density(aes(x = wfs1_beta, color = "wfs1", fill = "wfs1", alpha = "wfs1"),
               size = 1) +
  # geom_line(aes(x = wfs0_beta, color = "wfs0"), stat = "density") +
  # geom_line(aes(x = wfs1_beta, color = "wfs1"), stat = "density") +
  geom_vline(aes(xintercept = -50), color = "black", linetype = 2) +
  scale_color_viridis(name = "WFS-impacted?", discrete = T,
                      labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_fill_viridis(name = "WFS-impacted?", discrete = T,
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  scale_alpha_manual(name = "WFS-impacted?",
                     values = c("wfs1" = 0.25, "wfs0" = 0.25),
                     labels = c("wfs1" = "Yes", "wfs0" = "No")) +
  # xlab("BC \u03b2 value (True \u03b2 = 50)") + 
  # ylab("Density") +
  xlab(NULL) + ylab(NULL) +
  simple_theme +
  theme(legend.position = c(0.15, 0.8),
        plot.margin = unit(c(5,5,5,5), "mm"))
beta_50_plot
ggsave(filename = here::here("Figs", "ER_Sim_Beta_50.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 5, width = 5)


#' Combine the plots
plot_labs <- paste0("\u03B2 = ", c(0, -5, -10, -20, -50))

beta_combined <- annotate_figure(
  ggarrange(beta_0_plot, beta_5_plot, beta_10_plot, beta_20_plot, beta_50_plot,
            ncol = 3, nrow = 2, common.legend = T, align = "hv",
            labels = plot_labs, label.x = 0.60, label.y = 0.85),
  bottom = text_grob("Beta coefficient"),
  left = text_grob("Denisty", rot = 90), 
)
beta_combined
ggsave(filename = here::here("Figs", "ER_Sim_Beta.jpeg"),
       device = "jpeg", dpi=750, units = "in", height = 6.5, width = 9)

#' -----------------------------------------------------------------------------
#' Smoke plumes plot
#' -----------------------------------------------------------------------------

#' Colorado boundary
library(USAboundaries)
co_state <- us_states(states = c("Colorado")) %>% 
  st_as_sf(sf_column_name = "geometry")
head(co_state)
co_state_m <- st_transform(co_state, crs = albers)

co_county <- st_read(here::here("Data", "Colorado_County_Boundaries.shp"))
head(co_county)
co_county_wgs <- st_transform(co_county, crs = ll_wgs84)
co_county_m <- st_transform(co_county, crs = albers)

head(co_county_wgs)
head(co_county_m)

co_bound <- st_transform(st_buffer(co_state_m, dist = 10000), crs = ll_wgs84)

#' Smoke plumes
hms_path <- "R:/RSTOR-Magzamen/Research/Secondary_Data/HMS_Smoke_Plumes/"
smoke_plumes <- st_read(paste0(hms_path, "hms_smoke20180805.shp"))
st_crs(smoke_plumes) <- ll_wgs84 
head(smoke_plumes)  

#' Denver Metro area counties
counties <- c("001", "005", "013", "014", "031", "059")

monitors <- read_csv(paste0(lur_path, "Monitor_PM_Data_AEA.csv")) %>% 
  filter(County_Code %in% counties) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>%
  st_transform(crs = ll_wgs84) %>% 
  dplyr::select(monitor_id)
head(monitors)

plot(st_geometry(co_bound))
plot(st_geometry(smoke_plumes), border = "red", add = T)
plot(st_geometry(monitors), col = "blue", add = T)

st_bbox(smoke_plumes)
x_min <- unname(st_bbox(co_bound)["xmin"])
x_max <- unname(st_bbox(co_bound)["xmax"]) 
y_min <- unname(st_bbox(co_bound)["ymin"])
y_max <- unname(st_bbox(co_bound)["ymax"])

smoke_plumes_crop <- st_crop(smoke_plumes, st_bbox(co_bound))

plume_map <- ggplot() +
  geom_sf(data = smoke_plumes_crop, aes(color = "smoke", fill = "smoke", alpha = "smoke"),  
          inherit.aes = F, show.legend = "polygon") +
  geom_sf(data = co_county_wgs, aes(color = "state", fill = "state", alpha = "state"), 
          inherit.aes = F, show.legend = "polygon") +
  geom_sf(data = monitors, aes(color = "monitor", fill = "monitor", alpha = "monitor"), 
          inherit.aes = F, show.legend = "point", size = 1) +
  scale_fill_manual(name = "", 
                    values = c("state" = NA, 
                               "smoke" = "#20A387FF", 
                               "monitor" = NA),
                    labels = c("state" = "Colorado county boundaries",
                               "smoke" = "Smoke plumes (Aug 5, '18)", 
                               "monitor" = "PM\u2082.\u2085 Monitors near Denver")) +
  scale_alpha_manual(name = "", 
                     values = c("state" = 1, 
                                "smoke" = 0.25, 
                                "monitor" = 1),
                     labels = c("state" = "Colorado county boundaries",
                                "smoke" = "Smoke plumes (Aug 5, '18)", 
                                "monitor" = "PM\u2082.\u2085 Monitors near Denver")) +
  scale_color_manual(name = "", 
                     values = c("state" = "plum3", 
                                "smoke" = "#20A387FF", 
                                "monitor" = "black"),
                     labels = c("state" = "Colorado county boundaries",
                                "smoke" = "Smoke plumes (Aug 5, '18)", 
                                "monitor" = "PM\u2082.\u2085 Monitors near Denver"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "solid", "solid"),
                                                              fill = c(NA, "#20A387FF", NA),
                                                              shape = c(16, NA, NA),
                                                              alpha = c(0.5, 0.25, 1)))) +
  north(co_bound, location = "bottomright", symbol = 12, scale = 0.05,
        anchor = c(x = -102.0, y = 36.75)) +
  scalebar(co_bound, dist = 100, dist_unit = "km", transform = T,
           height = 0.015, st.size = 3, st.bottom = F,
           anchor = c(x = -102.5, y = 36.75)) +
  xlab("") + ylab("") +
  map_theme + theme(legend.title = element_blank(),
                    legend.position = "left") 
plume_map

fig_name <- "Smoke_Plumes_Map.jpeg"
ggsave(filename = here::here("Figs", fig_name),
       device = "jpeg", dpi=500, units = "in", height = 6.5, width = 8)

#' -----------------------------------------------------------------------------
#' BC and PM2.5 trend data
#' -----------------------------------------------------------------------------

counties <- c("001", "005", "013", "014", "031", "059")
lur_data_path <- "R:/RSTOR-Magzamen/Research/Projects/ECHO_Aim1/Subprojects/ECHO_Aim1_LUR/Data/" 

#' PM2.5 concentrations
pm_data <- read_csv(paste0(lur_data_path, "Monitor_PM_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(week = isoweek(Date_Local),
         month = month(Date_Local),
         year = year(Date_Local)) %>% 
  mutate(month_yr = paste(month, year, sep="_"),
         start_iso_week = isoweek(as.Date(Date_Local))) %>%
  filter(Sample_Duration != "1 HOUR") %>% 
  filter(County_Code %in% counties) %>% 
  filter(year %in% c(2008:2018))

#' Black carbon
bc_data <- read_csv(paste0(lur_data_path, "Monitor_BC_Data_AEA.csv")) %>% 
  filter(!is.na(Arithmetic_Mean)) %>% 
  st_as_sf(wkt = "WKT", crs = albers) %>% 
  mutate(week = isoweek(Date_Local),
         month = month(Date_Local),
         year = year(Date_Local)) %>% 
  mutate(month_yr = paste(month, year, sep="_"),
         start_iso_week = isoweek(as.Date(Date_Local))) %>%
  filter(County_Code %in% counties) %>% 
  filter(year %in% c(2008:2018))

bc_ts <- ggplot(bc_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean), method = "lm", color = "red") +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Year") + ylab("Central site BC (\u03BCg/m\u00B3)") +
  theme(legend.position = "bottom") +
  simple_theme
bc_ts
ggsave(filename = here::here("Figs", "BC_Trend_Plot.jpeg"),
       device = "jpeg", dpi=500, units = "in", height = 6, width = 8)

bc_lm <- st_set_geometry(bc_data, NULL) %>% 
  select(monitor_id, Arithmetic_Mean, year) %>% 
  group_by(monitor_id) %>% 
  summarize(lm_beta = summary(lm(Arithmetic_Mean ~ year))$coef[2,1])
bc_lm

pm_ts <- ggplot(pm_data) +
  geom_point(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id),
             size = 0.5) +
  geom_line(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  # geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean, color = monitor_id)) +
  geom_smooth(aes(x = Date_Local, y = Arithmetic_Mean), method = "lm", color = "red") +
  scale_color_viridis(discrete = T, name = "Monitor ID") +
  xlab("Year") + ylab("Central site PM\u2082.\u2085 (\u03BCg/m\u00B3)") +
  facet_wrap(. ~ monitor_id) +
  theme(legend.position = "bottom") +
  simple_theme
pm_ts
ggsave(filename = here::here("Figs", "PM_Trend_Plot.jpeg"),
       device = "jpeg", dpi=500, units = "in", height = 7, width = 9)

pm_lm <- st_set_geometry(pm_data, NULL) %>% 
  select(monitor_id, Arithmetic_Mean, year) %>% 
  group_by(monitor_id) %>% 
  summarize(lm_beta = summary(lm(Arithmetic_Mean ~ year))$coef[2,1])
pm_lm

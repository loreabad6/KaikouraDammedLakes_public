#' Generate plots for the manuscript. 

#' Load libraries
library(cowplot)
library(ggmap)
library(ggnewscale)
library(ggspatial)
library(patchwork)
library(prettymapr)
library(raster)
library(rgee)
library(rnaturalearth)
library(scico)
library(sf)
library(stars)
library(tidyverse)
library(tmap)
library(tsibble)
library(units)

#' Some plots make use of functions, which are sourced from the following script:
source('figure_preparation/figure_plot_functions.R')

#' Set English as language to avoid problems with dates
Sys.setlocale("LC_TIME", "English")

#' Load data that is present in more than one figure
#' Some have already pre-processing steps
##' Study area
study_area = st_read('ref_data/North_Canterbury_AOI_2193.shp', quiet = T)
##' Coastal outline for NZ
##' Source: LINZ data portal
coast_outline = st_read('ref_data/nz-coastlines-and-islands-polygons-topo-150k.shp' , quiet = T) %>% 
  # Since NZ has several small islands, we will select the largest polygons to show as reference
  mutate(area = st_area(geometry)) %>% 
  top_n(3, area) 
##' Landslide dams inventory
##' Source: ECAN 2017
files_dams = list.files(path = 'ref_data', pattern = '_dams.shp', full.names = T)
landslide_dams = do.call(rbind, lapply(files_dams, st_read, quiet = T))
##' Hillshade generated from 15m DEM
hillshade = read_stars("NZ_DEM_15m/hillshade.tif", proxy = T) 
##' Mapping results exported from GEE
lakes = read_stars('results/results_update/mapping_results_update-0000000000-0000000000.tif', proxy = T)
lakesproj = read_stars('results/results_update/mapping_results_update-epsg2193.tif', proxy = T)
##' Validation areas
valareas = st_read('validation/validation_areas.geojson') %>% 
  # Select areas we use
  filter(id %in% c(0,3)) %>%
  # Project the data and calculate area
  st_transform(2193) 
validation_areas = valareas %>% 
  mutate(area = round(as.numeric(units::set_units(st_area(geometry),"km^2"))),1) %>% 
  # Add subset names and repeat geometries to reflect the years 
  # This process is mainly done to create an easy faceting 
  mutate(area_name = as.character(c("A","B"))) %>% 
  expand_grid(year = c('2016-12','2018-03','2019-01')) %>%
  # Convert back to sf and rearrange columns
  st_as_sf() %>%
  dplyr::select(-geometry, -`1`, geometry)

# Graphical Abstract ----
#' --------------------------------------------------------------------------------------------------

###' Call dam data 
dams = landslide_dams
dams_ts = dams %>% 
  # Select relevant dams for the time series plot
  dplyr::select(OBJECTID_1, Formal_Nam, Key_Dam) %>% 
  filter(OBJECTID_1 %in% c(484,516,528,574)) %>% 
  mutate(OBJECTID_1 = as.factor(OBJECTID_1))

###' Call computed result from GEE with lake areas per connected component
ee_Initialize(user = "loreabad6")
lake_sum = ee$Image("projects/ee-loreabad6/assets/Kaikoura_landslidedammedlakes_update/dammedLakes_components")

# Call and prepare time series
# points_time_series.geojson is generated here: 
# https://code.earthengine.google.com/b346dc0ff6f2669f94084dc7aa8719ab?noload=true

points = st_read('results/points_time_series.geojson') %>% 
  mutate(id = as.factor(id), OBJECTID_1 = as.factor(OBJECTID_1))
points_ee = sf_as_ee(points)

lake_area = ee_extract(lake_sum, points_ee, scale = 10, sf = TRUE)

lake_ts = lake_area %>% 
  pivot_longer(
    starts_with("lake"),
    names_to = 'dateid',
    values_to = 'area'
  ) %>% 
  mutate(
    date = dateid %>% 
      str_remove("lake") %>% 
      paste0("01") %>% 
      as.Date(format = "%Y%m%d"),
    month = yearmonth(format(date, '%Y-%m')),
    area = area / 10000
  )

ts = lake_ts %>% 
  st_drop_geometry() %>% 
  inner_join(dams_ts, by = 'OBJECTID_1') %>% 
  mutate(
    coord_x = paste(gsub(' ', '°', 
                         measurements::conv_unit(
                           round(st_coordinates(st_transform(geometry, 4326))[,1], 2),
                           from = 'dec_deg', 
                           to = 'deg_dec_min'
                         )), 'E'),
    coord_y = paste(
      gsub('-','',
           gsub(' ', '°', 
                measurements::conv_unit(
                  round(st_coordinates(st_transform(geometry, 4326))[,2], 2),
                  from = 'dec_deg', 
                  to = 'deg_dec_min'
                ))), 'S')
  ) %>% 
  mutate(coords = paste(coord_x, coord_y, sep = ", ")) %>% 
  group_by(Formal_Nam, month, OBJECTID_1) %>% 
  summarize(
    area = sum(area, na.rm = T), 
    Key_Dam = first(Key_Dam), 
    coords = first(coords)
  ) %>% 
  filter(
    month != yearmonth('2015-12-01')
  ) %>% 
  as_tsibble(key = c(Formal_Nam, Key_Dam, coords, OBJECTID_1), index = month) %>% 
  ungroup() %>% fill_gaps() %>% 
  mutate(
    category = ifelse(
      OBJECTID_1 == 484, 'increasing', 
      ifelse(OBJECTID_1 == 528, 'constant', 
             ifelse(OBJECTID_1 == 516, 'decreasing', 'variable')
      ))) %>% 
  mutate(label = paste0(Formal_Nam,
                        ifelse(Key_Dam == 'Y', ' (Key dam)',''), ': ', coords))


bourne_484 = plot_lake_occurrence(dams = dams, 484, nudge_x = 0,
                                  nudge_y = 120, position = 'right', 
                                  zoom_buffer = 250, background = T,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
leader_528 = plot_lake_occurrence(dams = dams, 528, nudge_x = -750,
                                  nudge_y = 500, zoom_buffer = 800,
                                  background = T,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
hapuku_516 = plot_lake_occurrence(dams = dams, 516, nudge_x = 200,
                                  nudge_y = 0, zoom_buffer = 500, 
                                  background = T,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
stanton_574 = plot_lake_occurrence(dams = dams, 574, nudge_x = 100, 
                                   nudge_y = 150, zoom_buffer = 300,
                                   background = T,
                                   scale_text_cex = 0.8,
                                   scale_height = unit(0.12, "cm"),
                                   scale_width_hint = 0.3,
                                   legend_title_size = 11,
                                   legend_text_size = 11)
time_series = ts_plot(ts, legend_title_size = 13,
                      date_breaks_months =  '12 month', label = F) + 
  facet_wrap(~category, ncol = 1, strip.position = 'left',
             scales = 'free_y'
  ) + 
  theme(
    strip.text.y.left = element_text(face = 'italic', size = 14,
                                     angle = 0, margin = margin(r = 10)), 
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    axis.title.y = element_blank()
  )

###' Create layout and patches for patchwork
layout = "
AAAAB
AAAAC
AAAAD
AAAAE
"

patch = time_series + leader_528 + hapuku_516 + bourne_484 + stanton_574 +
  plot_layout(design = layout) 

###' Apply function to combine plots with patchwork
# fig_ga = plot_build(patch, legend_title_size = 10, legend_text_size = 10)
fig_ga = 
  ((patch + plot_layout(widths = c(0.65,0.35))) + guide_area()) +
  plot_layout(guides = 'collect', width = c(0.65,0.35)) +
  plot_annotation(title = 'Landslide-dammed lake classification according to lake area evolution') &
  theme(
    plot.title = element_text(size = 18, color = 'grey20', 
                              face = 'italic', hjust = 0.5),
    legend.direction = 'vertical',
    legend.title = element_text(size = 13, margin = margin(b = 10)),
    legend.title.align = 0.5, legend.box = 'vertical',
    legend.text = element_text(size = 11), 
    legend.key.height = unit(7, 'mm')
    # legend.position = 'top'
  )
###' Save to JPEG
ggsave(
  'graphical_abstract_s2.jpeg', fig_ga,
  path = "figure_preparation/figure_putaiao", device = 'jpeg',
  width = 22, height = 15, units = 'cm', dpi = 650
)
###' Save to PNG
ggsave(
  'graphical_abstract_s2.png', fig_ga, 
  path = "figure_preparation/figure_putaiao", device = 'png',
  width = 22, height = 15, units = 'cm', dpi = 650
)

# With orthophoto background -----------
ortho_list = list.files(
  path = "figure_preparation/figure_putaiao/orthophotos",
  full.names = TRUE, pattern = ".tif"
)
ortho_mosaic = st_mosaic(ortho_list)
ortho = read_stars(ortho_mosaic, proxy = TRUE)

bourne_484_o = plot_lake_occurrence(dams = dams, 484, nudge_x = 0,
                                  nudge_y = 120, position = 'right', 
                                  zoom_buffer = 250, background = T,
                                  background_custom = ortho,
                                  bg_downsample = 0,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
leader_528_o = plot_lake_occurrence(dams = dams, 528, nudge_x = -750,
                                  nudge_y = 500, zoom_buffer = 800,
                                  background = T,
                                  background_custom = ortho,
                                  bg_downsample = 2,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
hapuku_516_o = plot_lake_occurrence(dams = dams, 516, nudge_x = 200,
                                  nudge_y = 0, zoom_buffer = 500, 
                                  background = T,
                                  background_custom = ortho,
                                  bg_downsample = 0,
                                  scale_text_cex = 0.8,
                                  scale_height = unit(0.12, "cm"),
                                  scale_width_hint = 0.3,
                                  legend_title_size = 11,
                                  legend_text_size = 11)
stanton_574_o = plot_lake_occurrence(dams = dams, 574, nudge_x = 250, 
                                   nudge_y = 150, zoom_buffer = 300,
                                   background = T,
                                   background_custom = ortho,
                                   bg_downsample = 0,
                                   scale_text_cex = 0.8,
                                   scale_height = unit(0.12, "cm"),
                                   scale_width_hint = 0.3,
                                   legend_title_size = 11,
                                   legend_text_size = 11)



patch_o = time_series + leader_528 + hapuku_516_o + bourne_484_o + stanton_574_o +
  plot_layout(design = layout) 

###' Apply function to combine plots with patchwork
# fig_ga = plot_build(patch, legend_title_size = 10, legend_text_size = 10)
fig_ga_o = 
  ((patch_o + plot_layout(widths = c(0.65,0.35))) + guide_area()) +
  plot_layout(guides = 'collect', width = c(0.65,0.35)) +
  plot_annotation(title = 'Landslide-dammed lake classification according to lake area evolution') &
  theme(
    plot.title = element_text(size = 18, color = 'grey20', 
                              face = 'italic', hjust = 0.5),
    legend.direction = 'vertical',
    legend.title = element_text(size = 13, margin = margin(b = 10)),
    legend.title.align = 0.5, legend.box = 'vertical',
    legend.text = element_text(size = 11), 
    legend.key.height = unit(7, 'mm')
    # legend.position = 'top'
  )
###' Save to JPEG
ggsave(
  'graphical_abstract_ortho_temp.jpeg', fig_ga_o,
  path = "figure_preparation/figure_putaiao", device = 'jpeg',
  width = 22, height = 15, units = 'cm', dpi = 650
)
###' Save to PNG
ggsave(
  'graphical_abstract_ortho.png', fig_ga_o, 
  path = "figure_preparation/figure_putaiao", device = 'png',
  width = 22, height = 15, units = 'cm', dpi = 650
)


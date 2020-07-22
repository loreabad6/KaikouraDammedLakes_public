#' Generate plots for the manuscript. 

#' Load libraries
library(cowplot)
library(dplyr)
library(ggmap)
library(ggplot2)
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
library(stringr)
library(tidyr)
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
lakes = read_stars('results/mapping_results-0000000000-0000000000.tif', proxy = T)
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

# Figure 1 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 1** Study area located in the northeast of the South Island, New Zealand

###' Load required data and preprocess
sta = study_area %>% 
  st_transform(crs = 4326) %>% mutate(name = 'Study Area')
dem = read_stars('NZ_DEM_15m/nzdem_nztm_nzgd49_kaikoura.tif', quiet = T, proxy = T) 
coast = coast_outline %>% 
  st_transform(crs = 4326)
epicenter = st_sf(
  name = "Earthquake epicenter", color = 'red', shape = as.factor(16),
  geometry = st_sfc(st_point(c(173.02, -42.69)), crs = 4326)
)
dams = landslide_dams %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(geometry) %>% 
  mutate(name = 'GNS landslide dams', color = 'white', shape = as.factor(17))
extra = rbind(dams, epicenter)

###' Call function to generate plot
fig1 = sa_plot(
  country = 'New Zealand', aoi = sta, proj_crs = 2193, 
  zoom_background = dem, hillshade_effect = hillshade, font_family = 'sans',
  extra_feature = extra, extra_legend = "name", 
  extra_shape = "shape", extra_outline = coast  ## star shape: "\u2605"
)

###' Save to PDF
ggsave(
  "fig1.pdf", 
  fig1, 
  device = "pdf", 
  path = "manuscript/figures", 
  width = 10, height = 10,
  units = "cm", 
  dpi = "retina"
)

###' Save to PNG
ggsave(
  "fig1.png", 
  fig1, 
  device = "png", 
  path = "manuscript/figures", 
  width = 10, height = 10,
  units = "cm", 
  dpi = "retina"
)


# Figure 2 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 2** Landslide-dammed lake detection workflow. MNDWI stands for Modified Normalized 
#' Difference Water Index, GEP stands for Google Earth Pro. The determination of the *Th* threshold 
#' is further explained in section 3.2
#' 
#' Created in power point. See figure_preparation/figure_methods.pptx

# Figure 3 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 3** Sentinel-2 scenes used to generate post-event monthly mosaics. a) MGRS tiles\' location, 
#' b) the number of Sentinel-2 scenes per MGRS tile

###' Call and pre-process data
sta = study_area %>% 
  st_transform(crs = 4326) %>% mutate(name = 'Study Area')
coast = coast_outline %>% 
  st_transform(crs = 4326)

###' Create data table. Information obtained from the GEE script which fetches the information on all 
###' the imagery used to generate the monthly mosaics. It was then manually copied to this script
mosaic_data = tibble(
  month = as.Date(
    c('01/12/2016', '01/01/2017', '01/02/2017', '01/10/2017', '01/11/2017', '01/12/2017', 
      '01/01/2018', '01/02/2018', '01/03/2018', '01/10/2018', '01/11/2018', '01/01/2019', 
      '01/02/2019', '01/03/2019', '01/01/2020', '01/02/2020', '01/03/2020'), format = '%d/%m/%Y'),
  # value = c(13,9,29,22,35,33,34,29,33,37,29,48,50,39),
  # s2a = c(13, 9,29,12,15,21,21,11,17,15,11,26,27,18,13,25, 8),
  # s2b = c( 0, 0, 0,10,20,12,13,18,16,22,18,22,23,21, 7,12,31),
  `59GPN` = c(1,2,4,3,4,3,4,3,6,4,3,6,7,5,2,5,4),
  `59GPP` = c(1,2,3,4,4,4,4,3,4,8,5,7,6,6,3,3,6),
  `59GQN` = c(1,1,4,3,5,4,4,5,5,4,5,7,5,4,1,5,5),
  `59GQP` = c(2,1,5,4,5,5,6,4,6,5,4,6,7,5,4,6,4),
  `59GQQ` = c(3,1,4,2,6,6,5,5,3,5,4,8,9,7,3,6,8),
  `60GTU` = c(2,1,5,4,5,5,6,4,6,5,4,6,7,5,4,6,4),
  `60GTV` = c(3,1,4,2,6,6,5,5,3,6,4,8,9,7,3,6,8)
) %>% tidyr::gather(property, count, -month)

###' Load required data and preprocess
s2tiles = st_read('ref_data/s2tiles_kaikoura.kml')
s2tiles_study = s2tiles %>% 
  st_zm(drop = T) %>% 
  right_join(mosaic_data, by = c('Name'='property')) %>% 
  group_by(Name) %>% 
  summarize(sum = sum(count))

###' Generate plot 3a
fig3a = ggplot() + 
  geom_sf(data = coast, fill = NA, color = 'grey50', lwd = 0.3) +
  geom_sf(data = s2tiles_study, aes(fill = Name), color = NA, alpha = 0.7, show.legend = F) + 
  scico::scale_fill_scico_d('MGRS Tile', palette = 'batlow') +
  geom_sf(data = sta, color = 'grey10', fill = NA, lwd = 0.4) + 
  geom_sf_text(data = sta, aes(label = name), family = 'sans', size = 3, angle = 56) +
  scale_x_continuous(breaks = c(172.5, 173.5, 174.5)) +
  scale_y_continuous(breaks = c(-43, -42, -41)) + 
  coord_sf(
    xlim = unname(st_bbox(s2tiles_study)[c("xmin", "xmax")]),
    ylim = unname(st_bbox(s2tiles_study)[c("ymin", "ymax")]),
    label_graticule = 'NW', clip = "on"
  ) + 
  theme(
    text = element_text(family = 'sans', size = 9),
    panel.background = element_rect(fill = "transparent"), 
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.border = element_rect(fill = NA, color = 'grey50')
  )

###' Generate plot 3b
fig3b = ggplot(mosaic_data ) +
  geom_col(aes(x = month, y = count, fill = property), alpha = 0.7) + 
  # scale_fill_manual('Satellite', values = c('lightskyblue1', 'lightskyblue3'), labels = c('Sentinel-2A', 'Sentinel-2B')) +
  xlab('') + ylab('Number of Sentinel-2 scenes') +
  scico::scale_fill_scico_d(
    'MGRS Tile', palette = 'batlow',
    guide = guide_legend(label.position = 'bottom', nrow = 1)
  ) +
  scale_x_date(date_breaks = '3 month',  date_labels = '%m-%Y') +
  scale_y_continuous(position = 'right') +
  theme(
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8), size = 9),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = 'top', legend.text = element_text(size = 7), 
    legend.title = element_text(size = 9), 
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0, 0, -5, 0), 
    text = element_text(family = 'sans'), 
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = 'grey85', linetype = 'dotted', size = 0.25),
    panel.border = element_rect(color = 'grey50', fill = 'transparent')
  )

fig3 = plot_grid(
  fig3a, fig3b, 
  ncol = 2, rel_widths = c(0.35,0.65), 
  labels = list('a)', 'b)'), 
  label_fontfamily = 'sans', label_size = 9, 
  label_fontface = 'plain'
)

###' Save to PDF
ggsave(
  "fig3.pdf", 
  fig3, 
  device = "pdf", 
  path = "manuscript/figures", 
  width = 15, height = 7.6,
  units = "cm", 
  dpi = "retina"
)

###' Save to PNG
ggsave(
  "fig3.png", 
  fig3, 
  device = "png", 
  path = "manuscript/figures", 
  width = 15, height = 7.6,
  units = "cm", 
  dpi = "retina"
)

# Figure 4 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 4** Classification refinement workflow
#'  
#' Created in power point. See figure_preparation/figure_refinement.pptx

#' --------------------------------------------------------------------------------------------------
#' **Fig. 5** Subset areas used for validation and examples of landslide-dammed lakes. 
#' a) Location of subset areas within the study area. 
#' b) Hapuku river landslide dam (Hapuku 740) and its corresponding lake within subset B 
#' (© James Thompson -- Environment Canterbury 2016; date taken: January 4, 2017). 
#' c) Leader river landslide-dammed lake and d) Leader river dam (Leader 220) located within subset A 
#' (© Anne-Laure Argentin 2019; date taken: January 3, 2019)
#' 
#' Fig 5a was created with the code below. Fig 5b,c,d are field photographs. The final composition is 
#' done in powerpoint. See figure_preparation/figure_validation_field.pptx

###' Call data and preprocess
starea = study_area %>%
  st_transform(crs = 2193)
coast = coast_outline %>% 
  st_transform(crs = 2193) 
areas_reshaped = validation_areas

###' Generate plot 5a
fig5a = ggplot() +
  geom_sf(data = starea, col = NA, fill = 'grey80') +
  geom_sf(data = coast %>% st_crop(starea), fill = 'grey90',  alpha = 0.7, col = NA) +
  geom_sf(
    data = areas_reshaped %>% filter(year == '2016-12'), 
    aes(col = year), fill = NA
  ) +
  scale_color_manual('', values = 'red', labels = 'validation subset') +
  geom_sf_text(
    data = areas_reshaped %>% filter(year == '2016-12'), 
    aes(label = area_name), size = 2, nudge_y = 10000
  ) +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  annotation_scale(
    plot_unit = 'm', data = scale_params, location = 'br', 
    width_hint = 0.25, height = unit(0.15, "cm"), text_cex = 0.5
  ) +
  theme_void() +
  theme(
    legend.position = c(0.3,0.99), 
    plot.margin = margin(), 
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, 'cm'), legend.spacing.x = unit(0.1, 'cm')
  )

###' Save to PNG
ggsave(
  fig5a, filename = 'fig5a.png', 
  path = "figure_preparation/figure_elements", device = 'png',
  width = 4, height = 4.7, units = 'cm', dpi = 300
)

# Figure 6 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 6** Landslide-dammed lake detection and mapping results for three selected time periods and 
#' two study area subsets. Lakes indicated in blue, background: hillshade generated from the 15 m 
#' pre-earthquake DEM

###' Load required data and preprocess
areas_reshaped = validation_areas
areas_hs = areas_reshaped %>% 
  st_transform(crs = st_crs(hillshade)) %>% 
  filter(year == '2016-12')
hs_a0 = hillshade[areas_hs[1,]]
hs_a3 = hillshade[areas_hs[2,]]
lakes = read_stars(
  'results/mapping_results-0000000000-0000000000.tif', 
  proxy = T, NA_value = 0
) 
areas_lakes = areas_reshaped %>% 
  st_transform(crs = st_crs(lakes)) %>% 
  filter(year == '2016-12') %>% 
  mutate(name = c('Subset A', 'Subset B'))
lakes_a0 = lakes[,,,c(2,9,13)][areas_lakes[1,]] %>% 
  st_as_stars() %>% 
  na_if(0) %>% 
  dplyr::select(lake = everything()) %>% 
  st_set_dimensions(which = 'band', values = c('December 2016','March 2018','January 2019'))
lakes_a3 = lakes[,,,c(2,9,13)][areas_lakes[2,]] %>%   
  st_as_stars() %>% 
  na_if(0) %>% 
  dplyr::select(lake = everything()) %>% 
  st_set_dimensions(which = 'band', values = c('December 2016','March 2018','January 2019'))
# Workaround to add scale bar only to one plot
scale_params = tibble::tibble(
  band = factor('January 2019')
)

###' Generate plot 6 left
fig6left = ggplot() +
  geom_stars(data = hs_a0) +
  scico::scale_fill_scico(15, begin = 0, end = 0.25, alpha = 0.75, palette = 'grayC', na.value = NA) +
  coord_sf(crs = 2193) +
  new_scale_fill() +
  geom_stars(data = lakes_a0, downsample = 1) +
  scale_fill_gradient(low = 'darkblue', high = 'darkblue', na.value = NA) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~band, ncol = 1, strip.position = 'left') +
  labs(title = 'Subset A') +
  annotation_scale(
    plot_unit = 'm', data = scale_params, location = 'br', 
    width_hint = 0.25, height = unit(0.15, "cm"), text_cex = 0.5
  ) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, margin = margin(b = 1, unit = 'mm')),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.background = element_blank(), plot.background = element_blank(), 
    strip.background = element_blank(), panel.grid = element_blank(),
    # plot.margin = margin(rep(0.5,4), unit = 'mm'), 
    legend.position = 'none', 
    panel.spacing = unit(1, "mm")#, panel.spacing.y = unit(0, "cm")
  )

###' Generate plot 6 right
fig6right = ggplot() +
  geom_stars(data = hs_a3) +
  scico::scale_fill_scico(15, begin = 0, end = 0.25, alpha = 0.75, palette = 'grayC', na.value = NA) +
  coord_sf(crs = 2193) +
  new_scale_fill() +
  geom_stars(data = lakes_a3, downsample = 1) +
  scale_fill_gradient(low = 'darkblue', high = 'darkblue', na.value = NA) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~band, ncol = 1, strip.position = 'left') +
  labs(title = 'Subset B') +
  annotation_scale(
    plot_unit = 'm', data = scale_params, location = 'br', 
    width_hint = 0.25, height = unit(0.15, "cm"), text_cex = 0.5
  ) +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, margin = margin(b = 1, unit = 'mm')),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
    panel.background = element_blank(), plot.background = element_blank(), 
    strip.background = element_blank(), panel.grid = element_blank(),
    strip.text = element_blank(), #plot.margin = margin(rep(0.5,4), unit = 'mm'),
    legend.position = 'none', panel.spacing = unit(0.5, "mm")#, panel.spacing.y = unit(0, "cm")
  )

fig6 = fig6left | fig6right

###' Save to PDF
ggsave(
  fig6, filename = 'fig6.pdf', 
  path = "manuscript/figures", device = 'pdf',
  width = 15, height = 12.56, units = 'cm', dpi = 350
)

###' Save to PNG
ggsave(
  fig6, filename = 'fig6.png', 
  path = "manuscript/figures", device = 'png',
  width = 15, height = 12.56, units = 'cm', dpi = 350
)

# Figure 7 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 7** Landslide-dammed lake surface area monitoring and time series analysis for selected dams

###' Call dam data 
dams = landslide_dams
dams_ts = dams %>% 
  # Select relevant dams for the time series plot
  dplyr::select(OBJECTID_1, Formal_Nam, Key_Dam) %>% 
  filter(OBJECTID_1 %in% c(484,499,516,528,534,536,540,563,569,574)) %>% 
  mutate(OBJECTID_1 = as.factor(OBJECTID_1))

# Call and prepare time series files. This are exported manually from the GEE.
files_ts = list.files('time-series', pattern = '.csv', full.names = T) 
ts = do.call(
  rbind, 
  lapply(
    files_ts, 
    function(x) cbind(
      read.csv(x, na.strings = '', dec = '.', stringsAsFactors = F),
      name=strsplit(strsplit(x,'\\/')[[1]][2], '\\.')[[1]][1])
  )
) %>% 
  mutate(
    date = as.Date(system.time_start, format = '%b %d, %Y'),
    sumArea = as.numeric(gsub(",", "", sumArea))/10000,
    month = yearmonth(format(date, '%Y-%m')),
    objectid = stringr::str_extract(name, "(\\d)+")
  ) %>%
  inner_join(dams_ts, by = c('objectid' = 'OBJECTID_1')) %>% 
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
  group_by(Formal_Nam, month, objectid) %>% 
  summarize(
    sumArea = sum(sumArea, na.rm = T), 
    Key_Dam = first(Key_Dam), 
    coords = first(coords)
  ) %>% 
  filter(
    month != yearmonth('2015-12-01')
  ) %>% 
  as_tsibble(key = c(Formal_Nam, Key_Dam, coords, objectid), index = month) %>% 
  ungroup() %>% fill_gaps() %>% 
  mutate(
    category = ifelse(
      objectid == 484, 'increasing', 
      ifelse(objectid == 528, 'constant', 
             ifelse(objectid == 516, 'decreasing', 'variable')
      ))) %>% 
  mutate(label = paste0(Formal_Nam, ifelse(Key_Dam == 'Y', ' (Key dam)',''), ': ', coords))

###' Apply function to selected dams to obtain the lake occurrence plots
bourne_484 = plot_lake_occurrence(dams = dams, 484, nudge_x = 0, nudge_y = 120, position = 'right', zoom_buffer = 250)
conway_499 = plot_lake_occurrence(dams = dams, 499, nudge_x = -38, nudge_y = 286, zoom_buffer = 300)
hapuku_516 = plot_lake_occurrence(dams = dams, 516, nudge_x = 200, nudge_y = 0, zoom_buffer = 500)
leader_528 = plot_lake_occurrence(dams = dams, 528, nudge_x = -750, nudge_y = 500, zoom_buffer = 800)
leader_534 = plot_lake_occurrence(dams = dams, 534, nudge_x = 0, nudge_y = 0, zoom_buffer = 200)

leader_536 = plot_lake_occurrence(dams = dams, 536, nudge_x = 0, nudge_y = 0, position = 'right', zoom_buffer = 200)
linton_540 = plot_lake_occurrence(dams = dams, 540, nudge_x = 0, nudge_y = 0, zoom_buffer = 200)
stanton_563 = plot_lake_occurrence(dams = dams, 563, nudge_x = 0, nudge_y = 150, zoom_buffer = 300)
stanton_569 = plot_lake_occurrence(dams = dams, 569, nudge_x = 0, nudge_y = 0, zoom_buffer = 200)
stanton_574 = plot_lake_occurrence(dams = dams, 574, nudge_x = 100, nudge_y = 150, zoom_buffer = 300)

###' Apply function to plot the time series of lake surface, data is split in half.
###' That is 5 dams per page
ts1 = ts_plot(ts[1:200,])
ts2 = ts_plot(ts[201:400,])

###' Create layout and patches for patchwork
layout = "
AAAAB
AAAAC
AAAAD
AAAAE
AAAAF
"

patch1 = ts1 + bourne_484 + conway_499 + hapuku_516 + leader_528 + leader_534 +
  plot_layout(design = layout)
patch2 = ts2 + leader_536 + linton_540 + stanton_563 + stanton_569 + stanton_574 +
  plot_layout(design = layout)

###' Apply function to combine plots with patchwork
fig7a = plot_build(patch1)
fig7b = plot_build(patch2)

###' Save to PDF
ggsave(
  'fig7a.pdf', fig7a, 
  path = "manuscript/figures", device = 'pdf',
  width = 16.5, height = 21.2, units = 'cm', dpi = 350
)
ggsave(
  'fig7b.pdf', fig7b, 
  path = "manuscript/figures", device = 'pdf',
  width = 16.5, height = 21.2, units = 'cm', dpi = 350
)
###' Save to PNG
ggsave(
  'fig7a.png', fig7a, 
  path = "manuscript/figures", device = 'png',
  width = 16.5, height = 21.2, units = 'cm', dpi = 350
)
ggsave(
  'fig7b.png', fig7b, 
  path = "manuscript/figures", device = 'png',
  width = 16.5, height = 21.2, units = 'cm', dpi = 350
)

# Figure 8 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 8** Landslide dam examples where no lakes were detected by the automatic method on 
#' Sentinel-2 imagery. Left column: True Color composite (RGB: B4-B3-B2) with landslide dam 
#' locations and detected landslide-dammed lakes indicated. Center column: False Color composite 
#' (RGB: B8-B4-B3). Right column: MNDWI as calculated in equation (1)

###' Call dam data 
dams = landslide_dams

###' This plot gathers data from the GEE directly, so a connection with the `rgee` package is required
ee_Initialize()
ee_reattach()

###' Note: the recordPlot() function allows to store the resulting plot to a device. It will record
###' as it shows on the plot viewer on RStudio and this can be a source of misplacement of 
###' certain elements in the final plot.
###' Hint: set plot viewer to 1022, 357 for good results

###' Apply function to generate facet plots for specific dams were no lakes were detected
plot_lake_detection(510, zoom_buffer = 700)
p1 = recordPlot()
plot_lake_detection(532, zoom_buffer =800)
p2 = recordPlot()
plot_lake_detection(553, zoom_buffer = 600)
p3 = recordPlot()
plot_lake_detection(604, zoom_buffer = 1500)
p4 = recordPlot()

###' Create a color bar for the legend 
par(mfrow = c(1,1), mar = c(1,0,1,1), mgp = c(0,0,0), cex = 0.6, bg = NA)
z=matrix(1:30,nrow=30)
y=1
x=seq(-0.5,0.5,len=30)
colors = scico::scico(n = 30, begin = 0.1, end = 0.9, direction = -1, palette = 'nuuk')
image(x,y,z, col=colors,axes=FALSE,xlab="",ylab="")
axis(1,tick = F,line = F)
p5 = recordPlot()

###' Create the remaining elements of the legend
#https://stackoverflow.com/questions/21262472/adjust-spacing-between-text-in-horizontal-legend
legtext = c('Detected landslide-dammed lake','GNS lansdlide dam', 'MNDWI values')
xcoords = c(0, 0.45, 0.74)
secondvector = (1:length(legtext))-1
textwidths = xcoords/secondvector # this works for all but the first element
textwidths[1] = 0 
par(mfrow = c(1,1), mar = rep(0,4), cex = 0.6, bg = NA)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend(
  0, 0.5,
  x.intersp = 1.7,
  text.width = textwidths,
  horiz = T, 
  legend = legtext, 
  col = c(
    rgb(0, 255, 255, alpha = 100, maxColorValue = 255),
    'red', 
    'white'
  ),
  pch = c(15, 20, 15), pt.cex = 2,
  bty = "n" 
)
p6 = recordPlot()

###' Combine the plots together
top = plot_grid(
  p1, p2, p3, p4,
  nrow = 4
)
bottom = plot_grid(p6, p5, ncol = 2, rel_widths = c(0.7, 0.3))
final = plot_grid(top, bottom, nrow = 2, rel_heights = c(0.95,0.05))

###' Save to PDF
save_plot('manuscript/figures/fig8.pdf', final, dpi = 350, base_height = 8.1, base_width = 5.8)

###' Save to PNG
save_plot('manuscript/figures/fig8.png', final, dpi = 350, base_height = 8.1, base_width = 5.8)

# Figure 9 ----
#' --------------------------------------------------------------------------------------------------
#' **Fig. 9** Examples of the accuracy assessment results for "Leader 220" located in validation 
#' subset A and "Hapuku 740" located in validation subset B. True Positives (TP), True Negatives(TN), 
#' False Positives (FP) and False Negatives (FN) area shown at the pixel level

###' Create pixel-wise confusion matrix for subsets A and B
comp_a0 = create_pixelwise_confusion_matrix(0)
comp_a3 = create_pixelwise_confusion_matrix(3)

###' Create zooms around specific dams as examples
leader_zoom = landslide_dams %>% 
  filter(OBJECTID_1 == 528) %>% 
  st_buffer(900, endCapStyle = "SQUARE") 
st_geometry(leader_zoom) = st_sfc(st_geometry(leader_zoom)[[1]] + sf::st_point(c(-850, 500)), crs = 2193)

hapuku_zoom = landslide_dams %>% 
  filter(OBJECTID_1 == 516) %>% 
  st_buffer(700, endCapStyle = "SQUARE")

###' Define color palette
###' Colors inspired by
###' https://news.uchicago.edu/sites/default/files/attachments/_uchicago.identity.guidelines.pdf
###' page 41
cm_palette = c("#155F83", "#ADB17D", "#9e265e", "#FFB547")

###' Create a customized legend to show the colors on a confusion matrix 
legend = data.frame(
  pred = c(1,0,1,0), 
  ref = c(1,0,0,1), 
  cm = c(1,2,3,4), 
  label = c('TP', 'TN', 'FP', 'FN')
) %>% 
  mutate_all(as.factor) %>% 
  mutate_at(vars(ref), function(x) factor(x, levels = c(1,0), ordered = T))

legend_matrix = ggplot(legend, aes(y = pred, x = ref, fill = cm)) + 
  geom_tile(alpha = 0.75, color = NA) +
  geom_text(aes(label = label)) +
  xlab('Reference') + ylab('Automatic detection') +
  scale_fill_manual(values = cm_palette, na.translate = F) +
  scale_x_discrete(expand = c(0,0), position = "top", labels = c('Lake', 'No lake')) +
  scale_y_discrete(expand = c(0,0), labels = c('No lake', 'Lake')) +
  theme(
    axis.text = element_text(size = 9, color = 'black'),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 9),
    legend.position = 'none',
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    aspect.ratio = 1
  )

###' Generate plots and combine them with patchwork
facets = cm_plot(
  comp_a0[leader_zoom], 
  hillshade[st_transform(leader_zoom, crs = st_crs(hillshade))], 
  'Leader 220'
  ) /
cm_plot(
  comp_a3[hapuku_zoom], 
  hillshade[st_transform(hapuku_zoom, crs = st_crs(hillshade))], 
  'Hapuku 740', show_title = F
  )

fig9 = (facets | legend_matrix) + 
  plot_layout(ncol = 2, widths = c(0.85, 0.15)) & 
  theme(plot.margin = margin(-10,1,0,1, unit = 'mm'))

###' Save to PDF
ggsave(
  'fig9.pdf', 
  fig9, 
  path = "manuscript/figures",
  width = 18, height = 10, units = 'cm', 
  dpi = 350, device = 'pdf'
)

###' Save to PNG
ggsave(
  'fig9.png', fig9, 
  path = "manuscript/figures",
  width = 18, height = 10, units = 'cm', 
  dpi = 350,  device = 'png'
)

# Graphical Abstract ----
#' --------------------------------------------------------------------------------------------------

###' Call dam data 
dams = landslide_dams
dams_ts = dams %>% 
  # Select relevant dams for the time series plot
  dplyr::select(OBJECTID_1, Formal_Nam, Key_Dam) %>% 
  filter(OBJECTID_1 %in% c(484,516,528,574)) %>% 
  mutate(OBJECTID_1 = as.factor(OBJECTID_1))

# Call and prepare time series files. This are exported manually from the GEE.
files_ts = list.files('time-series', pattern = '.csv', full.names = T) 
ts = do.call(
  rbind, 
  lapply(
    files_ts, 
    function(x) cbind(
      read.csv(x, na.strings = '', dec = '.', stringsAsFactors = F),
      name=strsplit(strsplit(x,'\\/')[[1]][2], '\\.')[[1]][1])
  )
) %>% 
  mutate(
    date = as.Date(system.time_start, format = '%b %d, %Y'),
    sumArea = as.numeric(gsub(",", "", sumArea))/10000,
    month = yearmonth(format(date, '%Y-%m')),
    objectid = stringr::str_extract(name, "(\\d)+")
  ) %>%
  inner_join(dams_ts, by = c('objectid' = 'OBJECTID_1')) %>% 
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
  group_by(Formal_Nam, month, objectid) %>% 
  summarize(
    sumArea = sum(sumArea, na.rm = T), 
    Key_Dam = first(Key_Dam), 
    coords = first(coords)
    #   apply(
    #     round(
    #       st_coordinates(
    #         st_transform(geometry, 4326)
    #       ),
    #       2
    #     ),
    #     1,paste,collapse=", ")
    # )
  ) %>% 
  filter(
    month != yearmonth('2015-12-01')
  ) %>% 
  as_tsibble(key = c(Formal_Nam, Key_Dam, coords, objectid), index = month) %>% 
  ungroup() %>% fill_gaps() %>% 
  mutate(
    category = ifelse(
      objectid == 484, 'increasing', 
      ifelse(objectid == 528, 'constant', 
             ifelse(objectid == 516, 'decreasing', 'variable')
      ))) %>% 
  mutate(label = paste0(Formal_Nam, ifelse(Key_Dam == 'Y', ' (Key dam)',''), ': ', coords))

bourne_484 = plot_lake_occurrence(dams = dams, 484, nudge_x = 0, nudge_y = 120, position = 'right', zoom_buffer = 250, bg = T)
leader_528 = plot_lake_occurrence(dams = dams, 528, nudge_x = -750, nudge_y = 500, zoom_buffer = 800, bg = T)
hapuku_516 = plot_lake_occurrence(dams = dams, 516, nudge_x = 200, nudge_y = 0, zoom_buffer = 500, bg = T)
stanton_574 = plot_lake_occurrence(dams = dams, 574, nudge_x = 100, nudge_y = 150, zoom_buffer = 300, bg = T)
time_series = ts_plot(ts) + 
  facet_wrap(~category, ncol = 1, strip.position = 'left', scales = 'free_y') + 
  ylab('Landslide-dammed lake classification according to lake area evolution') +
  theme(
    strip.text = element_text(face = 'italic'), 
    axis.title.y = element_text(size = 10, color = 'grey20', face = 'italic'),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 7)
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
fig_ga = plot_build(patch)

###' Save to PDF
ggsave(
  'graphical_abstract.pdf', fig_ga, 
  path = "manuscript/figures", device = 'pdf',
  width = 16.5, height = 18, units = 'cm', dpi = 350
)
###' Save to PNG
ggsave(
  'graphical_abstract.png', fig_ga, 
  path = "manuscript/figures", device = 'png',
  width = 16.5, height = 18, units = 'cm', dpi = 350
)

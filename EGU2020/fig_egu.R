library(dplyr)
library(sf)
library(stars)
library(ggplot2)
library(ggtext)
library(tsibble)
library(ggsflabel)

sta <- st_read('ref_data/North_Canterbury_AOI_2193.shp', quiet = T) %>% st_transform(crs = 4326) %>% mutate(name = 'Study Area')
epicenter = st_sf(st_sfc(st_point(c(173.02, -42.69)), crs = 4326))
dem <- read_stars('NZ_DEM_15m/nzdem_nztm_nzgd49_kaikoura.tif', quiet = T, proxy = T) 
coast <- st_read('ref_data/nz-coastlines-and-islands-polygons-topo-150k.shp' , quiet = T) %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(3, area) %>% 
  st_transform(crs = 4326)
source('study_area_plot_function.R')
sa_plot = sa_plot(
  country = 'New Zealand', aoi = sta, proj_crs = 2193, zoom_background = dem, font_family = 'sans',
  extra_font_size = 3.5, font_size = 12,
  extra_feature = epicenter, extra_shape = 18, extra_size = 3, extra_label = 'Earthquake\nEpicenter', extra_outline = coast 
  ## star shape: "\u2605"
)
ggsave('EGU2020/study_area.png', sa_plot, height = 15, width = 14, units = 'cm', dpi = 300)

mosaic_data = tibble(
  month = as.Date(
    c('01/12/2015','01/12/2016', '01/01/2017', '01/02/2017', 
      '01/10/2017', '01/11/2017', '01/12/2017', '01/01/2018', 
      '01/02/2018', '01/03/2018', '01/10/2018', '01/11/2018', 
      '01/01/2019', '01/02/2019', '01/03/2019', '01/01/2020', 
      '01/02/2020', '01/03/2020'), 
    format = '%d/%m/%Y'),
  # value = c(3,13,9,29,22,35,33,34,29,33,37,29,48,50,39),
  `Sentinel-2A` = c( 3, 13, 9,29,12,15,21,21,11,17,15,11,26,27,18,13,25, 8),
  `Sentinel-2B` = c( 0, 0, 0, 0,10,20,12,13,18,16,22,18,22,23,21, 7,12,31),
  # `59GPN` = c(1,1,2,4,3,4,3,4,3,6,4,3,6,7,5,2,5,4),
  # `59GPP` = c(1,1,2,3,4,4,4,4,3,4,8,5,7,6,6,3,3,6),
  # `59GQN` = c(0,1,1,4,3,5,4,4,5,5,4,5,7,5,4,1,5,5),
  # `59GQP` = c(1,2,1,5,4,5,5,6,4,6,5,4,6,7,5,4,6,4),
  # `59GQQ` = c(0,3,1,4,2,6,6,5,5,3,5,4,8,9,7,3,6,8),
  # `60GTU` = c(0,2,1,5,4,5,5,6,4,6,5,4,6,7,5,4,6,4),
  # `60GTV` = c(0,3,1,4,2,6,6,5,5,3,6,4,8,9,7,3,6,8)
) %>% tidyr::gather(property, count, -month)

event_data = tibble(
  month = as.Date(c('23/06/2015', '7/3/2017', '14/11/2016'), format = '%d/%m/%Y'),
  label = c('Sentinel-2A<br>Launch', 'Sentinel-2B<br>Launch', 'Kaik&#333;ura<br>Earthquake'),
  # icon = c('f7bf', 'f7bf','f6f1'),
  color = c('#1375bd','#254e00','grey50'),
  face = c('plain','plain','bold'),
  height = c(35,35,45)
)

scenes_plot = ggplot() +
  geom_segment(data = event_data, aes(x = month, xend = month, y = 0, yend = height, linetype = face, color = color)) +
  geom_point(data = event_data, aes(x = month, y = height, color = color), size = 4) +
  geom_richtext(
    data = event_data, aes(x = month, y = height, label = label, color = color, fontface = face), 
    size = 4, nudge_y = 5.5, fill = NA, label.color = NA
  ) +
  geom_col(data = mosaic_data, aes(x = month, y = count, fill = property)) + 
  xlab('') + ylab('Number of Sentinel-2 scenes') +
  scale_fill_manual(values = event_data$color) +
  scale_color_manual(values = event_data$color) +
  scale_linetype_manual(values = c('solid','dashed')) +
  # scale_fill_viridis_d('MGRS Tile', guide = guide_legend(label.position = 'bottom', nrow = 1)) +
  scale_x_date(date_breaks = '3 month',  date_labels = '%m-%Y') +
  scale_y_continuous(position = 'left', expand = c(0,0), limits = c(0,55)) +
  theme(
    axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10), size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = 'none', 
    text = element_text(family = 'sans', size = 12), 
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = 'grey85', linetype = 'dotted', size = 0.35),
    panel.border = element_rect(color = 'grey50', fill = 'transparent')
  )

ggsave('EGU2020/s2_scenes.png', scenes_plot, height = 8.5, width = 33, units = 'cm', dpi = 300)


files = list.files('ref_data', pattern = 'dams.shp', full.names = T)
dams = do.call(rbind, lapply(files, st_read)) %>% 
  filter(OBJECTID %in% c(34,46,81)) %>% 
  mutate(names = c('Hapuku\nRiver', 'Leader\nRiver', 'Stanton\nRiver'))
sta <- st_read('ref_data/North_Canterbury_AOI_2193.shp', quiet = T)
nz_outlines <- st_read('ref_data/nz-coastlines-and-islands-polygons-topo-150k.shp' , quiet = T) %>% 
  mutate(area = st_area(geometry)) %>% 
  top_n(1, area) %>% st_transform(st_crs(dams)) %>% 
  st_crop(st_bbox(st_buffer(dams, 7000))) 

cases_plot = ggplot() +
  geom_sf(data = nz_outlines, fill = '#d5dbc4', color = NA, alpha = 0.5) +
  # geom_sf(data = sta, fill = 'grey70', alpha = 0.5, color = NA) +
  geom_sf(data = dams, aes(color = names), size = 2) +
  geom_sf_text_repel(data = dams, aes(label = names, color = names), vjust = -0.3, size = 4) +
  scale_color_manual(values = c('#254e00','#1375bd','grey50')) +
  # scale_y_continuous(breaks = c(-42.3, -42.4, -42.5, -42.6, -42.7)) +
  # scale_x_continuous(breaks = c(173.2, 173.4, 173.6)) +
  coord_sf(crs = 2193, label_graticule = 'NW', expand = F) +
  # theme_void() +
  theme(
    text = element_text(size = 9),
    legend.position = 'none', 
    panel.grid = element_line(color = 'grey85', linetype = 'dotted', size = 0.3),
    panel.background = element_rect(fill = "transparent"), 
    axis.text = element_text(color = 'grey65'),
    axis.ticks = element_line(color = 'grey85', size = 0.3),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.border = element_rect(fill = NA, color = 'transparent')
  )

ggsave('EGU2020/cases_loc.png', cases_plot, height = 10, width = 10, units = 'cm', dpi = 300)

cyclones = data.frame(
  cyc = c('DEBBIE', 'COOK','GITA'),
  date = as.Date(c('2017-04-04','2017-04-13','2018-02-20'))
)
files_ts = list.files('time-series', pattern = '.csv', full.names = T) 
ts = do.call(
  rbind, 
  lapply(
    files_ts, 
    function(x) cbind(read.csv(x, na.strings = '', dec = '.', stringsAsFactors = F), name=strsplit(strsplit(x,'\\/')[[1]][2], '\\.')[[1]][1])
  )
) %>% 
  mutate(
    date = as.Date(system.time_start, format = '%b %d, %Y'),
    connArea = as.numeric(gsub(",", "", connArea))/10000,
    month = yearmonth(format(date, '%Y-%m')),
    dam = c(ifelse(name == 'hapuku', 'Hapuku River',ifelse(name == 'lower-leader-rebekah', 'Leader River', 'Stanton River')))
  ) %>% 
  group_by(dam, month) %>% 
  summarize(connArea = sum(connArea, na.rm = T)) %>% 
  filter(
    month != yearmonth('2015-12-01')
  ) %>% as_tsibble(key = dam, index = month) %>% fill_gaps()



ts_plot = ggplot(ts, aes(x = month, y = connArea, color = dam, fill = dam)) +
  geom_point(size = 2) + geom_area(alpha = 0.5, color = NA) +
  scale_x_date(date_breaks = '4 month',  date_labels = '%m-%Y') +
  scale_y_continuous(position = 'left',labels = function (x) sprintf("%.1f", x)) +
  scale_color_manual(values = c('#254e00','#1375bd','grey50')) +
  scale_fill_manual(values = c('#254e00','#1375bd','grey50')) +
  xlab('') + ylab('Lake area (ha)') +
  facet_wrap(~dam, ncol = 1, scales = 'free_y') +
  theme(
    legend.position = 'none', axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.background = element_rect(fill = 'grey95', color = 'grey50'),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = 'grey85', linetype = 'dotted', size = 0.35),
    panel.border = element_rect(color = 'grey50', fill = 'transparent')
  )
  # geom_vline(data = cyclones, aes(xintercept = date)) 


ggsave('EGU2020/cases_ts2.png', ts_plot, height = 15.6, width = 12, units = 'cm', dpi = 300)

library(tidyverse)
library(sf)
sta = st_read("ref_data/North_Canterbury_AOI_grid_WGS84.shp")
landslides = read.csv('ref_data/LANDSLIDES.csv')
landslides_sf = landslides %>% 
  filter(X.Co.ordinate != -9999, Y.Co.ordinate != -9999) %>% 
  st_as_sf(coords = c('X.Co.ordinate', 'Y.Co.ordinate')) %>% 
  st_set_crs(4326)
landslides_kaikoura = landslides_sf %>% st_intersection(sta)
landslides_sf = landslides_sf %>% mutate(study_area = ifelse(Landslide.ID %in% landslides_kaikoura$Landslide.ID, 'Yes', 'No'))
require(reshape2)
landslides_sf %>% dcast(Size.Category~study_area)
landslides_sf %>% dcast(Trigger~study_area)
data = st_read('ref_data/earthquakes.geojson')
data_sta = st_intersection(data,sta)

library(plotly)
plot = data_sta %>% filter(magnitude >= 5) %>% 
  ggplot() + 
  geom_point(aes(x = origintime, y = depth, size = magnitude), alpha = 0.5) +
  scale_y_reverse( lim=c(100,0))
plot %>% ggplotly()

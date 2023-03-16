library(sf)
library(tidyverse)
lulc = st_read('ref_data/lris-lcdb-v50-land-cover-database-version-50-mainland-new-zealand-SHP/lcdb-v50-land-cover-database-version-50-mainland-new-zealand.shp')
val_areas = st_read('validation/validation_areas.geojson', quiet = T) %>% 
  st_transform(crs = st_crs(lulc)) %>%
  filter(id %in% c(0,3))
t = st_intersection(lulc, val_areas) %>% 
  mutate(higher_class = case_when(
    Class_2018 %in% c(10:19) ~ "Bare or Lightlyvegetated Surfaces",
    Class_2018 %in% c(20:29) ~ "Water Bodies",
    Class_2018 %in% c(30:39) ~ "Cropland",
    Class_2018 %in% c(40:49) ~ "Grassland, Sedgeland and Marshland",
    Class_2018 %in% c(50:59, 80:89) ~ "Scrub and Shrubland",
    Class_2018 %in% c(60:79) ~ "Forest",
    TRUE ~ "Other"
  ))

pal = c("grey", "yellow", "green4", "springgreen", "deepskyblue")

tm_shape(t) +
  # tm_fill(col = "Name_2018") +
  tm_fill(col = "higher_class", palette = pal) +
  tm_facets(by = 'id', ncol = 1) 

t1 = t %>% 
  group_by(id) %>% 
  mutate(
    subset_area = sum(st_area(geometry))
  ) %>% 
  ungroup() %>% 
  mutate(landcv_frac = st_area(geometry)/subset_area)

t1_summary = t1 %>% 
  st_drop_geometry() %>% 
  group_by(id, Name_2018) %>% 
  summarise(landcv_perc = sum(landcv_frac)*100) %>% 
  ungroup() %>% 
  arrange(id, desc(landcv_perc))

t2_summary = t1 %>% 
  st_drop_geometry() %>% 
  group_by(id, higher_class) %>% 
  summarise(higher_perc = sum(landcv_frac)*100) %>% 
  ungroup() %>% 
  arrange(id, desc(higher_perc))

## s2tiles.kml downloaded from:
## https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-2/data-products
sta = st_read('ref_data/North_Canterbury_AOI_2193.shp', quiet = T)
s2tiles = st_read('ref_data/s2tiles.kml') %>% 
  st_collection_extract('POLYGON') %>% 
  st_filter(sta, op = st_intersects) %>% 
  select(Name) 

st_write(s2tiles, 'ref_data/s2tiles_kaikoura.geojson')
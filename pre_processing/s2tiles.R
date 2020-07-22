s2tiles = st_read('ref_data/s2tiles.kml') %>% 
  st_collection_extract('POLYGON') %>% 
  st_filter(sta, op = st_intersects) %>% 
  select(Name) 

st_write(s2tiles, 'ref_data/s2tiles_kaikoura.kml')
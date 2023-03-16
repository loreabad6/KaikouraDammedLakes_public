library(sf)
library(stars)
library(dplyr)
library(ggplot2)
lakes = read_stars('results/mapping_results-0000000000-0000000000.tif', proxy = T, NA_value = 0)
valareas = st_read('validation/validation_areas.geojson') %>% 
  # Select areas we use
  filter(id %in% c(3)) %>%
  # Project the data and calculate area
  st_transform(2193) %>% 
  st_buffer(10000)


water_percentage = function(x) sum(x, na.rm = T)/17*100
lake_zoom_sum = lakes %>%
  st_apply(c('x','y'), FUN = water_percentage) 

ggplot() +
  geom_stars(data = lake_zoom_sum, downsample = 50) +
  scale_fill_viridis_c(limits = c(1,100), na.value= "transparent") +
  theme_void()

## Follow-up: create water percentage with rgee and then import as stars object

library(rgee)
ee_Initialize()
lakes = ee$Image(
'projects/ee-loreabad6/assets/Kaikoura_landslidedammedlakes/dammedLakes_pixelwise')
ee_print(lakes)
occurrence = ee$Image(
  'projects/ee-loreabad6/assets/Kaikoura_landslidedammedlakes/dammedLakes_occurrence')
ee_print(occurrence)
# Downloading takes too long
aoi = ee$FeatureCollection('users/loreabad6/NZ/AOI/North_Canterbury_AOI_WGS84')$
  geometry()#$bounds()

occvec = occurrence$selfMask()$reduceToVectors(
  geometry = aoi,
  scale = 10, 
  crs = occurrence$projection(),
  geometryType = "centroid",
  labelProperty = "occurrence",
  maxPixels = 1e8
)
ee_print(occvec)
occ_sf = ee_as_sf(occvec, maxFeatures = 1e6)
st_write(occ_sf, 'results/occurrence_points.geojson')

##' Landslide dams inventory
##' Source: ECAN 2017
library(sf)
library(dplyr)
files_dams = list.files(path = 'ref_data', pattern = '_dams.shp', full.names = T)
landslide_dams = do.call(rbind, lapply(files_dams, st_read, quiet = T))
dams_buffer = landslide_dams %>% 
  st_buffer(200) %>% 
  select(OBJECTID_1, Formal_Nam) %>% 
  st_transform(4326) %>% 
  sf_as_ee()

occ_sf %>% st_transform(2193) %>% 
  st_filter(.predicate = st_is_within_distance(landslide_dams, 800))

Map$centerObject(occurrence)
Viz = list(min = 1, max = 17, palette = c("00FFFF", "0000FF"))
Map$addLayer(occurrence$selfMask(), Viz, 'Lake occurrence') +
  Map$addLayer(dams_buffer)

dams = landslide_dams %>% 
  st_transform(crs = 4326) %>% 
  dplyr::select(geometry) %>% 
  mutate(name = 'GNS Science\nlandslide dams', color = 'white', shape = as.factor(17))

t = st_read('results/occurrence_points.geojson')
library(sf)
library(spatstat)
library(here)
library(raster)
library(dplyr)
#' @param x sf POINT object with landslide locations
#' @param sigma sigma value for Kernel density (bandwith)
kernel_density = function(x, sigma = 600, weights = NULL) {
  p = suppressWarnings(as.ppp(x))
  d = density(p, sigma = sigma, weights = weights)
  r = raster(d, crs = crs(x)) * 1e6
  r
}
tproj = st_transform(t, 2193)
a = kernel_density(tproj, sigma = 300, weights = tproj$occurrence)
astars = st_as_stars(a) %>% 
  mutate(layer = ifelse(layer < 10, NA, layer))
plot(astars)
ggplot() +
  # geom_sf(data = dams, color = 'red', alpha = 0.7) +
  geom_sf(data = t, aes(color = occurrence), size = 0.7, alpha = 0.5) +
  # geom_stars(data = astars, na.rm = T) +
  # scale_fill_viridis_c(direction = -1, na.value = 'white') +
  scale_color_viridis_c(direction = -1, na.value = 'white') +
  # coord_sf() +
  theme_void()

  


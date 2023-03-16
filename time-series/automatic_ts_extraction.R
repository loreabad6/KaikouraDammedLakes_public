library(rgee)
ee_Initialize()
library(sf)
library(tidyverse)

lake_sum = ee$Image("projects/ee-loreabad6/assets/Kaikoura_landslidedammedlakes_update/dammedLakes_components")
# lake_occ = ee$Image("projects/ee-loreabad6/assets/Kaikoura_landslidedammedlakes_update/dammedLakes_occurrence")
# 
# lake_vec = lake_occ$reduceToVectors(
#   geometry = geometry,
#   crs = lake_occ$projection(),
#   scale = 10,
#   geometryType = 'centroid',
#   eightConnected = true,
#   labelProperty = 'occurrence'
# )
# 
# ##' Landslide dams inventory
# ##' Source: ECAN 2017
# files_dams = list.files(path = 'ref_data', pattern = '_dams.shp', full.names = T)
# dams = do.call(rbind, lapply(files_dams, st_read, quiet = T))
# dams_ts = dams %>% 
#   # Select relevant dams for the time series plot
#   dplyr::select(OBJECTID_1, Formal_Nam, Key_Dam) %>% 
#   filter(OBJECTID_1 %in% c(484,499,516,528,534,536,540,563,569,574)) %>% 
#   mutate(OBJECTID_1 = as.factor(OBJECTID_1)) %>% 
#   st_transform(4326)
# 
# points = st_sf(
#   id = 1:5, 
#   geom = st_sfc(
#     st_point(c(173.20717451368324,-42.585415633143630)),
#     st_point(c(173.67275473015735,-42.242661883067390)),
#     st_point(c(173.12675783051640,-42.609609844471570)),
#     st_point(c(173.17760271149905,-42.604094737112675)),
#     st_point(c(173.26517323611404,-42.557756952864380)),
#     crs = 4326
# ))
# 
# points_dams = st_join(points, dams_ts, join = st_nearest_feature)

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
      as.Date(format = "%Y%m%d")
  )

lake_normalized = lake_ts %>% 
  mutate(
    month = tsibble::yearmonth(format(date, '%Y-%m')),
    area = replace_na(area, 0)
  ) %>% 
  group_by(OBJECTID_1) %>% 
  tsibble::as_tsibble(
    key = c(OBJECTID_1, count, occurrence, id, distance),
    index = month
  ) %>%
  tsibble::fill_gaps() %>% 
  mutate(
    minarea = min(area, na.rm = TRUE),
    maxarea = max(area, na.rm = TRUE),
    meanarea = mean(area, na.rm = TRUE),
  ) %>%
  rowwise() %>% 
  mutate(
    area_norm = (area - minarea) / (maxarea - minarea)
  ) %>% 
  ungroup() %>% 
  mutate(
    OBJECTID_1 = fct_reorder(OBJECTID_1, .x = maxarea, .fun = min)
  ) %>% 
  select(-c(minarea, maxarea, dateid))

# From: https://stackoverflow.com/a/56763530/12118669
gaps = lake_normalized %>%
  filter(is.na(lead(area_norm)) & row_number() != n() |
           is.na(lag(area_norm)) & row_number() != 1) %>%
  filter(!(is.na(area_norm) & (lag(is.na(area_norm)) | lead(is.na(area_norm))))) %>% 
  mutate(group = cumsum(row_number() %% 2))

sampleIds = levels(lake_normalized$OBJECTID_1)[1:64]
sampleIds = levels(lake_normalized$OBJECTID_1)[65:124]

ggplot(filter(lake_normalized, OBJECTID_1 %in% sampleIds)) +
  aes(x = month, y = area) +
  geom_point() + geom_line() +
  geom_line(
    data = filter(gaps, OBJECTID_1 %in% sampleIds),
    aes(group = group),
    linetype = "dashed"
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  tsibble::scale_x_yearmonth(date_breaks = '2 years',  date_labels = '%m-%Y') +
  facet_wrap(~OBJECTID_1, scales = 'free_y', ncol = 8)

mapview::mapview(filter(dams, OBJECTID_1 == 563))

lake_classified = lake_normalized %>% 
  group_by(OBJECTID_1) %>% 
  mutate(
    class = case_when(
      (is.null(pracma::findpeaks(area_norm[!is.na(area_norm)]))
       ) &
      (first(area_norm) >= 0.5 |
         nth(area_norm, n = 2L) >= 0.5 |
         nth(area_norm, n = 3L) >= 0.5 ) &
        nth(area_norm, n = -2L) <= 0.2 &
        last(area_norm) <= 0.2 ~ 'decreasing',
      first(area_norm) <= 0.2 &
        nth(area_norm, n = 2L) <= 0.2 &
        nth(area_norm, n = 3L) <= 0.3 &
        last(area_norm) >= 0.5 ~ 'increasing',
      TRUE ~ NA_character_
  )) %>% 
  ungroup()
gaps = lake_classified %>%
  filter(is.na(lead(area_norm)) & row_number() != n() |
           is.na(lag(area_norm)) & row_number() != 1) %>%
  filter(!(is.na(area_norm) & (lag(is.na(area_norm)) | 
                                 lead(is.na(area_norm))))) %>% 
  mutate(group = cumsum(row_number() %% 2))

lake_classified %>% 
  # filter(class == "decreasing") %>%
  # filter(is.na(class)) %>%
  ggplot() +
  aes(x = month, y = area, group = OBJECTID_1) +
  geom_point() + geom_line() +
  geom_line(
    data = gaps,
    # data = filter(gaps, class == "decreasing"),
    # data = filter(gaps, is.na(class)),
    aes(group = group),
    linetype = 'dashed'
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  tsibble::scale_x_yearmonth(date_breaks = '2 years',  date_labels = '%m-%Y') +
  # facet_wrap(~ OBJECTID_1, scales = 'free_y')
  facet_wrap(~ class, scales = 'free_y')
library(pracma)
findpeaks(list$`1072`[!is.na(list$`1072`)])

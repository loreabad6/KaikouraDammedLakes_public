library(readxl)
gepval = read_excel("DetectedDams.csv") %>% 
  dplyr::select(-`Detected Pixel`, -`Detected Vector`, -`Check on GEE`)
name_based = gepval %>% 
  filter(!is.na(Name)) %>% 
  dplyr::select(-Formal_Nam) %>% 
  left_join(dams_or %>% dplyr::select(NAME, Formal_Nam, Key_Dam, OBJECTID_1), by = c('Name' = 'NAME'))
formal_name_based = gepval %>% 
  filter(!is.na(Formal_Nam)) %>% 
  dplyr::select(-Name) %>% 
  left_join(dams_or %>% dplyr::select(NAME, Formal_Nam, Key_Dam, OBJECTID_1))
gepval_withid = bind_rows(name_based, formal_name_based) %>% 
  filter(is.na(NAME)) %>% 
  dplyr::select(-NAME)


detected = read.table('results/detected_lakes_objectid.txt', sep = ':') %>% 
  rename(OBJECTID_1 = V2) %>% dplyr::select(OBJECTID_1) %>% 
  mutate(detected = 1)

compare = gepval_withid %>% left_join(detected)

compare %>% filter(detected == 1) %>% pull(`Visible Lake on GEP?`) %>% table()
compare %>% filter(detected == 1) %>% pull(Area) %>% table()

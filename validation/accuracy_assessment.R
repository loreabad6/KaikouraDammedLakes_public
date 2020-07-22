library(stars)
library(dplyr)
lakes = read_stars('../results/mapping_results-0000000000-0000000000.tif', proxy = T)
error_matrix = function(subset){
  val_areas = st_read('validation/validation_areas.geojson', quiet = T) %>% 
    st_transform(crs = st_crs(lakes)) 
  lakes = lakes[,,,c(2,9,13)][val_areas %>% filter(id == subset)] %>% 
    st_as_stars() %>% 
    st_set_dimensions(names = c('x','y','month'))
  pixels_lakes = lakes %>% st_apply('month', sum, na.rm = T) %>% as.data.frame()
  
  files = list.files('validation', pattern = 'validation_data', full.names = T)
  val = do.call(
    'rbind', 
    lapply(
      files, 
      function(x) mutate(
        st_read(x, quiet = T), 
        year =  strsplit(strsplit(x,'\\/')[[1]][2], '\\.')[[1]][1]
      )
    )
  ) %>% st_transform(crs = st_crs(lakes)) %>% st_join(val_areas, left = F)
  
  lakes_skeleton = lakes[,,,1, drop = T] %>% 
    dplyr::select(results = mapping_results.0000000000.0000000000.tif) %>%
    mutate(results = ifelse(results == 1, 0, results))
  val_2016 = val %>% 
    filter(id.y == subset, stringr::str_detect(year, '2016')) %>% 
    st_rasterize(template = lakes_skeleton, options = "ALL_TOUCHED=FALSE") 
  val_2018 = val %>% 
    filter(id.y == subset, stringr::str_detect(year, '2018')) %>% 
    st_rasterize(template = lakes_skeleton, options = "ALL_TOUCHED=FALSE")
  val_2019 = val %>% 
    filter(id.y == subset, stringr::str_detect(year, '2019')) %>% 
    st_rasterize(template = lakes_skeleton, options = "ALL_TOUCHED=FALSE") 
  
  val = c(val_2016, val_2018, val_2019, along = 'month')
  pixels_val = val %>% st_apply('month', sum, na.rm = T) %>% as.data.frame()
  
  cm2016 = c(val[,,,1], lakes[,,,1], along = 3) %>% 
    as_tibble() %>% 
    mutate(status = ifelse(month == min(month), 'actual', 'predicted')) %>% 
    tidyr::pivot_wider(id_cols = c(x,y), names_from = status, values_from = lakes) %>% 
    tidyr::drop_na() %>% 
    dplyr::select(-x,-y) %>% 
    table() %>%
    as.matrix()
  
  cm2018 = c(val[,,,2], lakes[,,,2], along = 3) %>% 
    as_tibble() %>% 
    mutate(status = ifelse(month == min(month), 'actual', 'predicted')) %>% 
    tidyr::pivot_wider(id_cols = c(x,y), names_from = status, values_from = lakes) %>% 
    tidyr::drop_na() %>% 
    dplyr::select(-x,-y) %>% 
    table() %>%
    as.matrix()
  
  cm2019 = c(val[,,,3], lakes[,,,3], along = 3) %>% 
    as_tibble() %>% 
    mutate(status = ifelse(month == min(month), 'actual', 'predicted')) %>% 
    tidyr::pivot_wider(id_cols = c(x,y), names_from = status, values_from = lakes) %>% 
    tidyr::drop_na() %>% 
    dplyr::select(-x,-y) %>% 
    table() %>%
    as.matrix()
  
  res = list(cm2016, cm2018, cm2019)
  periods = c('2016', '2018', '2019')
  names(res) = sapply(periods, function(x) paste0('a', subset, '_', x))
  res
}

accuracy_assessment = function(cm){
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  oa = sum(diag) / n * 100
  
  prod_acc = diag / colsums * 100
  cons_acc = diag / rowsums * 100
  
  sign = binom.test(x = sum(diag),
                    n = n,
                    alternative = c("two.sided"),
                    conf.level = 0.95
  )
  
  pvalue = sign$p.value
  CI95 = sign$conf.int[1:2]
  No = sum(diag)
  Ne = 1 / n * sum(colSums(cm) * rowSums(cm))
  kappa = (No - Ne) / (n - Ne)
  
  cm_ext = addmargins(cm) %>% 
    rbind("users" = c(prod_acc, NA)) %>% 
    cbind("producers" = c(cons_acc, NA, oa)) %>% 
    round(digits = 3)
  
  colnames(cm_ext) = c('No Lake', 'Lake', 'Sum', 'PA')
  rownames(cm_ext) = c('No Lake', 'Lake', 'Sum', 'CA')
  
  dimnames(cm_ext) = list("Prediction" = colnames(cm_ext),
                               "Reference" = rownames(cm_ext))
  
  actual_lake_pixels = rowsums[2]
  predicted_lake_pixels = colsums[2]
  correctly_predicted_lake_pixels = diag[2]
  class(cm_ext) = 'table'
  res = list(
    cm_ext, pvalue, CI95, kappa, 
    actual_lake_pixels, predicted_lake_pixels, correctly_predicted_lake_pixels
  )
  names(res) = c(
    'Accuracy Table', 'P-value', 'Conf. Interval', 'Kappa coefficient', 
    'Actual lake pixels', 'Predicted lake pixels', 'Correctly detected lake pixels')
  res
}

cm = c(error_matrix(0), error_matrix(3))

acc_list = lapply(cm, accuracy_assessment)

library(tidyr)
library(dplyr)
acc_results = data.frame(
  pa = formatC(sapply(acc_list, function(x) x$`Accuracy Table`[4,2]), format = 'f', digits = 2),
  ca = formatC(sapply(acc_list, function(x) x$`Accuracy Table`[2,4]), format = 'f', digits = 2),
  # oa = sapply(acc_list, function(x) x$`Accuracy Table`[4,4]),
  kp = formatC(sapply(acc_list, function(x) x$`Kappa coefficient`), format = 'f', digits = 3),
  alp = formatC(sapply(acc_list, function(x) x$`Actual lake pixels`), format = 'd', digits = 0),
  plp = formatC(sapply(acc_list, function(x) x$`Predicted lake pixels`), format = 'd', digits = 0),
  clp = formatC(sapply(acc_list, function(x) x$`Correctly detected lake pixels`), format = 'd', digits = 0)
  # dt = c(rep('December 2016', 3), rep('March 2018', 3), rep('January 2019', 3)), #, rep('Overall',3)),
  # ar = c(rep(c('Subset A', 'Subset B', 'Subset C'), 3))
) %>% 
  tibble::rownames_to_column() %>% 
  tidyr::separate(rowname, c('ar', 'dt'), '_') %>%
  mutate(
    ar = ifelse(ar == 'a0', 'Subset A', 'Subset B'),
    dt = ifelse(dt == 2016, 'December 2016', ifelse(dt == 2018, 'March 2018', 'January 2019'))
  ) %>% 
  pivot_longer(cols = c(pa,ca,kp,alp,plp,clp), names_to = 'am') %>% 
  mutate(am = factor(am, levels = c('alp','plp','clp','pa','ca','kp', ordered = T))) %>% 
  pivot_wider(id_cols = c(am,ar), names_from = dt, values_from = value) %>%
  arrange(am) 

save(acc_results, file = "accuracy_assessment.Rda")

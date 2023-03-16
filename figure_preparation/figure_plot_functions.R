#' Plot study area with inset map 
#' 
#' @param aoi area of interest to show on the inset map, and where the zoom map will zoom into. 
#' @param country character vector with countr name, to show inset map relative to whole country. 
#' It takes the country outline from \code{rnaturalearth}
#' @param extra_outline if country is not used, what should be the outline to locate the study area, e.g. coastlines
#' @param zoom_background \code{stars} object to be used as a background for the zoom map, 
#' i.e. satellite imagery or DEM
#' @param hillshade_effect (optional) \code{stars} object with a calculated hillshade. 
#' Used generally with the DEM, it plots the hillshade on top with a grey scale and low alpha. 
#' @param sa_color fill color for the study area on the inset map, and outline color on the zoom map.
#' @param font_family font for the whole plot. Default 'serif'
#' @param font_size font size for the axis labels, legend, etc. Default 9 pts
#' @param extra_feature extra \code{sf} feature to add to the map
#' @param extra_shape shape aesthetic, mainly if extra_feature is a \code{POINT} geometry
#' @param extra_legend include extra elements in the legend corresponding to the extra features
#' @param extra_color color for the extra_feature. Default 'red'
#' @param extra_size size for extra_feature on map, mainly used with \code{POINT} geometry. Default 7
#' @param extra_label label for the extra feature to appear on the map
#' @param extra_font_size font size for the extra_label of extra_feature on the map, relative to map size. Default 2.5
#' 
#' @return study area plot with extra features, customized bacground and inset map for location

sa_plot = function(
  aoi, country, extra_outline = NULL, proj_crs, zoom_background = NULL, 
  hillshade_effect = NULL, sa_color = 'red', 
  font_family = 'serif', font_size = 9, 
  extra_feature = NULL, extra_shape = NULL, 
  extra_legend = NULL, extra_color = 'red', extra_size = 7, 
  extra_label = '', extra_font_size = 2.5
){
  
  # Required libraries #REVIEW WHAT IS REALLY NEEDED
  # library(paletteer)
  
  # World countries
  sa_country = if (is.null(extra_outline)) {
    ne_countries(country = country, scale = 'large', returnclass = "sf") %>% 
      st_cast("POLYGON") %>%
      mutate(area = st_area(geometry)) %>%
      top_n(3, area)
  } else { extra_outline }
  
  # sa_country_bg = get_stamenmap(
  #   as.vector(st_bbox(aoi)), maptype = 'terrain-background', color = 'bw', crop = T, force = T, zoom = ggmap_zoom
  #   ) %>%
  #   ggmap()
  
  mainplot = ggplot() + 
    geom_sf(data = sa_country, fill = '#d5dbc4', color = NA) +
    geom_sf(data = aoi, fill = sa_color, color = NA, alpha = 0.5) +
    coord_sf(crs = proj_crs) +
    theme_void() +
    theme(
      panel.border = element_rect(fill = NA, color = 'grey50')
    )
  
  zoom_country = sa_country %>% st_crop(st_bbox(st_buffer(aoi, 0.1))) 

  aoiplot = ggplot() +
    geom_sf(data = zoom_country, fill = '#d5dbc4', color = NA) +
    geom_stars(data = zoom_background, downsample = 15) +
    scico::scale_fill_scico('Elevation (m a.s.l.)', begin = 0.5, palette = 'oleron', na.value = 'transparent')
  
  if(!is.null(hillshade_effect)){
    aoiplot = aoiplot + 
      new_scale_fill() +
      geom_stars(data = hillshade_effect, downsample = 15, alpha = 0.25) +
      scale_fill_gradientn(colours = grey.colors(100, start = 0, end=.95), guide='none', na.value = NA) 
  }
  aoiplot = aoiplot +
    geom_sf(data = aoi, color = sa_color, fill = NA, size = 1) 
  if(!is.null(extra_feature)){
    extra_scales = extra_feature %>% st_drop_geometry() %>% dplyr::distinct() 
    extra_labels = extra_scales %>% pull(extra_legend) 
    extra_shapes = extra_scales %>% pull(extra_shape) 
    aoiplot = aoiplot +
      geom_sf(data = extra_feature, aes_string(color = extra_shape, shape = extra_shape, size = extra_shape)) +
      scale_color_manual(name = '', labels = extra_labels, values = c('white','red')) +
      scale_shape_manual(name = '', labels = extra_labels, values = c(20,18)) +
      scale_size_manual(name = '', labels = extra_labels, values = c(0.5, 3)) +
      geom_sf_text(data = extra_feature, aes(label = extra_label), nudge_y = 9000, size = extra_font_size, family = font_family, ) #fontface = 'bold'
  }
  aoiplot = aoiplot +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_sf(crs = proj_crs, label_graticule = 'NW') +
    annotation_scale(
      location = "br", width_hint = 0.2, style = 'bar', height = unit(0.08, "cm"),
      text_family = font_family, text_cex = 0.5, line_width = 0.5, pad_x = unit(1.7, "cm"),
      pad_y = unit(0.2, "cm"), text_pad = unit(0.1, "cm")
    ) +
    guides(shape = guide_legend(override.aes = list(size = 3))) +
    theme(
      legend.key.size = unit(0.35, "cm"),
      legend.title = element_text(size = 8),
      legend.key = element_blank(),
      legend.position = c(0.25,0.8), 
      legend.spacing = unit(0,'cm'),
      legend.background = element_blank(),
      text = element_text(family = font_family, size = font_size),
      panel.background = element_rect(fill = "transparent"), 
      axis.title = element_blank(),
      axis.text.y = element_text(angle = 90, hjust = 0.5), 
      panel.border = element_rect(fill = NA, color = 'transparent')
    )

  ggdraw(xlim = c(0, 23.5), ylim = c(0, 10)) +
    draw_plot(aoiplot, x = 0, y = 0, width = 20, height = 10) +
    draw_plot(mainplot, x = 16, y = 0, width = 6, height = 6)
    # geom_segment(aes(x = 7, y = y1, xend = x2, yend = y2), data = arrowA,
    #              arrow = arrow(), lineend = "round")
    # geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
    #              arrow = arrow(), lineend = "round")
}


#' Plot lake occurrence
#' 
#' @param dams `sf` object where landslide dams locations are stored, preferably projected
#' @param landslide_dam_id Integer corresponding to the OBJECTID_1 column on the 
#' landslide dam inventory
#' @param nudge_x x shift of buffer center, defaults to 0 which then puts the landslide dam on the center
#' @param nudge_y same as nudge_x but for the y coordinate
#' @param zoom_buffer how large should the buffer around the selected dam be. The buffer is
#' called for the projected dams. Defaults to 1000 m in a square representation. 
#' @param position legend position for the plot, defaults to 'none'. 
#' @param background should a sentinel2 RGB thumbnail be used as background? Defaults to FALSE
#' Note: Will get rearranged by patchwork. 
#' @param no_postevents how many post-event dates are processed? Used to calculate water percentage
#' 
#' @return map of selected landslide dam with the lake occurrence aggregated for the study periods.
 
plot_lake_occurrence = function(
  dams, landslide_dam_id, nudge_x = 0, nudge_y = 0,
  zoom_buffer = 1000, position = 'none', background = FALSE,
  background_custom = NULL, bg_downsample = 0,
  no_postevents = 19, scale_text_cex = 0.5, 
  scale_height = unit(0.08, "cm"), scale_width_hint = 0.2,
  legend_title_size = 9, legend_text_size = 9
  ) {
  dam_point = dams %>% dplyr::filter(OBJECTID_1 == landslide_dam_id) 
  dam_buffer = dam_point 
  st_geometry(dam_buffer) = st_sfc(st_geometry(dam_buffer)[[1]] +
                                     sf::st_point(c(nudge_x, nudge_y)), crs = 2193)
  lake_zoom_buffer = dam_buffer %>% 
    st_buffer(zoom_buffer, endCapStyle = 'SQUARE')
  
  if(background & is.null(background_custom)){
    lake_zoom_gee = ee$Geometry$Rectangle(
      coords = as.vector(st_bbox(st_transform(lake_zoom_buffer, 4326))),
      proj = "EPSG:4326",
      geodesic = FALSE
    )
    
    bg = ee$ImageCollection("COPERNICUS/S2")$
      filterDate(ee$Date('2016-12-01'), ee$Date('2016-12-31'))$
      filterBounds(lake_zoom_gee)$
      sort("CLOUDY_PIXEL_PERCENTAGE", TRUE)$
      select(c("B11","B8", "B4", "B3", "B2"))$  
      first()
    
    bg_tc = ee_as_thumbnail(
      bg$select(c("B4", "B3", "B2")), 
      region = lake_zoom_gee, dimensions = 2048, 
      vizparams = list(min = 0, max = 6000, gamma = 2.5)
    ) %>% st_set_crs(4326)
  }
  
  lake_zoom = lakesproj[lake_zoom_buffer] %>%
    st_as_stars() %>%
    st_set_dimensions(names = c('x','y','month'))

  water_percentage = function(x) sum(x, na.rm = T) / no_postevents * 100
  lake_zoom_sum = lake_zoom %>%
    st_apply(c('x','y'), FUN = water_percentage) %>%
    na_if(0)
    
  g = ggplot()
  
  if(background & is.null(background_custom)) {
      g = g + layer_spatial(data = bg_tc, alpha = 1)
  }
  
  if(background & !is.null(background_custom)) {
    bg_tc = background_custom[lake_zoom_buffer]
    bg_tc = st_rgb(
      st_as_stars(bg_tc)[,,,1:3],
      dimension = 3,
      maxColorValue = 250,
      use_alpha = FALSE, 
      probs = c(0.02, 0.98), #ignored when histogram equalizer stretch defined
      stretch = TRUE)
      
    g = g + 
      geom_stars(data = bg_tc, alpha = 0.85,
                 show.legend = FALSE, downsample = bg_downsample) +
      scale_fill_identity() +
      ggnewscale::new_scale_fill()
  }
  g +
    geom_stars(data = lake_zoom_sum) +
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
    scico::scale_fill_scico(
      'Water occurrence (%)', limits = c(0,100),
      palette = 'devon', na.value = NA, direction = -1,
      guide = guide_colorbar(title.position = 'top')
    ) +
    geom_sf(data = dam_point, size = 2, shape = 18, aes(col = as.factor(Key_Dam))) +
    scale_color_manual("", labels = c('GNS landslide dam', 'GNS landslide dam'),
                       values = c('red', 'red')) +
    coord_sf(crs = 2193, label_graticule = 'NE') +
    annotation_scale(
      location = "tl", width_hint = scale_width_hint,
      style = 'bar', height = scale_height,
      text_cex = scale_text_cex, line_width = 0.5,
      pad_y = unit(0.2, "cm"), pad_x = unit(0.2, "cm"),  
      text_pad = unit(0.1, "cm")
    ) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key = element_blank(),
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      legend.position = position, 
      legend.background = element_blank(),
      panel.background = element_rect(fill = "grey95"), 
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.border = element_blank()
    )
}

#' Plot time series of lake surface area evolution
#' 
#' @param ts_data data obtained manually from GEE. See fig_generation.R for details.
#' 
#' @return time series plot with facets per lake organized in 1 column.
#' Having a function to generate this, allows to create two separate plots when they are too
#' large to fit on one single page.
ts_plot = function(ts_data, label_size = 2.5, 
                   legend_title_size = 8,
                   legend_text_size = 8, label = TRUE,
                   date_breaks_months = '4 month') {
  # Identify data gaps to plot dashed line in between
  # From: https://stackoverflow.com/a/56763530/12118669
  gaps = ts_data %>%
    filter(is.na(lead(area)) & row_number() != n() |
             is.na(lag(area)) & row_number() != 1) %>%
    filter(!(is.na(area) & (lag(is.na(area)) | lead(is.na(area))))) %>% 
    mutate(group = cumsum(row_number() %% 2))
  
  g = ggplot(ts_data, aes(x = month, y = area, color = area)) +
    # geom_area(alpha = 0.5, color = NA, fill = 'grey80') + 
    geom_line(data = gaps, color = 'grey70', aes(group = group), linetype = "dashed") +
    # new_scale_color() +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_yearmonth(date_breaks = date_breaks_months,  date_labels = '%m-%Y') +
    scale_y_continuous(
      position = 'left', expand = expansion(mult = c(0, 0.1), add = c(0,0.55)), 
      labels = function (x) sprintf("%.1f", x), limits = c(0,NA)
    ) +
    scico::scale_color_scico(
      "Lake area (ha)", palette = 'batlow', direction = -1, limits = c(0,10.3)
      # guide = guide_colorbar(title.position = 'top', title.theme = element_text(size = legend_title_size))
    ) +
    xlab('') + ylab('Lake area (ha)') +
    facet_wrap(~label, ncol = 1, scales = 'free_y', strip.position = 'left') +
    theme(
      legend.title = element_text(size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      legend.position = 'right', axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
      strip.background = element_blank(),
      strip.text = element_blank(),
      # strip.background = element_rect(fill = 'grey95', color = 'grey50'),
      axis.ticks = element_line(color = 'grey50', size = 0.3),
      panel.background = element_rect(fill = 'white'),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = 'grey90', size = 0.2),
      panel.border = element_rect(color = 'grey50', fill = 'transparent')
    )
  
  if(label) {
    g +
      geom_text(
        aes(label = label), size = label_size,
        x = Inf,
        y = Inf,
        color = 'black', check_overlap = T, hjust = 1.1, vjust = 1.5
      ) 
  } else g
} 

#' Build the time series plot combined with lake occurrence plot
#' 
#' @param pre-arranged patch
#' 
#' @return a facet plot to the left with time series of lake area evolution, 
#' matched with the lake occurrence plot to its right. The legends are combined on the 
#' top of the plot.

plot_build = function(patch_construct, legend_title_size = 8, 
                      legend_text_size = 8) {
  (guide_area() / (patch_construct + plot_layout(widths = c(0.65,0.35)))) +
    plot_layout(guides = 'collect', height = c(0.05,0.95)) & 
    theme(
      legend.direction = 'horizontal',
      legend.title = element_text(size = legend_title_size),
      legend.title.align = 0.5, legend.box = 'horizontal',
      legend.text = element_text(size = legend_text_size), 
      legend.key.height = unit(2.5, 'mm')
      # legend.position = 'top'
    )
}

#' Plot lake detection results
#' 
#' @param landslide_dam_id Integere corresponding to the OBJECTID_1 column on the 
#' landslide dam inventory
#' @param zoom_buffer how large should the buffer around the selected dam be. The buffer is
#' called for the projected dams. Defaults to 1000 m in a square representation.
#' @param downsample how to downsample the stars object considering plotting time
#' 
#' @return facet map zooming into a selected landslide dam, showing an RGB with the mapped
#' lakes for the first facet, a false color RGB for the second and MNDWI for the third facet.

plot_lake_detection = function(landslide_dam_id, zoom_buffer = 1000, downsample = 5) {
  dam_point = dams %>% dplyr::filter(OBJECTID_1 == landslide_dam_id) %>% 
    mutate(
      coord_x = paste(gsub(' ', '\u00B0', 
                           measurements::conv_unit(
                             round(st_coordinates(st_transform(geometry, 4326))[,1], 2),
                             from = 'dec_deg', 
                             to = 'deg_dec_min'
                           )), 'E'),
      coord_y = paste(
        gsub('-','',
             gsub(' ', '\u00B0', 
                  measurements::conv_unit(
                    round(st_coordinates(st_transform(geometry, 4326))[,2], 2),
                    from = 'dec_deg', 
                    to = 'deg_dec_min'
                  ))), 'S')
    ) %>% 
    mutate(coords = paste(coord_x, coord_y, sep = ", "))
  
  lake_zoom_buffer = dam_point %>% 
    st_buffer(zoom_buffer, endCapStyle = 'SQUARE')

  lake_zoom_gee = ee$Geometry$Rectangle(
    coords = as.vector(st_bbox(st_transform(lake_zoom_buffer, 4326))),
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
  bg = ee$ImageCollection("COPERNICUS/S2")$
    filterDate(ee$Date('2016-12-01'), ee$Date('2016-12-31'))$
    filterBounds(lake_zoom_gee)$
    sort("CLOUDY_PIXEL_PERCENTAGE", TRUE)$
    select(c("B11","B8", "B4", "B3", "B2"))$  
    first()
  
  mndwi = bg$normalizedDifference(c("B3", "B11"))$rename('MNDWI')
  
  # bg_crs = ee_as_stars(bg$select(c("B4","B3")), region = lake_zoom_gee, scale = 100)
  bg_tc = ee_as_thumbnail(
    bg$select(c("B4", "B3", "B2")), 
    region = lake_zoom_gee, dimensions = 2048, 
    vizparams = list(min = 0, max = 6000, gamma = 1.2)
  ) %>% st_set_crs(4326)
  bg_fc = ee_as_thumbnail(
    bg$select(c("B8", "B4", "B3")), 
    region = lake_zoom_gee, dimensions = 2048, 
    vizparams = list(min = 0, max = 6000, gamma = 1.2)
  ) %>% st_set_crs(4326)
  bg_ix = ee_as_thumbnail(
    mndwi, 
    region = lake_zoom_gee, dimensions = 2048,
    vizparams = list(min = -1, max = 1)
  ) %>% st_set_crs(4326)
  lake_dec2016 = lakes[,,,4][st_transform(lake_zoom_buffer, 4326)] %>%
    st_as_stars() %>% 
    st_as_sf(as_points = F, merge = F, na.rm = T) %>% 
    filter(lake201612 == 1) %>% 
    st_union() %>% 
    st_transform(crs = st_crs(bg_tc))
  
  dam_points = dams %>% 
    st_intersection(lake_zoom_buffer) %>% 
    st_transform(crs = st_crs(bg_tc)) %>%
    st_geometry()
  
  par(mfrow = c(1,3), mar = c(0,0,0,0), bg = NA, font.main = 1, cex.main = 0.95)
  plot(bg_tc, rgb = 1:3, main = NA, key.pos = NULL, reset = F) 
  plot(lake_dec2016, border = NA, col = rgb(0, 255, 255, alpha = 100, maxColorValue = 255),  reset = F, add = T)
  plot(dam_points, cex = 1.75, col = 'red', pch = 20, add = T)
  plot(bg_tc, rgb = 1:3, main = NA, key.pos = NULL, reset = F) 
  plot(bg_fc, rgb = 1:3, main = paste(lake_zoom_buffer$Formal_Nam, lake_zoom_buffer$coords, sep = ": "), adj = 0.5, reset = F, add = TRUE)
  plot(bg_tc, rgb = 1:3, main = NA, key.pos = NULL, reset = F) 
  plot(bg_ix, col = scico::scico(n = 20, begin = 0.1, end = 0.9, direction = -1, palette = 'nuuk'), 
       main = NA, zlim = c(-0.4,0.4), key.pos = NULL, reset = F, add = T)
  addscalebar(
    plotepsg = 4326, pos = 'topleft', plotunit = 'm',
    widthhint = 0.25, padin = c(0.25, 0.20), lwd = 0.5, htin = 0.05, label.col = 'black'
  )
}

#' Create pixel_wise confusion matrix
#'  
#' @param subset define for which subset the matrix should be created 
#' 
#' @return `stars` object with assigned TP, TN, FP, FN values for each pixel to then visually inspect the results

create_pixelwise_confusion_matrix = function(subset){
  lakes = lakesproj
  val_areas = st_read('validation/validation_areas.geojson', quiet = T) %>% 
    st_transform(crs = st_crs(lakes)) 
  lakes = lakes[,,,c(4,12,15)][val_areas %>% filter(id == subset)] %>% 
    st_as_stars() %>% 
    st_set_dimensions(names = c('x','y','month')) %>% 
    st_set_dimensions(which = 'month', values = c('pred_dec_16','pred_mar_18','pred_jan_19'))
  
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
    dplyr::select(results = `mapping_results_update.epsg2193.tif`) %>%
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
  
  val = c(val_2016, val_2018, val_2019, along = 'month') %>% 
    st_set_dimensions(which = 'month', values = c('ref_dec_16','ref_mar_18','ref_jan_19'))
  
  comparison = c(lakes, val, along = 3) %>% 
    split(3) %>% 
    mutate(
      cm_dec_16 = as.factor(ifelse(
        pred_dec_16 == 1 & ref_dec_16 == 1, 1, #TP 
        ifelse(
          pred_dec_16 == 0 & ref_dec_16 == 0, 2, #TN
          ifelse(
            pred_dec_16 == 1 & ref_dec_16 == 0, 3, #FP
            ifelse(
              pred_dec_16 == 0 & ref_dec_16 == 1, 4, NA #FN
            )
          )
        )
      )),
      cm_mar_18 = as.factor(ifelse(
        pred_mar_18 == 1 & ref_mar_18 == 1, 1, #TP 
        ifelse(
          pred_mar_18 == 0 & ref_mar_18 == 0, 2, #TN
          ifelse(
            pred_mar_18 == 1 & ref_mar_18 == 0, 3, #FP
            ifelse(
              pred_mar_18 == 0 & ref_mar_18 == 1, 4, NA #FN
            )
          )
        )
      )),
      cm_jan_19 = as.factor(ifelse(
        pred_jan_19 == 1 & ref_jan_19 == 1, 1, #TP 
        ifelse(
          pred_jan_19 == 0 & ref_jan_19 == 0, 2, #TN
          ifelse(
            pred_jan_19 == 1 & ref_jan_19 == 0, 3, #FP
            ifelse(
              pred_jan_19 == 0 & ref_jan_19 == 1, 4, NA #FN
            )
          )
        )
      )
      )) %>% dplyr::select(starts_with('cm_')) %>% merge() %>% 
    st_set_dimensions(which = 'attributes', values = c('December 2016','March 2018','January 2019')) %>% 
    st_set_dimensions(names = c("x", "y", "month")) %>% 
    setNames('cm') 
  comparison$cm = as.factor(comparison$cm)
  comparison
}

#' Confusion matrix values plot
#' 
#' @param zoom result of pixel_wise_confusion_matrix set for a landslide dam buffered zoom
#' @param hillshade_zoom hillshade with the same zoom area as in the `zoom` parameter
#' @param subset_name text for the y axis to identify the area plotted
#' @param show_title boolean to set if the panel titles should be shown or not. Used to create the facet_grid illusion
#' 
#' @return plot for a desired zoomed dam for the three validation dates

cm_plot = function(zoom, hillshade_zoom, subset_name, show_title = T){
  scale_params = tibble::tibble(
    month = factor('January 2019')
  )
  ggplot() +
    geom_stars(data = zoom, downsample = 0) +
    scale_fill_manual(values = cm_palette, na.translate = F) + 
    new_scale_fill() +
    geom_stars(data = hillshade_zoom, downsample = 0, alpha = 0.25, interpolate = TRUE) +
    scale_fill_gradientn(colours = grey.colors(100, start = 0, end=.95), guide='none', na.value = NA) +
    #blue, light green, violet, yellow
    coord_sf(crs = 2193) +
    ylab(subset_name) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), position = 'left') +
    annotation_scale(
      plot_unit = 'm', data = scale_params, location = 'br', 
      width_hint = 0.3, height = unit(0.15, "cm"), text_cex = 0.5
    ) +
    facet_wrap('month') +
    theme(
      strip.text = element_text(size = 11),
      legend.position = 'none',
      panel.spacing.x = unit(.5, "cm"),
      panel.background = element_rect(fill = 'transparent'),
      strip.background = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_blank(), axis.ticks = element_blank(),
      strip.placement = NULL
    ) +
    if(!show_title){
      theme(strip.text = element_blank())
    }
}

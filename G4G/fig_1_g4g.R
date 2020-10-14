library(dplyr)
library(sf)
library(stars)
library(ggplot2)
library(ggtext)
library(tsibble)


#' Set English as language to avoid problems with dates
Sys.setlocale("LC_TIME", "English")

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
  color = c('#34A853','#4285F4','grey50'),
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

ggsave('G4G/fig_1_g4g.png', scenes_plot, height = 8.5, width = 33, units = 'cm', dpi = 300)


library(magick)
gif1 = image_read('G4G/leader_rgb.gif', density = 100)
gif2 = image_read('G4G/leader_fc.gif', density = 100)
gif3 = image_read('G4G/leader_ndwi.gif', density = 100)
gif4 = image_read('G4G/leader_res.gif', density = 100)

dates = format(mosaic_data$month[1:18], '%m/%Y')

combine = c(
  gif1[1:5],
  image_flatten(gif2[1:6]),
  gif2[7:10],
  image_flatten(gif3[1:11]),
  gif3[12:15],
  image_flatten(gif4[1:16]),
  gif4[17:18]
)

text = image_annotate(
  combine, text = dates, font = 'Trebuchet',weight = 700, kerning = 2, 
  boxcolor = 'white', size = 20, color = '#3b3c5d'
) 


image_write(format = 'gif', image = text, path = 'G4G/process.gif') 

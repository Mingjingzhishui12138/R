install.packages('devtools')
library(devtools)
library(usethis)
library(ggplot2)
library(baidumap)
library(ggmap)
options(baidumap.key='SoQj30u14pUFmOQ3410eXKn4igl7ntEu')
shmap<- getCoordinate('ÉîÛÚ´óÑ§')

shmap

library(mapproj)
map <- get_map(location = 'China', zoom = 4)
ggmap(map)
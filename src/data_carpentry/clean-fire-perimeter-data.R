### The original data source:
### These data need to be cleaned such that, at a minimum, each fire represented has a geometry of its perimeter and a fire start date. The Google Earth Engine will use these two variables to calculate heterogeneity of NDWI and a number of other metrics.

library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)

millisecondsSinceEpoch <- function(date) {
  date <- ymd(date)
  epoch <- ymd("1970-01-01")
  
  return(as.numeric(as.duration(date - epoch) * 1000))
}

metadata <- read.csv("data/fires-info.csv")
fires <- shapefile("data/sn-fire-perims-post-1985/sn-fire-perims-post-1985.shp")
# sn <- shapefile("/Users/mikoontz/Documents/Research/remote-sensing-resilience/sierra-nevada-perim/sierra-nevada-CA-perim/sierra-nevada-CA-perim.shp")

str(fires, max.level = 2)

missing_dates <-
  fires %>%
  as.data.frame() %>%
  filter(is.na(ALARM_DATE)) %>%
  dplyr::select(YEAR_, STATE, FIRE_NAME, FIRE_NUM, INC_NUM, COMMENTS, GIS_ACRES, CONT_DATE_)

fires <- fires[!is.na(fires$ALARM_DATE), ]
fires <- fires[ , c("FIRE_NAME", "FIRE_NUM", "ALARM_DATE", "CONT_DATE_", "GIS_ACRES", "Shape_Area")]
names(fires) <- c("name", "fire_id", "start_date", "cont_date", "gis_area", "area")

fires$start_date <- substr(fires$start_date, start = 1, stop = 10)
fires$fire_date <- millisecondsSinceEpoch(fires$start_date)
fires$cont_date <- substr(fires$cont_date, start = 1, stop = 10)
fires$contain_date <- millisecondsSinceEpoch(fires$cont_date)

# Write to shapefile
# dir.create("data/sn-fire-perims-post-1985-clean")
writeOGR(fires, "data/sn-fire-perims-post-1985-clean/sn-fire-perims-post-1985-clean.shp", layer="fire-perims", driver = "ESRI Shapefile")

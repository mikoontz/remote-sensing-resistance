library(raster)
library(lubridate)
library(ggmap)
library(rgdal)
library(sf)

landfire <- st_read("data/features/cbi_data/firesev_landfire/firesev_cbi_data")
sn <- st_read("data/features/SierraEcoregion_TNC/")
landfire <- st_transform(landfire, st_crs(sn))

filepaths <- list.files(path = "data/features/cbi_data/usgs", full.names = TRUE, pattern = ".csv")

usgs_list <- lapply(filepaths, FUN = function(file) read.csv(file, stringsAsFactors = FALSE))
usgs_list <- lapply(usgs_list, function(x) subset(x[which(x$qual_ea == 1), ]))
usgs <- do.call(rbind, usgs_list)

hist(as.numeric(usgs$cbi_totl), breaks = 100)
# usgs_compact <- subset(usgs, select = c(fireyr, firedate, fire_nam, plot_id,  utmeast, utmnorth, utm_zone, utme_rep, utmn_rep, utmz_rep, cbi_undr, cbi_over, cbi_totl))
usgs_compact <- usgs 
unique(usgs$firedate)

bad_dates <- c("7/2000", "7/2001", "01-AUG-200", "15-JUL-200", "8/2001", "Nov-01", "30-OCT-200", "01-NOV-200", "03-NOV-200", "29-JUL-200", "15-AUG-200", "10-SEP-200", "25-AUG-200", "16-AUG-200", "01-JUL-200", "May-00", "20-JUL-200")
y2k_bug <- c("07/01/02", "04/01/02", "04/02/02", "03/02/02", "07/02/02")

check_date <- function(data, bad_date) {
  list(bad_date, 
       unique(data[data$firedate == bad_date, "fire_nam"]),
       usgs_compact[usgs_compact$firedate == current, ])
}

# 7/2000
(current <- bad_dates[1])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Foraker", "firedate"] <- "06-23-2000"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Otter Creek", "firedate"] <- "06-24-2000"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Chitsia", "firedate"] <- "06-26-2000"

# 7/2001
(current <- bad_dates[2])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Herron River", "firedate"] <- "06-26-2001"

# 01-AUG-200
(current <- bad_dates[3])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Milepost 85", "firedate"] <- "08-04-2002"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "FALCON", "firedate"] <- "08-06-2001"

# 15-JUL-200
(current <- bad_dates[4])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Cottonwood Bar", "firedate"] <- "08-02-2002"

# 8/2001
(current <- bad_dates[5])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Hoover", "firedate"] <- "07-10-2001"

# Nov-01
(current <- bad_dates[6])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Green Mtn", "firedate"] <- "11-10-2001"

# 30-OCT-2000
(current <- bad_dates[7])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Schoolhouse", "firedate"] <- "10-30-2000"

# 01-NOV-200
(current <- bad_dates[8])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Camp Branch", "firedate"] <- "11-01-2000"

# 03-NOV-200
(current <- bad_dates[9])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Darrow Ridge", "firedate"] <- "11-03-2000"

# 29-JUL-200
(current <- bad_dates[10])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Arthur", "firedate"] <- "07-29-2001"

# 15-AUG-200
(current <- bad_dates[11])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Boundary00", "firedate"] <- "08-15-2000"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Moose00", "firedate"] <- "08-15-2000"

# 10-SEP-200
(current <- bad_dates[12])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Little", "firedate"] <- "09-10-2001"

# 15-AUG-200
(current <- bad_dates[13])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Little Joe", "firedate"] <- "08-25-2001"

# 16-AUG-200
(current <- bad_dates[14])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Stone", "firedate"] <- "08-16-2001"

# 15-AUG-200
(current <- bad_dates[15])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "FALCON", "firedate"] <- "08-06-2001"

# May-00
(current <- bad_dates[16])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Viveash", "firedate"] <- "05-29-2000"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Cerro Grande", "firedate"] <- "05-05-2000"

# 20-JUL-200
(current <- bad_dates[17])
check_date(usgs_compact, current)[[1]]
check_date(usgs_compact, current)[[2]]
check_date(usgs_compact, current)[[3]]

usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Bircher", "firedate"] <- "07-20-2000"
usgs_compact[usgs_compact$firedate == current & usgs_compact$fire_nam == "Pony", "firedate"] <- "08-02-2000"

# Final clean-up for dates whose years were designated as 2-digit years
idx <- which(usgs_compact$firedate %in% y2k_bug)
usgs_compact$FireDate <- mdy("01-01-1970")
usgs_compact$FireDate[idx] <- mdy(usgs_compact[idx, "firedate"])
usgs_compact$FireDate[-idx] <- mdy(usgs_compact[-idx, "firedate"])

#######################

clean_dates <- usgs_compact

clean_dates$utm_zone[clean_dates$utm_zone == "#NULL!"] <- NA
clean_dates$utm_zone <- as.numeric(clean_dates$utm_zone)
clean_dates$utmz_rep[clean_dates$utmz_rep == "#NULL!"] <- NA
clean_dates$utmz_rep <- as.numeric(clean_dates$utmz_rep)

clean_dates$utmeast[clean_dates$utmeast == "#NULL!"] <- NA
clean_dates$utmeast <- as.numeric(clean_dates$utmeast)
clean_dates$utmnorth[clean_dates$utmnorth == "#NULL!"] <- NA
clean_dates$utmnorth <- as.numeric(clean_dates$utmnorth)
clean_dates$utme_rep[clean_dates$utme_rep == "#NULL!"] <- NA
clean_dates$utme_rep <- as.numeric(clean_dates$utme_rep)
clean_dates$utmn_rep[clean_dates$utmn_rep == "#NULL!"] <- NA
clean_dates$utmn_rep <- as.numeric(clean_dates$utmn_rep)

bad_zone <- is.na(clean_dates$utm_zone) & is.na(clean_dates$utmz_rep)
bad_fires <- unique(clean_dates[which(bad_zone), "fire_nam"])
bad_fires

# Hmm. Just Alaska seems to be a problem.
unique(clean_dates$region[which(bad_zone)])

# Clean Foraker Fire
# usgs[which(bad_zone & clean_dates$fire_nam == bad_fires[1]), ]
# Based on the general Long/Lat coordinates, this fire should be in Zone 5. But the UTM coordinates make no sense for any zones.
# usgs[which(bad_zone & clean_dates$fire_nam == bad_fires[1]), "utmz_rep"] <- 5

# Clean Otter Creek Fire
# clean_dates[which(bad_zone & clean_dates$fire_nam == bad_fires[2]), ]
# Same deal here. These UTM coordinates don't make any sense.

# Get rid of all plots with messed up zones (as evidenced by no Zone information and weird coordinates)
clean_dates <- subset(clean_dates, subset = !bad_zone)

# Any CBI plots with no coordinates?
bad_coords <- (is.na(clean_dates$utme_rep) & is.na(clean_dates$utmeast)) | (is.na(clean_dates$utmn_rep) & is.na(clean_dates$utmnorth))
bad_fires <- unique(clean_dates[which(bad_coords), "fire_nam"])
bad_fires
# Nope! All seem to have a pair

clean_dates$utmz_rep[is.na(clean_dates$utmz_rep)] <- clean_dates$utm_zone[is.na(clean_dates$utmz_rep)]
clean_dates$utme_rep[is.na(clean_dates$utme_rep)] <- clean_dates$utmeast[is.na(clean_dates$utme_rep)]
clean_dates$utmn_rep[is.na(clean_dates$utmn_rep)] <- clean_dates$utmnorth[is.na(clean_dates$utmn_rep)]
clean_dates$datumrep[is.na(clean_dates$datumrep)] <- clean_dates$gpsdatum[is.na(clean_dates$datumrep)]

# tail(clean_dates)
clean <- subset(clean_dates, select = c(fire_nam, region, FireDate, utme_rep, utmn_rep, utmz_rep, datumrep, cbi_totl, cbi_undr, cbi_over))
# head(clean)

# All plots have a cbi_totl value?
clean$cbi_totl <- as.numeric(clean$cbi_totl)

# Clean the datumrep column
unique(clean$datumrep)
clean$datumrep[clean$datumrep == "NAD 27"] <- "NAD27"
clean$proj4string <- paste0("+proj=utm +zone=", clean$utmz_rep, " +datum=", clean$datumrep)

rows <- 1:nrow(clean)
list_points <- lapply(rows, FUN = function(i) {
  current_CRS <- crs(clean$proj4string[i])
  spdf_current <- SpatialPointsDataFrame(coords = clean[i, c("utme_rep", "utmn_rep")],
                                         data = clean[i, ],
                                         proj4string = current_CRS)
})

# Transform all projections to be the same as the one for Landfire database
list_points <- lapply(list_points, FUN = function(x) {
  spTransform(x, crs(landfire))
})

usgs_points <- do.call(rbind, list_points)
usgs_points_compact <- subset(usgs_points, select = c(fire_nam, FireDate, cbi_totl))
landfire_compact <- subset(landfire, select = c(FireName, FireDate, CBI))
landfire_compact$FireDate <- ymd(landfire_compact$FireDate)

usgs_points_compact <- as.data.frame(usgs_points_compact)
landfire_compact <- as.data.frame(landfire_compact)

names(usgs_points_compact) <- c("FireName", "FireDate", "cbi", "lon", "lat")
names(landfire_compact) <- names(usgs_points_compact)

all_cbi_points <- rbind(landfire_compact, usgs_points_compact)
all_cbi <- SpatialPointsDataFrame(coords = all_cbi_points[, c("lon", "lat")],
                                  data = all_cbi_points,
                                  proj4string = crs(landfire))

states <- map_data("state")
ca <- map_data("state", region = "California")
ca <- Polygon(coords = ca[, c("long", "lat")])
ca <- Polygons(srl = list(ca), ID = "California")
ca <- SpatialPolygons(Srl = list(ca), proj4string = crs(landfire))

ca_cbi <- all_cbi[ca, ]
sn_cbi <- all_cbi[sn, ]
nrow(ca_cbi)
nrow(sn_cbi)

plot(sn_cbi)
length(unique(sn_cbi$FireName))

plot(ca, lwd = 2)
plot(ca_cbi, col = "red", pch = 19, add = TRUE)
plot(sn, lwd = 2, add = TRUE)
plot(all_cbi, col = "red", pch = 19, add = TRUE)

# Some dates are way too low in usgs dataset!
min(all_cbi$FireDate)
which(year(landfire_compact$FireDate) < 500)
which(year(usgs_points_compact$FireDate) < 500)

ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat,  group = group), fill = "white", color = "black") +
  guides(fill = FALSE) +
  geom_point(data = all_cbi_points, aes(x = lon, y = lat), color = "red") +
  coord_fixed(ratio = 1.3)

plot(landfire)
plot(usgs_points, add = TRUE)
plot(sn, add = TRUE)

# dir.create("../data/all-cbi/")
# writeOGR(all_cbi, "../data/all-cbi/all-cbi.shp", layer="all-cbi", driver = "ESRI Shapefile")
writeOGR(all_cbi, "../data/all-cbi.kml", layer="all-cbi", driver = "KML")

# dir.create("../data/sierra-nevada-cbi/")
# writeOGR(sn_cbi, "../data/sierra-nevada-cbi/sierra-nevada-cbi.shp", layer="sierra-nevada-cbi", driver = "ESRI Shapefile")

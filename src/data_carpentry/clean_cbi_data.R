# rm(list = ls())
library(raster)
library(dplyr)
library(lubridate)
library(ggmap)
library(rgdal)
library(sf)

# sn <- st_read("data/features/SierraEcoregion_TNC/")
sn <- 
  st_read("data/features/SierraEcoregion_Jepson/") %>%
  st_transform(crs = "+init=epsg:4326")

#### First read the USGS data ####

# USGS Data from Zhu et al. 2006 (https://www.firescience.gov/projects/01-1-4-12/project/01-1-4-12_final_report.pdf)
# Spatial data from https://archive.usgs.gov/archive/sites/www.nrmsc.usgs.gov/science/fire/cbi/plotdata.html
#### Read files ####
filepaths <- list.files(path = "data/features/cbi_data/usgs", full.names = TRUE, pattern = ".csv")
usgs_list <- lapply(filepaths, FUN = function(file) read.csv(file, stringsAsFactors = FALSE))
usgs_list <- lapply(usgs_list, function(x) subset(x[which(x$qual_ea == 1), ]))
usgs_cbi <- do.call(rbind, usgs_list)

# usgs_compact <- subset(usgs, select = c(fireyr, firedate, fire_nam, plot_id,  utmeast, utmnorth, utm_zone, utme_rep, utmn_rep, utmz_rep, cbi_undr, cbi_over, cbi_totl))
unique(usgs_cbi$firedate)

#### Correct the fire dates ####
# Which dates don't parse?
# Indices of bad dates
bad_dates_idx <- is.na(mdy(unique(usgs_cbi$firedate)))
bad_dates_idx <- is.na(mdy(unique(usgs_cbi$firedate)))
# Bad dates themselves (c("7/2000", "7/2001", "01-AUG-200", "15-JUL-200", "8/2001", "Nov-01", "30-OCT-200", "01-NOV-200", "03-NOV-200", "29-JUL-200", "15-AUG-200", "10-SEP-200", "25-AUG-200", "16-AUG-200", "01-JUL-200", "May-00", "20-JUL-200"))
bad_dates <- unique(usgs_cbi$firedate)[bad_dates_idx]

# Convenience function for checking the dates on wildfires that don't have a parseable date
check_date <- function(data, bad_date) {
  list(bad_date, # The bad date
       unique(data[data$firedate == bad_date, "fire_nam"]), # The fires that have the bad date
       usgs_cbi[usgs_cbi$firedate == current, ]) # The whole record of all plots with the bad date
}

# I systematically checked each of the 17 bad dates to confirm the date of those fires
# using the Monitoring Trends in Burn Severity dataset (http://mtbs.gov/data/individualfiredata.html)

# 7/2000
(current <- bad_dates[1])
check_date(usgs_cbi, current)[[1]] # The bad date (confirm this is the same as "current")
check_date(usgs_cbi, current)[[2]] # All fires with the bad date in their $firedate column
check_date(usgs_cbi, current)[[3]] # The whole record of all plots with the bad date

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Foraker", "firedate"] <- "06-23-2000"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Otter Creek", "firedate"] <- "06-24-2000"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Chitsia", "firedate"] <- "06-26-2000"

# 7/2001
(current <- bad_dates[2])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Herron River", "firedate"] <- "06-26-2001"

# 01-AUG-200
(current <- bad_dates[3])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Milepost 85", "firedate"] <- "08-04-2002"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "FALCON", "firedate"] <- "08-06-2001"

# 15-JUL-200
(current <- bad_dates[4])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Cottonwood Bar", "firedate"] <- "08-02-2002"

# 8/2001
(current <- bad_dates[5])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Hoover", "firedate"] <- "07-10-2001"

# Nov-01
(current <- bad_dates[6])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Green Mtn", "firedate"] <- "11-10-2001"

# 30-OCT-2000
(current <- bad_dates[7])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Schoolhouse", "firedate"] <- "10-30-2000"

# 01-NOV-200
(current <- bad_dates[8])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Camp Branch", "firedate"] <- "11-01-2000"

# 03-NOV-200
(current <- bad_dates[9])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Darrow Ridge", "firedate"] <- "11-03-2000"

# 29-JUL-200
(current <- bad_dates[10])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Arthur", "firedate"] <- "07-29-2001"

# 15-AUG-200
(current <- bad_dates[11])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Boundary00", "firedate"] <- "08-15-2000"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Moose00", "firedate"] <- "08-15-2000"

# 10-SEP-200
(current <- bad_dates[12])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Little", "firedate"] <- "09-10-2001"

# 15-AUG-200
(current <- bad_dates[13])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Little Joe", "firedate"] <- "08-25-2001"

# 16-AUG-200
(current <- bad_dates[14])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Stone", "firedate"] <- "08-16-2001"

# 15-AUG-200
(current <- bad_dates[15])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "FALCON", "firedate"] <- "08-06-2001"

# May-00
(current <- bad_dates[16])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Viveash", "firedate"] <- "05-29-2000"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Cerro Grande", "firedate"] <- "05-05-2000"

# 20-JUL-200
(current <- bad_dates[17])
check_date(usgs_cbi, current)[[1]]
check_date(usgs_cbi, current)[[2]]
check_date(usgs_cbi, current)[[3]]

usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Bircher", "firedate"] <- "07-20-2000"
usgs_cbi[usgs_cbi$firedate == current & usgs_cbi$fire_nam == "Pony", "firedate"] <- "08-02-2000"

usgs_cbi$firedate <- mdy(usgs_cbi$firedate)

# 11/06/1999
# This error was insidious. Apparently, the "Lost Fire" did not start on 11/06/1999, despite the USGS
# data suggesting it did. This fire is also known as the "Lost Bear Fire" and is referenced in a
# few papers (Meng et al. 2015. Remote Sensing of the Environment; Van Wagtendonk et al. 2012. Fire Ecology; 
# Miller et al. 2009. Remote Sensing of the Environment). Pre-fire imagery from the Lost Bear
# Fire was taken 07/09/1999 and post-fire imagery was taken 07/11/2000. The FRAP database has a "Lost Bear
# Fire" that began on 07/11/1999. Based on the spatial locations of the CBI plots, the only possible 
# fire this could be in the MTBS database is the "South Park Complex" which burned a 
# similar area as the "Lost Fire" aka "Lost Bear" fire. MTBS has the South Park Complex fire starting
# on 07/11/1999, thus we believe the start date of 11/06/1999 for the "Lost Fire" is incorrect and
# we have changed it to 07/11/1999

usgs_cbi[usgs_cbi$fire_nam == "Lost", "firedate"] <- mdy("07/11/1999")

# 
#### Fix the spatial component ####
# Now we have a dataset with all clean dates
# We need to fix the spatial component
# Replace the USFS "missingness" value (#NULL!) with R's missingness value (NA)
# and convert to numeric

clean_dates <- usgs_cbi

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

# Fires with bad zone information if both the native UTM zone and the
# UTM zone used for image extraction are missing
bad_zone <- is.na(clean_dates$utm_zone) & is.na(clean_dates$utmz_rep)
bad_fires <- unique(clean_dates[which(bad_zone), "fire_nam"])
bad_fires

# Hmm. Just Alaska seems to be a problem.
unique(clean_dates$region[which(bad_zone)])

# Clean Foraker Fire
# usgs_cbi[which(bad_zone & clean_dates$fire_nam == bad_fires[1]), ]
# Based on the general Long/Lat coordinates, this fire should be in Zone 5. But the UTM coordinates make no sense for any zones.
# usgs_cbi[which(bad_zone & clean_dates$fire_nam == bad_fires[1]), "utmz_rep"] <- 5

# Clean Otter Creek Fire
# clean_dates[which(bad_zone & clean_dates$fire_nam == bad_fires[2]), ]
# Same deal here. These UTM coordinates don't make any sense to me.

# Get rid of all plots with messed up zones (as evidenced by no Zone information and weird coordinates)
clean_dates <- subset(clean_dates, subset = !bad_zone)

# Any CBI plots with no coordinates?
bad_coords <- (is.na(clean_dates$utme_rep) & is.na(clean_dates$utmeast)) | (is.na(clean_dates$utmn_rep) & is.na(clean_dates$utmnorth))
bad_fires <- unique(clean_dates[which(bad_coords), "fire_nam"])
bad_fires
# Nope! All seem to have a pair

# We're going to use the transformed coordinates for consistency (rather than whatever
# was collected in the field)
# Here, we assign the transformed coordinate columns to the native field column values (
# which occured when no transformation was necessary), but only if the transformed
# coordinate columns had missing values
clean_dates$utmz_rep[is.na(clean_dates$utmz_rep)] <- clean_dates$utm_zone[is.na(clean_dates$utmz_rep)]
clean_dates$utme_rep[is.na(clean_dates$utme_rep)] <- clean_dates$utmeast[is.na(clean_dates$utme_rep)]
clean_dates$utmn_rep[is.na(clean_dates$utmn_rep)] <- clean_dates$utmnorth[is.na(clean_dates$utmn_rep)]
clean_dates$datumrep[is.na(clean_dates$datumrep)] <- clean_dates$gpsdatum[is.na(clean_dates$datumrep)]

clean <- subset(clean_dates, select = c(fire_nam, region, firedate, utme_rep, utmn_rep, utmz_rep, datumrep, cbi_totl, cbi_undr, cbi_over))

# Swap out USGS missingness ("#NULL!") with R missingness (NA)
clean$cbi_totl[clean$cbi_totl == "#NULL!"] <- NA
clean$cbi_over[clean$cbi_over == "#NULL!"] <- NA

# Convert columns to numeric values instead of character
clean$cbi_totl <- as.numeric(clean$cbi_totl)
clean$cbi_over <- as.numeric(clean$cbi_over)

# Clean the datumrep column
unique(clean$datumrep)
clean$datumrep[clean$datumrep == "NAD 27"] <- "NAD27"

# Set the coordinate reference system for the points based on their particular zone and datum
clean$proj4string <- paste0("+proj=utm +zone=", clean$utmz_rep, " +datum=", clean$datumrep)

# Make the points spatial
# Also, turn sp objects into sf objects and transform all projections to be the same as the one for Landfire database
rows <- 1:nrow(clean)
list_points <- lapply(rows, FUN = function(i) {
  current_CRS <- crs(clean$proj4string[i])
  spdf_current <- SpatialPointsDataFrame(coords = clean[i, c("utme_rep", "utmn_rep")],
                                         data = clean[i, ],
                                         proj4string = current_CRS)
  x <- st_as_sf(spdf_current)
  x <- st_transform(x, 4326)
  x
})

usgs_cbi_sf <- do.call(rbind, list_points)
usgs_cbi_sf$source <- "zhu2006"

plot(usgs_cbi_sf$geometry)
plot(sn, add = TRUE)

#### Now work with the Landfire data ####
# Try to extract both overstory and overall CBI from Landfire data
# All data available from: https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0017/
# Sikkink, Pamela G.; Dillon, Gregory K.; Keane,Robert E.; Morgan, Penelope; Karau, Eva C.; Holden, Zachary A.; Silverstein, Robin P. 2013. Composite Burn Index (CBI) data and field photos collected for the FIRESEV project, western United States. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2013-0017

fd <- read.csv("data/features/cbi_data/firesev_landfire/FIRESEV_Dataset_All_variables.csv", skip = 1)

# Look just at the overstory columns (i.e. not the substrate (A) or small tree strata (B))
# CBI measurements with a _t suffix have a "smallest twig" component included
overstory_cols_t <- fd[, c("cbi_C_t", "cbi_D_t", "cbi_E", "cbi_F")]
overstory_cols <- fd[, c("cbi_C", "cbi_D", "cbi_E", "cbi_F")]

# Where are all the overstory strata NA?
overstory_all_NA <- apply(overstory_cols, 1, function(x) all(is.na(x)))
overstory_all_NA_t <- apply(overstory_cols_t, 1, function(x) all(is.na(x)))

# If the overstory strata are NA, overstory CBI is an NA, otherwise it's the mean value
# across the other strata
fd$cbi_over <- ifelse(overstory_all_NA, yes = NA, no = apply(overstory_cols, MARGIN = 1, FUN = mean, na.rm = TRUE))
fd$cbi_over_t <- ifelse(overstory_all_NA_t, yes = NA, no = apply(overstory_cols_t, MARGIN = 1, FUN = mean, na.rm = TRUE))

# Subset the fd dataframe to just be the few columns we need
fd_sub <- dplyr::select(fd, Northing, Easting, UTM_Zone, Datum, FireDate, FireName, PlotID, cbi_over, cbi_over_t, cbi_tot, cbi_tot_t)

fd_sub$proj4string <- paste0("+proj=utm +zone=", fd_sub$UTM_Zone, " +datum=", fd_sub$Datum)

# Make the points spatial
# Also, turn sp objects into sf objects and transform all projections to be the same as the one for Landfire database
rows <- 1:nrow(fd_sub)
list_points <- lapply(rows, FUN = function(i) {
  current_CRS <- crs(fd_sub$proj4string[i])
  spdf_current <- SpatialPointsDataFrame(coords = fd_sub[i, c("Easting", "Northing")],
                                         data = fd_sub[i, ],
                                         proj4string = current_CRS)
  x <- st_as_sf(spdf_current)
  x <- st_transform(x, 4326)
  x
})

landfire_cbi_sf <- do.call(rbind, list_points)
landfire_cbi_sf$source <- "sikkink2013"

plot(landfire_cbi_sf$geometry)
plot(sn, add = TRUE)

landfire_cbi_sf$FireDate <- mdy(landfire_cbi_sf$FireDate)

#### Combine data sources ####
head(usgs_cbi_sf)
head(landfire_cbi_sf)

usgs_cbi_sf %<>%
  dplyr::mutate(id = 1:nrow(.), cbi_over_t = NA, cbi_totl = NA, cbi_tot_t = NA) %>%
  dplyr::select(id, firedate, fire_nam, cbi_over, cbi_over_t, cbi_totl, cbi_tot_t, source, geometry) %>%
  dplyr::rename(alarm_date = firedate, fire_name = fire_nam, cbi_tot = cbi_totl)

landfire_cbi_sf %<>%
  dplyr::select(PlotID, FireDate, FireName, cbi_over, cbi_over_t, cbi_tot, cbi_tot_t, source, geometry) %>%
  dplyr::rename(id = PlotID, alarm_date = FireDate, fire_name = FireName)

head(usgs_cbi_sf)
head(landfire_cbi_sf)

cbi_sf <- rbind(usgs_cbi_sf, landfire_cbi_sf)
sn_cbi <- cbi_sf[sn, ]

plot(st_geometry(sn_cbi))
plot(st_geometry(sn), add = TRUE)  

# Write the objects as both a kml and a shapefile
st_write(obj = sn_cbi, dsn = "data/features/cbi_data/cbi_sn/cbi_sn.shp")
st_write(obj = sn_cbi, dsn = "data/features/cbi_data/cbi_sn/cbi_sn.kml")

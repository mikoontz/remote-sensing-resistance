# This script filters the USFS Region 5 fire perimeter dataset to just include the perimeters in the Sierra Nevada region
library(sf)
library(dplyr)

usfs <- st_read(dsn = "data/features/fire_perim/veg_severity_perimeters16_1.gdb/",
                stringsAsFactors = FALSE) %>% 
  mutate(geometry = st_sfc(Shape)) %>% 
  st_set_geometry("geometry") %>% 
  select(-Shape) %>% 
  st_transform(4326)

sn <- st_read("data/features/SierraEcoregion_Jepson/SierraEcoregion_Jepson.shp") %>% 
  st_transform(4326)

usfs_sn <- st_intersection(x = usfs, y = sn)

save(usfs_sn, list = "usfs_sn", file = "data/data_output/usfs-fire-perimeters-sn.rds")

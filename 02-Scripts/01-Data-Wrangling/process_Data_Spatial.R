#spatial


## load data


## packages
library(dplyr)
library(sf)


## helper functions



## read shapefile for counties

shape <- st_read("./01-Data/00-Raw-Data/Spatial/cb_2018_us_county_5m.shp")



## create summary metrics

states <- shape %>% 
  st_drop_geometry() %>%
  group_by(STATEFP) %>%
  summarise(county_count = n(), 
            county_area_km2_mean = mean(ALAND)/1000000, 
            state_area_km2 = sum(ALAND)/1000000, 
            state_area_km2_water = sum(AWATER)/1000000) %>% 
  ungroup()





## save 

save(states, file = "./01-Data/01-Processed-Data/states.rds")



## clean environment
rm(list=ls())
gc()








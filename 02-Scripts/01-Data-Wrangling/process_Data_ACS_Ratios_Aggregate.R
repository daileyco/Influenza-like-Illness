# script to process and aggregate commuting flows data


## load data
load("../Mobility-Models/01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")
load("../Mobility-Models/01-Data/02-Analytic-Data/od.rds")


## packages
library(dplyr)
library(tidyr)


## helper functions



## isolate distance thresholds to use
distance.thresholds <- distance.thresholds %>% 
  filter(!(region%in%c("All US")) & 
           objective.scale%in%c("log") & 
           model.extent%in%c("base*distance_threshold*large_populations") & 
           period %in% c("2011-2015", "2016-2020")) %>% 
  select(period, census.region.origin = region, distance_threshold)




## merge distance thresholds into commuting flows data
### use to create summary metrics of commutes at various distances
### commutes at distances used to create ratios and proportions
od <- od %>% 
  left_join(., 
            distance.thresholds, 
            by = c("period", "census.region.origin")) %>% 
  mutate(flow_type = case_when(log.distance.km==0 ~ "Internal", 
                               log.distance.km<=distance_threshold ~ "Short Distance", 
                               log.distance.km>distance_threshold ~ "Long Distance", 
                               TRUE ~ NA) %>% 
           factor(., levels = c("Internal", "Short Distance", "Long Distance"), ordered = TRUE))


com.meandist <- od %>%
  group_by(`State FIPS Residence`, `State Residence`, period) %>% 
  summarise(distance_mean_km = weighted.mean(distance.km, `Workers in Commuting Flow`), 
            distance_mean_km_nozeros = weighted.mean(distance.km[which(distance.km>0)], `Workers in Commuting Flow`[which(distance.km>0)])) %>%
  ungroup()




com <- od %>%
  group_by(`State FIPS Residence`, `State Residence`, period, flow_type) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`, na.rm = T)) %>%
  ungroup() %>%
  group_by(`State FIPS Residence`, `State Residence`, period) %>%
  mutate(Total.Workers = sum(`Workers in Commuting Flow`)) %>%
  ungroup() %>%
  pivot_wider(names_from = flow_type, values_from = `Workers in Commuting Flow`) %>%
  mutate(ratio.si = `Short Distance`/Internal,
         ratio.ls = `Long Distance`/`Short Distance`, 
         across(c(Internal, `Short Distance`, `Long Distance`), ~.x/Total.Workers*100, .names = "{.col}_prop")) %>%
  full_join(., 
            com.meandist, 
            by = c("State FIPS Residence", "State Residence", "period"))







## save
save(com, file = "./01-Data/02-Analytic-Data/commutes.rds")



## clean environment
rm(list=ls())
gc()








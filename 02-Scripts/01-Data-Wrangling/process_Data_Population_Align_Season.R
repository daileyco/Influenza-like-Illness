# script to interpolate population estimates to better align with influenza season

## load data
# load("./01-Data/01-Processed-Data/pop.rds")
load("../Mobility-Models/01-Data/01-Processed-Data/pop.rds")
load("./01-Data/02-Analytic-Data/netcommutes.rds")



## packages
library(zoo)
library(dplyr)
library(tibble)
library(tidyr)



# ## remove years not in ILI dataset
# pop <- pop %>% 
#   filter(year %in% 2011:2020)


# ## for each location separately, take average of two sequential years
# ## put out a nice dataframe with population estimates aligned with seasons
# pop.seasonal <- tapply(pop$population, pop$region, 
#                        function(x){
#                          rollmean(x, k=2) %>% as.data.frame()
#                        }) %>% 
#   bind_rows() %>% 
#   t() %>%
#   as.data.frame() %>%
#   setNames(., nm = rollmean(as.numeric(unique(pop$year)), k=2)) %>%
#   rownames_to_column(var = "region") %>%
#   pivot_longer(., 2:ncol(.), names_to = "year", values_to = "population") %>%
#   mutate(year = as.numeric(year), 
#          season = factor(paste0(as.integer(year-0.5), "-", as.integer(year+0.5))))
# 
# 


pop <- pop %>%
  mutate(fips = paste0(STATE,COUNTY)) %>% 
  mutate(period = case_when(as.numeric(YEAR) <= 2015 ~ "2011-2015", 
                            as.numeric(YEAR) <= 2020 ~ "2016-2020", 
                            TRUE ~ NA)) %>%
  full_join(., 
            net, 
            by = c("fips"="fips", 
                   "period"="period", 
                   "STATE"="State FIPS Residence", 
                   "STNAME"="State Residence")) %>%
  mutate(POPESTIMATE_day = as.numeric(POPESTIMATE)+total.in-total.out,
         POPESTIMATE_daysd = as.numeric(POPESTIMATE)+total.insd-total.outsd)


pop.seasonal <- pop %>% 
  group_by(fips) %>% 
  arrange(YEAR) %>% 
  mutate(pop2 = lag(POPESTIMATE,1), 
         popday2 = lag(POPESTIMATE_day,1),
         popdaysd2 = lag(POPESTIMATE_daysd,1),
         year2 = lag(YEAR)) %>% 
  ungroup() %>% 
  filter(!YEAR%in%c(2011)) %>% 
  mutate(pop2 = ifelse(is.na(pop2), 
                       POPESTIMATE, 
                       pop2),
         popday2 = ifelse(is.na(popday2), 
                       POPESTIMATE_day, 
                       popday2),
         popdaysd2 = ifelse(is.na(popdaysd2), 
                       POPESTIMATE_daysd, 
                       popdaysd2),
         year2 = ifelse(is.na(year2), 
                        as.character(as.numeric(YEAR)-1), 
                        year2)) %>% 
  mutate(pop = (as.numeric(POPESTIMATE)+as.numeric(pop2))/2,
         popday = (as.numeric(POPESTIMATE_day)+as.numeric(popday2))/2,
         popdaysd = (as.numeric(POPESTIMATE_daysd)+as.numeric(popdaysd2))/2,
         season = factor(paste0(year2, "-", YEAR))) %>%
  group_by(season, STATE, STNAME) %>%
  summarise(county.pop.mean = mean(pop), 
            population = sum(pop), 
            county.pop.var = var(pop), 
            
            county.popday.mean = mean(popday), 
            populationday = sum(popday), 
            county.popday.var = var(popday), 
            
            county.popdaysd.mean = mean(popdaysd), 
            populationdaysd = sum(popdaysd), 
            county.popdaysd.var = var(popdaysd)) %>%
  ungroup() %>%
  mutate(state.crowding = county.pop.mean+(ifelse(is.na(county.pop.var), 0, county.pop.var)/county.pop.mean)-1, 
         state.patchiness = state.crowding / county.pop.mean, 
         
         state.crowding.day = county.popday.mean+(ifelse(is.na(county.popday.var), 0, county.popday.var)/county.popday.mean)-1, 
         state.patchiness.day = state.crowding.day / county.popday.mean, 
         
         state.crowding.daysd = county.popdaysd.mean+(ifelse(is.na(county.popdaysd.var), 0, county.popdaysd.var)/county.popdaysd.mean)-1, 
         state.patchiness.daysd = state.crowding.daysd / county.popdaysd.mean, 
         
         state.crowding.dailychange = state.crowding.day-state.crowding, 
         state.patchiness.dailychange = state.patchiness.day-state.patchiness, 
         
         # state.crowding.dailysdchange = state.crowding.daysd-state.crowding, 
         # state.patchiness.dailysdchange = state.patchiness.daysd-state.patchiness, 
         
         state.crowding.dailychangeratio = state.crowding.day/state.crowding, 
         state.patchiness.dailychangeratio = state.patchiness.day/state.patchiness, 
         
         # state.crowding.dailysdchangeratio = state.crowding.daysd/state.crowding, 
         # state.patchiness.dailysdchangeratio = state.patchiness.daysd/state.patchiness
         ) %>%
  rename(region=STNAME)








## save
save(pop.seasonal, file = "./01-Data/02-Analytic-Data/pop_seasonal.rds")

## clean environment
rm(list = ls())
gc()







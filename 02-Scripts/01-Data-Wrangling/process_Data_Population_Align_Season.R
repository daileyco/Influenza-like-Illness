# script to interpolate population estimates to better align with influenza season

## load data
load("./01-Data/01-Processed-Data/pop.rds")


## packages
library(zoo)
library(dplyr)
library(tibble)
library(tidyr)



## remove years not in ILI dataset
pop <- pop %>% 
  filter(year %in% 2011:2020)


## for each location separately, take average of two sequential years
## put out a nice dataframe with population estimates aligned with seasons
pop.seasonal <- tapply(pop$population, pop$region, 
                       function(x){
                         rollmean(x, k=2) %>% as.data.frame()
                       }) %>% 
  bind_rows() %>% 
  t() %>%
  as.data.frame() %>%
  setNames(., nm = rollmean(as.numeric(unique(pop$year)), k=2)) %>%
  rownames_to_column(var = "region") %>%
  pivot_longer(., 2:ncol(.), names_to = "year", values_to = "population") %>%
  mutate(year = as.numeric(year), 
         season = factor(paste0(as.integer(year-0.5), "-", as.integer(year+0.5))))


## save
save(pop.seasonal, file = "./01-Data/02-Analytic-Data/pop_seasonal.rds")

## clean environment
rm(list = ls())
gc()







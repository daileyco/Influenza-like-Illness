# Create final analytic dataset

# load datasets from seperate sources
load("./01-Data/02-Analytic-Data/ili_ei.rds")
load("./01-Data/02-Analytic-Data/pop_seasonal.rds")
load("./01-Data/02-Analytic-Data/acs_prop_flowtype.rds")


# packages
library(dplyr)


# merge ili epidemic intensity df with seasonal population estimates
# one to one merge
ei.df <- full_join(ili.ei, 
                   pop.seasonal %>% 
                     select(-year), 
                   by = c("region", "season"))


# merge epidemic intensity&population dataset with the commuting data
# many to one merge
## commuting data is less temporally resolved with 5-year estimates
# epidemic intensity data for the 2015-2016 is merged with 2016-2020 commuting data

ei.df <- full_join(ei.df %>% 
                     mutate(period = case_when(as.numeric(substr(season,1,4)) <= 2014 ~ "2011-2015", 
                                               as.numeric(substr(season,1,4)) <= 2020 ~ "2016-2020", 
                                               TRUE ~ NA)) , 
                   acs.p.flowtype %>% 
                     select(-`State FIPS Residence`) %>%
                     select(region=`State Residence`, everything()), 
                   by = c("region", "period"))


# package all data into one save file
save(ei.df, 
     ili.ei, 
     pop.seasonal, 
     acs.p.flowtype, 
     file = "./01-Data/02-Analytic-Data/ei_df.rdata")


# clean environment
rm(list = ls())
gc()



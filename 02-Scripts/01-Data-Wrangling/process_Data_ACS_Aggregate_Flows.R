# ACS Data Processing II

load("./01-Data/01-Processed-Data/acs.rds")

library(dplyr)
library(tidyr)


# aggregate to flow type state level
# focal location is resident location
# separate for each time period, the two original datasets
acs <- acs %>%
  group_by(`State FIPS Residence`, `State Residence`, flow.type, period) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`, na.rm = TRUE)) %>%
  ungroup()


# calculate total workers from a given resident location
# use total to calculate proportions for each flow type
acs <- acs %>%
  group_by(`State FIPS Residence`, `State Residence`, period) %>%
  mutate(total.workers = sum(`Workers in Commuting Flow`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(proportion.total.workers = `Workers in Commuting Flow` / total.workers)

# widen dataset to set up for merge with epidemic intensity dataset
acs.p.flowtype <- acs %>% 
  select(-total.workers) %>%
  pivot_wider(names_from = flow.type, values_from = c(`Workers in Commuting Flow`, proportion.total.workers))


save(acs.p.flowtype, file = "./01-Data/02-Analytic-Data/acs_prop_flowtype.rds")

rm(list = ls())
gc()
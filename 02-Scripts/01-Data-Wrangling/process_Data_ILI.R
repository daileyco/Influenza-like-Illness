# ILI Data Processing


# Read FluView data
ili <- read.csv("./01-Data/00-Raw-Data/Flu/state_ili.csv")

# # Examine structure / format / variables
# str(ili)
# summary(ili)

# Select variables
ili <- ili[,c("region", "week_start", "year", "week", "ilitotal", "num_of_providers", "total_patients")]


# Cleaning
library(dplyr)
library(magrittr)

## formatting, calculations
ili %<>% 
  mutate_at(1, ~factor(.)) %>% 
  mutate_at(2, ~as.Date(., format = "%Y-%m-%d")) %>% 
  mutate(ili_incidence_p100 = ilitotal / total_patients * 100)

# ## re-examine structure / format / variables
# str(ili)
# summary(ili)


# # Evaluate missingness
# missingness <- ili %>% 
#   group_by(region) %>% 
#   summarise(n.missing = sum(is.na(ili_incidence_p100)), 
#             n = n()) %>% 
#   mutate(p.missing = n.missing / n) %>% 
#   ungroup() %>% 
#   
#   arrange(desc(p.missing))
# 
# # View(missingness)

## Florida is completely missing, as is Commonwealth of the Northern Mariana Islands
## everywhere else looks manageable

# What to do with non-mainland / non-contiguous 48?
















## Florida data

library(readxl)

ili.fl <- read_xlsx("./01-Data/00-Raw-Data/Flu/Florida_ILINet Data 2011-2021.xlsx", sheet = "Data")

### need to reformat dataframe, wide to long wrt season

ili.fl.totili <- ili.fl[2:nrow(ili.fl), c(1, which(ili.fl[1,]=="Total ILI"))] %>% 
  setNames(., nm = c("week", paste("ilitotal", names(ili.fl)[which(substr(names(ili.fl), 1,1)==2)], sep = "_")))

ili.fl.patients <- ili.fl[2:nrow(ili.fl), c(1, which(ili.fl[1,]=="Total Patients Seen"))] %>%
  setNames(., nm = c("week", paste("total_patients", names(ili.fl)[which(substr(names(ili.fl), 1,1)==2)], sep = "_")))


library(tidyr)

ili.fl.totili.long <- tidyr::pivot_longer(ili.fl.totili, 2:11, names_to = "year", names_prefix = "ilitotal_", values_to = "ilitotal")
ili.fl.patients.long <- tidyr::pivot_longer(ili.fl.patients, 2:11, names_to = "year", names_prefix = "total_patients_", values_to = "total_patients")


ili.fl <- full_join(ili.fl.totili.long, ili.fl.patients.long, by = c("week", "year")) %>% 
  mutate(year = ifelse(as.numeric(week) >= 40, substr(year, 1,4), paste0(20,substr(year, 6,7)))) %>%
  mutate_all(~as.numeric(.)) %>%
  mutate(region = "Florida")







# Incorporate Florida data


ili <- rbind(ili[which(ili$region!="Florida"),c("region", "year", "week", "week_start", "ilitotal", "total_patients")], 
             full_join(ili[which(ili$region=="Florida"), c("region", "year", "week", "week_start")], ili.fl, by = c("region", "year", "week"))) %>% 
  mutate(ili_incidence_p100 = ilitotal / total_patients * 100)

ili <- ili[which(!is.na(ili$week_start)),]


# # Evaluate missingness
# missingness <- ili %>% 
#   group_by(region) %>% 
#   summarise(n.missing = sum(is.na(ili_incidence_p100)), 
#             n = n()) %>% 
#   mutate(p.missing = n.missing / n) %>% 
#   ungroup() %>% 
#   
#   arrange(desc(p.missing))
# 
# # View(missingness)



# Decision to combine New York and New York City

ili.ny <- ili %>% 
  filter(region %in% c("New York", "New York City")) %>% 
  group_by(week_start, week, year) %>% 
  summarise(ilitotal = sum(ilitotal), total_patients = sum(total_patients), region = "New York") %>% 
  ungroup() %>% 
  mutate(ili_incidence_p100 = ilitotal / total_patients * 100)


ili <- ili %>% 
  filter(!region %in% c("New York", "New York City")) %>% 
  rbind(., ili.ny)




# # Calculate average incidence for entire nation
# ili.avg <- ili %>% 
#   filter(!is.na(ilitotal) & !is.na(total_patients)) %>% 
#   group_by(week_start) %>% 
#   summarise(ilitotal = sum(ilitotal, na.rm = T), total_patients = sum(total_patients, na.rm = T)) %>% 
#   ungroup() %>% 
#   
#   mutate(ili_incidence_p100 = ilitotal / total_patients * 100) %>% 
#   arrange(week_start)



## scaffolding dataset to identify implicitly missing data
scaffold <- expand.grid(region = unique(ili$region), 
                        week_start = seq(min(ili$week_start), max(ili$week_start), by = "weeks"))



ili.extensive <- left_join(scaffold, ili) %>% arrange(region, week_start)

# Evaluate missingness
missingness <- ili.extensive %>% 
  group_by(region) %>% 
  summarise(n.missing = sum(is.na(ili_incidence_p100)), 
            n = n()) %>% 
  mutate(p.missing = n.missing / n) %>% 
  ungroup() %>% 
  
  arrange(desc(p.missing))

# View(missingness)



# ## pivot wider to have matrix of ili incidence, columns for each region
# library(tidyr)
# ili.wide <- ili.extensive[,c("region", "week_start", "ili_incidence_p100")] %>% pivot_wider(names_from = region, values_from = ili_incidence_p100)




ili <- ili.extensive


save(ili, file = "./01-Data/01-Processed-Data/ili.rds")

rm(list = ls())
gc()

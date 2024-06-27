# epidemic intensity calculations


load("./01-Data/01-Processed-Data/ili.rds")

library(dplyr)
library(tidyr)
library(lubridate)

ili.seasonal.distributions <- ili %>% 
  filter(!is.na(week) & !region%in%c("Commonwealth of the Northern Mariana Islands", "Virgin Islands")) %>%
  mutate(region = droplevels(region), 
         year = year(week_start), #weird entries/errors for year values with week start at end of december (e.g., 12/30/12); maybe cuz epiweek one?
         season = ifelse(week<40 & month(week_start)!=12, 
                         paste0(year-1, "-", year), 
                         paste0(year, "-", year+1)
                         ))

scaffold <- expand.grid(region = unique(ili.seasonal.distributions$region), 
                        # season = unique(ili.seasonal.distributions$season), 
                        week_start = unique(ili.seasonal.distributions$week_start)) %>%
  mutate(week = epiweek(week_start), 
         year = year(week_start)) %>%
  mutate(season = ifelse(week<40 & month(week_start)!=12, 
                         paste0(year-1, "-", year), 
                         paste0(year, "-", year+1)))


ili.seasonal.distributions <- full_join(scaffold, 
                                        ili.seasonal.distributions, 
                                        by = c("region", "week_start", "week", "year", "season"))


# missingness <- ili.seasonal.distributions %>%
#   group_by(region, season) %>%
#   summarise(n = n(),
#             n.miss = sum(is.na(ilitotal))) %>%
#   ungroup() %>%
#   mutate(p.miss = n.miss / n) %>%
#   select(-n, -n.miss) %>%
#   pivot_wider(names_from = season,
#               values_from = p.miss) %>%
#   arrange(region)
# 
# View(missingness)
# 
# # florida missing 2010-11 season completely
# # puerto rico missing 2010-11, 2011-12, and almost all 2012-13
# # dataset is truncated for 2020-21 season due to time of download likely
# # # trim data to span from 2011-12 through 2019-2020 seasons
# # # # matches with the commuting data coverage as well
# # # # will need to see how missing 2 seasons of data for PR will impact analyses



ili.seasonal.distributions <- ili.seasonal.distributions %>%
  filter(!season%in%c("2010-2011", "2020-2021")) %>%
  group_by(season, region) %>%
  mutate(n = n()) %>%
  ungroup()



#### got to split week 53


ili.seasonal.distributions <- bind_rows(ili.seasonal.distributions %>% filter(n!=53 | (n==53 & !week%in%c(52,53,1))), 
                                        
                                        ili.seasonal.distributions %>% filter(n==53 & week%in%c(52,53,1)) %>%
                                          group_by(region) %>%
                                          mutate(ilitotal53 = ilitotal[which(week==53)]) %>% 
                                          ungroup() %>%
                                          filter(week!=53) %>%
                                          mutate(ilitotal = ilitotal + ilitotal53/2)
                                        ) %>%
  arrange(week_start) %>% 
  group_by(season, region) %>%
  mutate(n2 = n()) %>%
  ungroup()







ili.seasonal.distributions <- ili.seasonal.distributions %>% 
  group_by(season, region) %>%
  mutate(ili.cumulative = sum(ilitotal),
         tp.cumulative = sum(total_patients),
         n = n(), 
         n.miss = sum(is.na(ilitotal))) %>%
  ungroup() %>% 
  mutate(p.cili = ilitotal / ili.cumulative, 
         p.cpatients = total_patients / tp.cumulative) 




ili.ei.units <- ili.seasonal.distributions %>% 
  group_by(season, region) %>%
  mutate(ei.unit = p.cili * log(p.cili)) %>% 
  mutate(ei.unit = ifelse(ilitotal==0 | total_patients==0, 0, ei.unit), 
         maxili = ilitotal==max(ilitotal),
         peak_wk = week[which(maxili)][1]) %>%
  ungroup()


ili.ei <- ili.ei.units %>% 
  group_by(season, region) %>%
  summarise(ei = (-1*sum(ei.unit))^-1, 
            peak_wk = unique(peak_wk)) %>%
  ungroup() %>%
  # mutate(ei.unscaled = ei) %>%
  mutate(ei = ei - min(ei, na.rm = TRUE)) %>%
  mutate(ei = ei / max(ei, na.rm = TRUE)) %>% 
  mutate(peak_wk = ifelse(peak_wk>=40, peak_wk-52, peak_wk))


ili <- ili.ei.units %>%
  group_by(season, region) %>%
  mutate(ei = (-1*sum(ei.unit))^-1) %>%
  ungroup() %>%
  group_by(season) %>%
  mutate(ei = ei - min(ei, na.rm = TRUE)) %>%
  mutate(ei = ei / max(ei, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sqrt.p.cili = sqrt(p.cili),
         ei.unit1 = ei.unit*-1,
         sqrt.ei.unit1 = sqrt(ei.unit*-1))



save(ili.ei, file = "./01-Data/02-Analytic-Data/ili_ei.rds")

save(ili, file = "./01-Data/02-Analytic-Data/ili.rds")


rm(list = ls())
gc()





# quick exploration of distributions
# sqrt transformation seems sufficient to normalize
# ei.unit may be preferable over p.cili as not bounded on one end
# ei.unit1 would be absolute value of ei.unit (similar direction interpretation as ei)
# 
# hist(ili$p.cili, breaks = 30)
# summary(ili$p.cili)
# quantile(ili$p.cili, na.rm = T)
# 
# hist(sqrt(ili$p.cili), breaks = 30)
# summary(sqrt(ili$p.cili))
# quantile(sqrt(ili$p.cili), na.rm = T)
# 
# hist(ili$ei.unit, breaks = 30)
# summary(ili$ei.unit)
# quantile(ili$ei.unit, na.rm = T)
# 
# hist(ili$ei.unit1, breaks = 30)
# summary(ili$ei.unit1)
# quantile(ili$ei.unit1, na.rm = T)
# 
# hist(sqrt(ili$ei.unit1), breaks = 30)
# summary(sqrt(ili$ei.unit1))
# quantile(sqrt(ili$ei.unit1), na.rm = T)
# ACS Data Processing

library(readxl)

#table for 2011 - 2015
acs1115 <- read_xlsx(path = "./01-Data/00-Raw-Data/Commuting/table1-11-15.xlsx", 
                     skip = 7, 
                     col_names = c("State FIPS Residence", "County FIPS Residence", 
                                   "State Residence", "County Residence",  
                                   "State FIPS Work", "County FIPS Work", 
                                   "State Work", "County Work",  
                                   "Workers in Commuting Flow", "Margin of Error"))
#drop footer notes
acs1115 <- acs1115[-c(nrow(acs1115)-1, nrow(acs1115)),]


#table for 2016-2020
acs1620 <- read_xlsx(path = "./01-Data/00-Raw-Data/Commuting/table1-16-20.xlsx", 
                     skip = 8, 
                     col_names = c("State FIPS Residence", "County FIPS Residence", 
                                   "State Residence", "County Residence",  
                                   "State FIPS Work", "County FIPS Work", 
                                   "State Work", "County Work",  
                                   "Workers in Commuting Flow", "Margin of Error"))
#drop footer notes
acs1620 <- acs1620[-c({nrow(acs1620)-3}:nrow(acs1620)),]






#combine state and county fips
#categorize flow type according to origins and destinations as 
##international, 
##interstate, 
##intrastate (but intercounty), 
##or intracounty

library(dplyr)

acs1115 <- acs1115 %>%
  mutate(`State FIPS Work` = substr(`State FIPS Work`, 2,3)) %>%
  mutate(`FIPS Residence` = paste0(`State FIPS Residence`, `County FIPS Residence`), 
         `FIPS Work` = paste0(`State FIPS Work`, `County FIPS Work`)) %>% 
  mutate(flow.type = case_when(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S.") ~ "International",
                               `FIPS Residence`==`FIPS Work` ~ "Intracounty", 
                               `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
                               TRUE ~ "Interstate")) %>%
  mutate(flow.type = ifelse(`State Residence`=="District of Columbia" & `State Work`=="District of Columbia", 
                            "Intrastate", 
                            flow.type)) %>%
  mutate(`State FIPS Work` = ifelse(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S."), 
                                    NA,
                                    `State FIPS Work`), 
         `State Work` = ifelse(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S."), 
                               "International", 
                               `State Work`))

acs1620 <- acs1620 %>%
  mutate(`State FIPS Work` = substr(`State FIPS Work`, 2,3)) %>%
  mutate(`FIPS Residence` = paste0(`State FIPS Residence`, `County FIPS Residence`), 
         `FIPS Work` = paste0(`State FIPS Work`, `County FIPS Work`)) %>% 
  mutate(flow.type = case_when(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S.") ~ "International",
                               `FIPS Residence`==`FIPS Work` ~ "Intracounty", 
                               `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
                               TRUE ~ "Interstate")) %>%
  mutate(flow.type = ifelse(`State Residence`=="District of Columbia" & `State Work`=="District of Columbia", 
                            "Intrastate", 
                            flow.type)) %>%
  mutate(`State FIPS Work` = ifelse(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S."), 
                                    NA,
                                    `State FIPS Work`), 
         `State Work` = ifelse(`State Work`%in%c("Canada", "Mexico", "Other workplace outside of the U.S."), 
                               "International", 
                               `State Work`))





#aggregate to state level

acs1115.state <- acs1115 %>%
  group_by(`State FIPS Residence`, `State Residence`, `State FIPS Work`, `State Work`, flow.type) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>% 
  ungroup()


acs1620.state <- acs1620 %>%
  group_by(`State FIPS Residence`, `State Residence`, `State FIPS Work`, `State Work`, flow.type) %>%
  summarise(`Workers in Commuting Flow` = sum(`Workers in Commuting Flow`)) %>% 
  ungroup()




# # all acs1115 fips are in acs1620
# View(acs1115.state[which(!(acs1115.state$`State FIPS Residence` %in% acs1620.state$`State FIPS Residence`)),])
# View(acs1115.state[which(!(acs1115.state$`State FIPS Work` %in% acs1620.state$`State FIPS Work`)),])
# # and vice versa
# View(acs1620.state[which(!(acs1620.state$`State FIPS Residence` %in% acs1115.state$`State FIPS Residence`)),])
# View(acs1620.state[which(!(acs1620.state$`State FIPS Work` %in% acs1115.state$`State FIPS Work`)),])

# differences in rows must be differences in combinations/flows



#all combos
# scaffold <- expand.grid(`State FIPS Residence` = unique(c(acs1115.state$`State FIPS Residence`, acs1115.state$`State FIPS Work`, acs1620.state$`State FIPS Residence`, acs1620.state$`State FIPS Work`)), 
#                         `State FIPS Work` = unique(c(acs1115.state$`State FIPS Residence`, acs1115.state$`State FIPS Work`, acs1620.state$`State FIPS Residence`, acs1620.state$`State FIPS Work`)))

scaffold <- expand.grid(`State FIPS Residence` = unique(c(acs1115.state$`State FIPS Residence`, acs1620.state$`State FIPS Residence`)), 
                        `State FIPS Work` = unique(c(acs1115.state$`State FIPS Work`, acs1620.state$`State FIPS Work`)))

#duplicate rows with same state for intracounty and intrastate
#add flow type
scaffold <- bind_rows(scaffold %>% 
                        mutate(flow.type = case_when(is.na(`State FIPS Work`) ~ "International", 
                                                     `State FIPS Residence`==`State FIPS Work` ~ "Intrastate", 
                                                     TRUE ~ "Interstate")), 
                      scaffold %>% 
                        filter(`State FIPS Residence`==`State FIPS Work`) %>%
                        filter(!`State FIPS Residence`%in%c("District of Columbia")) %>%
                        mutate(flow.type = "Intracounty"))



#fill in state names
scaffold$`State Residence` <- acs1115.state$`State Residence`[match(scaffold$`State FIPS Residence`, acs1115.state$`State FIPS Residence`)]
scaffold$`State Work` <- acs1115.state$`State Work`[match(scaffold$`State FIPS Work`, acs1115.state$`State FIPS Work`)]



acs1115.state.full <- full_join(scaffold, acs1115.state, by = c("State FIPS Residence", "State Residence", "State FIPS Work", "State Work", "flow.type")) %>%
  mutate(missing.flag1115 = ifelse(is.na(`Workers in Commuting Flow`), 1, 0), 
         period = "2011-2015")

acs1620.state.full <- full_join(scaffold, acs1620.state, by = c("State FIPS Residence", "State Residence", "State FIPS Work", "State Work", "flow.type")) %>%
  mutate(missing.flag1620 = ifelse(is.na(`Workers in Commuting Flow`), 1, 0), 
         period = "2016-2020")


acs <- bind_rows(acs1115.state.full, acs1620.state.full) %>%
  arrange(`State FIPS Residence`, `State Residence`, `State FIPS Work`, `State Work`, flow.type) %>%
  select(`State FIPS Residence`, `State Residence`, `State FIPS Work`, `State Work`, flow.type, period, `Workers in Commuting Flow`)


# #simple average between data sets
# # not really necessary i don't think
# # can just use different values depending on flu season
# acs <- bind_rows(acs1115.state.full, acs1620.state.full) %>%
#   group_by(`State FIPS Residence`, `State Residence`, `State FIPS Work`, `State Work`) %>%
#   summarise(`Workers in Commuting Flow` = mean(`Workers in Commuting Flow`, na.rm = TRUE),
#             across(matches("missing"), ~sum(.x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(`Workers in Commuting Flow` = ifelse(is.nan(`Workers in Commuting Flow`), NA, `Workers in Commuting Flow`))


acs <- acs %>% 
  mutate(across(1:6, ~factor(.x)))



save(acs, file = "./01-Data/01-Processed-Data/acs.rds")

rm(list = ls())
gc()
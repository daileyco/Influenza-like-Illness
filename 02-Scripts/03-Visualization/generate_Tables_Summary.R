


load("./01-Data/02-Analytic-Data/ei_df.rdata")



library(dplyr)


ei.df <- ei.df %>%
  rename(state_area_km2_land = state_area_km2) %>% 
  mutate(state_area_km2 = state_area_km2_land + state_area_km2_water)


names(ei.df) <- sub(" ", "_", names(ei.df))

# myvars <- names(ei.df)[c(4,19,23:24,20,25,21:22,8,17,18,9,14,10,15,11,16,12:13)]

myvars <- names(ei.df)[which(!names(ei.df)%in%c("season", "region", "ei", "period", "State_FIPS Residence"))]



# ei.df <- ei.df %>% 
#   mutate(eitert = findInterval(ei, 
#                                quantile(ei,probs = 0:3/3, na.rm = T), 
#                                all.inside = TRUE))
# 
# 
# 
# table.summary <- create.Table.1(myvars, "eitert", ei.df)








table.summary.raw <- 
  apply(ei.df[,which(names(ei.df)%in%c("ei", myvars))], 
        MARGIN = 2, 
        (\(x){data.frame(mean = mean(x, na.rm = T), 
                         sd = sd(x, na.rm = T), 
                         median = median(x, na.rm = T), 
                         min = min(x, na.rm = T),
                         max = max(x, na.rm = T), 
                         nmiss = sum(is.na(x)), 
                         n = length(x))})) %>% 
  bind_rows(.id = "var") %>%
  mutate(type = "raw")




ei.df <- ei.df %>%
  # rename(state_area_km2_land = state_area_km2) %>%
  # mutate(state_area_km2 = state_area_km2_land + state_area_km2_water) %>%
  mutate(across(c(ei), ~sqrt(.x)), 
         across(c(ratio.si,
                  ratio.ls,
                  ratio.li
                  # , 
                  # state.crowding.dailychange,
                  # state.patchiness.dailychange
                  ), ~sqrt(sqrt(.x))), 
         across(c(state.crowding.dailychange, 
                  state.patchiness.dailychange, 
                  state.crowding.dailychangeratio, 
                  state.patchiness.dailychangeratio), 
                ~((.x-min(.x))/(max(.x-min(.x))))^(1/4)),
         across(c(county.pop.mean, 
                  county.popday.mean,
                  population, 
                  populationday,
                  state.crowding,
                  state.patchiness, 
                  state.crowding.day,
                  state.patchiness.day,  
                  # state.crowding.dailychangeratio,
                  # state.patchiness.dailychangeratio,
                  Total.Workers, 
                  Internal, 
                  `Short_Distance`, 
                  `Long_Distance`, 
                  distance_mean_km, 
                  distance_mean_km_nozeros, 
                  county_area_km2_mean, 
                  state_area_km2,
                  state_area_km2_land,
                  state_area_km2_water,
                  pop_density, 
                  pop_density2), ~log(.x))) 

table.summary.transformed <- 
  apply(ei.df[,which(names(ei.df)%in%c("ei", myvars))], 
        MARGIN = 2, 
        (\(x){data.frame(mean = mean(x, na.rm = T), 
                         sd = sd(x, na.rm = T), 
                         median = median(x, na.rm = T), 
                         min = min(x, na.rm = T),
                         max = max(x, na.rm = T), 
                         nmiss = sum(is.na(x)), 
                         n = length(x))})) %>% 
  bind_rows(.id = "var") %>%
  mutate(type = "transformed")











ei.df <- ei.df %>%
  mutate(across(c(ratio.si, 
                  ratio.ls, 
                  ratio.li,
                  state.crowding.dailychange,
                  state.patchiness.dailychange, 
                  county.pop.mean,
                  county.popday.mean,
                  population, 
                  populationday,
                  state.crowding,
                  state.patchiness,
                  state.crowding.day,
                  state.patchiness.day,  
                  state.crowding.dailychangeratio,
                  state.patchiness.dailychangeratio,
                  Total.Workers, 
                  Internal, 
                  `Short_Distance`, 
                  `Long_Distance`, 
                  Internal_prop, 
                  `Short_Distance_prop`, 
                  `Long_Distance_prop`, 
                  distance_mean_km, 
                  distance_mean_km_nozeros, 
                  county_area_km2_mean, 
                  state_area_km2,  
                  state_area_km2_land,
                  state_area_km2_water, 
                  pop_density, 
                  pop_density2), ~c(scale(.x, center = TRUE, scale = TRUE)), .names = "{.col}"))


table.summary.transformed2 <- 
  apply(ei.df[,which(names(ei.df)%in%c("ei", myvars))], 
        MARGIN = 2, 
        (\(x){data.frame(mean = mean(x, na.rm = T), 
                         sd = sd(x, na.rm = T), 
                         median = median(x, na.rm = T), 
                         min = min(x, na.rm = T),
                         max = max(x, na.rm = T), 
                         nmiss = sum(is.na(x)), 
                         n = length(x))})) %>% 
  bind_rows(.id = "var") %>%
  mutate(type = "centered/scaled")






table.summary <- bind_rows(table.summary.raw, table.summary.transformed, table.summary.transformed2) %>%
  mutate(order = match(var, c("ei", myvars))) %>%
  arrange(order, type) %>%
  mutate(across(where(is.numeric), ~trimws(format(round(.x,3), scientific = F, drop0trailing = T)))) %>%
  mutate(`Mean (SD)` = paste0(mean, " (", sd, ")"), 
         `Median [Min,Max]` = paste0(median, " [", min, ", ", max, "]"), 
         `Missing n (%)` = ifelse(nmiss==0, "", nmiss), 
         Transformed = type) %>%
  select(-mean, -sd, -median, -min, -max, -nmiss, -n, -type, -order)







save(list = ls(pattern = "^table[.]"), 
     file = "./03-Output/01-Tables/tables_summaries.rdata")




rm(list=ls())
gc()


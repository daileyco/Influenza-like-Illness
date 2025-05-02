


## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)





## setup

ei.df <- ei.df %>%
  rename(state_area_km2_land = state_area_km2) %>% 
  mutate(state_area_km2 = state_area_km2_land + state_area_km2_water)




# these <- c(3:5,8:ncol(ei.df))
these <- which(!names(ei.df)%in%c("season", "region", "period", "State FIPS Residence"))

## plot

if(!dir.exists("./03-Output/02-Figures/histograms")){dir.create("./03-Output/02-Figures/histograms")}
png(filename = "./03-Output/02-Figures/histograms/histogram%03d.png", width = 8, height = 4.5, pointsize = 10, units = "in", res = 300)

par(mfrow = c(2,2))
  
for(i in 1:length(these)){
  
  hist(unlist(ei.df[,these[i]]), main = names(ei.df)[these[i]])
  hist(log(unlist(ei.df[,these[i]])), main = "log")
  hist(sqrt(unlist(ei.df[,these[i]])), main = "sqrt")
  hist(sqrt(sqrt(unlist(ei.df[,these[i]]))), main = "qtrt")
  # readline()
}

dev.off()





## scatters



these <- which(!names(ei.df)%in%c("season", "region", "ei", "period", "State FIPS Residence"))

## plot

if(!dir.exists("./03-Output/02-Figures/scatters")){dir.create("./03-Output/02-Figures/scatters")}

png(filename = "./03-Output/02-Figures/scatters/scatter%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

# par(mfrow = c(2,2))

for(i in 1:length(these)){
  
  plot(unlist(ei.df[,these[i]]), ei.df$ei, main = paste0("EI v ", names(ei.df)[these[i]]), pch = 16, col = rgb(0,0,0,0.25))
  
  loess.fit <- loess(ei.df$ei~unlist(ei.df[,these[i]]))
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
  

}

dev.off()







## transform


ei.df <- ei.df %>%
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
                  `Short Distance`, 
                  `Long Distance`, 
                  distance_mean_km, 
                  distance_mean_km_nozeros, 
                  county_area_km2_mean, 
                  state_area_km2,
                  state_area_km2_land,
                  state_area_km2_water,
                  pop_density, 
                  pop_density2), ~log(.x)))



## scatters


these <- which(!names(ei.df)%in%c("season", "region", "ei", "period", "State FIPS Residence"))

## plot


if(!dir.exists("./03-Output/02-Figures/scatters/transformed")){dir.create("./03-Output/02-Figures/scatters/transformed")}
png(filename = "./03-Output/02-Figures/scatters/transformed/scatter_transformed%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

# par(mfrow = c(2,2))

for(i in 1:length(these)){
  
  plot(unlist(ei.df[,these[i]]), ei.df$ei, main = paste0("EI v ", names(ei.df)[these[i]]), pch = 16, col = rgb(0,0,0,0.25))
  
  loess.fit <- loess(ei.df$ei~unlist(ei.df[,these[i]]))
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
  
  
}

dev.off()














## save



## clean environment
rm(list=ls())
gc()




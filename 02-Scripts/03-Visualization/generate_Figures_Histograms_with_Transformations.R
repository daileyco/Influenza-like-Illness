


## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)





## setup

these <- c(3:5,8:ncol(ei.df))

## plot


# svg(filename = "./03-Output/02-Figures/epidemic_intensity_scatters.svg", width = 16, height = 9, pointsize = 10)
png(filename = "./03-Output/02-Figures/histogram%03d.png", width = 8, height = 4.5, pointsize = 10, units = "in", res = 300)

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



these <- c(4:5,8:ncol(ei.df))

## plot


# svg(filename = "./03-Output/02-Figures/epidemic_intensity_scatters.svg", width = 16, height = 9, pointsize = 10)
png(filename = "./03-Output/02-Figures/scatter%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

# par(mfrow = c(2,2))

for(i in 1:length(these)){
  
  plot(unlist(ei.df[,these[i]]), ei.df$ei, main = paste0("EI v ", names(ei.df)[these[i]]), pch = 16, col = rgb(0,0,0,0.25))
  
  loess.fit <- loess(ei.df$ei~unlist(ei.df[,these[i]]))
  
  lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
  

}

dev.off()







## transform


ei.df <- ei.df %>%
  mutate(across(c(ei,ratio.si), ~sqrt(.x)), 
         across(c(), ~sqrt(sqrt(.x))), 
         across(c(population, 
                  ratio.ls, 
                  Total.Workers, 
                  Internal, 
                  `Short Distance`, 
                  `Long Distance`, 
                  distance_mean_km, 
                  distance_mean_km_nozeros, 
                  county_area_km2_mean, 
                  state_area_km2,
                  state_area_km2_water,
                  pop_density, 
                  pop_density2), ~log(.x)))



## scatters



these <- c(4:5,8:ncol(ei.df))

## plot


# svg(filename = "./03-Output/02-Figures/epidemic_intensity_scatters.svg", width = 16, height = 9, pointsize = 10)
png(filename = "./03-Output/02-Figures/scatter_transformed%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

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




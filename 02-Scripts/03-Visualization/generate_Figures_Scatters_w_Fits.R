# script to make residual plots with fitted curves

## load data
load("./01-Data/02-Analytic-Data/regressions.rdata")


## packages
library(dplyr)
library(lme4)

## helper functions



## set up

ei.df <- ei.df %>%
  filter(complete.cases(.))

ei.df$fitme <- predict(fitme)

for(i in 1:length(myvars)){
  ei.df[,paste0("fit",i)] <- predict(get(paste0("fit",i)))
}


ei.df <- ei.df %>%
  mutate(across(matches("^fit[0-9]{1,2}$"), ~ei-.x, .names = "{.col}_resid"))







## plot


# svg(filename = "./03-Output/02-Figures/epidemic_intensity_scatters.svg", width = 16, height = 9, pointsize = 10)
png(filename = "./03-Output/02-Figures/scatter_modelcurve%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

# par(mfrow = c(2,2))

for(i in 1:length(myvars)){
  
  plot(unlist(ei.df[,myvars[i]]), ei.df$ei, 
       main = paste0("EI v ", myvars[i]), pch = 16, col = rgb(0,0,0,0.25), 
       xlab = myvars[i], 
       ylab = "Epidemic Intensity")
  
  
  
  intercept <- coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,mean(ei.df$population))
  coefs <- coef(summary(get(paste0("fit", i))))[3:4,1]
  
  curve(intercept+coefs[1]*x+coefs[2]*x^2, 
        from = par('usr')[1], 
        to = par('usr')[2], 
        lty = 5, 
        lwd = 3,
        col = "orchid", 
        add = TRUE)
  
  # coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]
  # 
  # curve(intercept+coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3, 
  #       from = par('usr')[1], 
  #       to = par('usr')[2], 
  #       lty = 5, 
  #       lwd = 3,
  #       col = "orchid", 
  #       add = TRUE)
  
  
}

dev.off()





## save


## clean environment
rm(list=ls())
gc()










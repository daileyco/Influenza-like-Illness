# Create scatter plots figure of epidemic intensity against covariates

## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)



## figure
svg(filename = "./03-Output/02-Figures/epidemic_intensity_scatters.svg", width = 16, height = 9, pointsize = 10)

par(mfrow = c(3,2))
par(mar = c(3.1,4.1,0.5,0.5))

### vs population
plot(sqrt(ei)~log(population), data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = FALSE)

title(xlab = "Population (millions)", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = log(c(0.5,1,2,4,8,16,32)*1000000), 
     labels = F)
axis(1, 
     at = log(c(0.5,1,2,4,8,16,32)*1000000), 
     labels = c(0.5,1,2,4,8,16,32), 
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~log(population), data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(0,1), xpd = T, cex = 1, font = 2)






### vs internal
plot(sqrt(ei)~Internal_prop, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = F)

title(xlab = "Internal Commutes Proportion", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = c(50,60,70,80,90,100), 
     labels = F)
axis(1, 
     at = c(50,60,70,80,90,100),  
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~Internal_prop, data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)







### vs short dist
plot(sqrt(ei)~`Short Distance_prop`, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = F)

title(xlab = "Short Distance Commutes Proportion", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = c(0,10,20,30,40,50), 
     labels = F)
axis(1, 
     at = c(0,10,20,30,40,50),  
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~ei.df$`Short Distance_prop`, data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)







### vs long dist
plot(sqrt(ei)~`Long Distance_prop`, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = F)

title(xlab = "Long Distance Commutes Proportion", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = c(0,0.5,1,1.5,2,2.5,3), 
     labels = F)
axis(1, 
     at = c(0,0.5,1,1.5,2,2.5,3),  
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~ei.df$`Long Distance_prop`, data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "D", adj = c(0,1), xpd = T, cex = 1, font = 2)













### vs ratio si
plot(sqrt(ei)~log(ratio.si), data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = F)

title(xlab = "log(Ratio of Short-distance to Internal Commutes)", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = (-6:0), 
     labels = F)
axis(1, 
     at = (-6:0),  
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~log(ratio.si), data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "E", adj = c(0,1), xpd = T, cex = 1, font = 2)






### vs ratio ls
plot(sqrt(ei)~log(ratio.ls), data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
     xlab = "", 
     ylab = "", 
     axes = F)

title(xlab = "log(Ratio of Long-distance to Short-distance Commutes)", 
      line = 1.5)

title(ylab = "Epidemic Intensity", 
      line = 2.5)
axis(1, 
     at = (-6:1), 
     labels = F)
axis(1, 
     at = (-6:1),  
     lwd = 0, 
     line = -0.5, 
     cex.axis =0.7)
axis(2, 
     at = seq(0,1,by=0.2),
     labels = F)
axis(2, 
     at = seq(0,1,by=0.2), 
     las = 2, 
     lwd = 0, 
     line = -0.25, 
     cex.axis =0.7)

loess.fit <- loess(sqrt(ei)~log(ratio.ls), data = ei.df)

lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "F", adj = c(0,1), xpd = T, cex = 1, font = 2)















dev.off()



## clean environment
rm(list = ls())
gc()




## old



# 
# 
# 
# 
# 
# 
# ### vs international commutes
# plot(ei~proportion.total.workers_International, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
#      xlab = "", 
#      ylab = "", 
#      axes = F)
# 
# title(xlab = "International Commutes Proportion", 
#       line = 1.5)
# 
# title(ylab = "Epidemic Intensity", 
#       line = 2.5)
# axis(1, 
#      at = c(0.0001,0.0005,0.001,0.0015), 
#      labels = F)
# axis(1, 
#      at = c(0.0001,0.0005,0.001,0.0015),  
#      lwd = 0, 
#      line = -0.5, 
#      cex.axis =0.7)
# axis(2, 
#      at = seq(0,1,by=0.2),
#      labels = F)
# axis(2, 
#      at = seq(0,1,by=0.2), 
#      las = 2, 
#      lwd = 0, 
#      line = -0.25, 
#      cex.axis =0.7)
# 
# loess.fit <- loess(ei~proportion.total.workers_International, data = ei.df)
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
# box()
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "B", adj = c(0,1), xpd = T, cex = 1, font = 2)
# 
# 
# ### vs interstate commutes
# plot(ei~proportion.total.workers_Interstate, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
#      xlab = "", 
#      ylab = "", 
#      axes = F)
# 
# title(xlab = "Interstate Commutes Proportion", 
#       line = 1.5)
# 
# title(ylab = "Epidemic Intensity", 
#       line = 2.5)
# axis(1, 
#      at = c(0,0.05,0.1,0.15,0.2,0.25), 
#      labels = F)
# axis(1, 
#      at = c(0,0.05,0.1,0.15,0.2,0.25),  
#      lwd = 0, 
#      line = -0.5, 
#      cex.axis =0.7)
# axis(2, 
#      at = seq(0,1,by=0.2),
#      labels = F)
# axis(2, 
#      at = seq(0,1,by=0.2), 
#      las = 2, 
#      lwd = 0, 
#      line = -0.25, 
#      cex.axis =0.7)
# 
# loess.fit <- loess(ei~proportion.total.workers_Interstate, data = ei.df)
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
# box()
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "C", adj = c(0,1), xpd = T, cex = 1, font = 2)
# 
# 
# ### vs intrastate commutes
# plot(ei~proportion.total.workers_Intrastate, data = ei.df, pch = 16, col = rgb(0,0,0,0.25), 
#      xlab = "", 
#      ylab = "", 
#      axes = F)
# 
# title(xlab = "Intrastate Commutes Proportion", 
#       line = 1.5)
# 
# title(ylab = "Epidemic Intensity", 
#       line = 2.5)
# axis(1, 
#      at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7), 
#      labels = F)
# axis(1, 
#      at = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),  
#      lwd = 0, 
#      line = -0.5, 
#      cex.axis =0.7)
# axis(2, 
#      at = seq(0,1,by=0.2),
#      labels = F)
# axis(2, 
#      at = seq(0,1,by=0.2), 
#      las = 2, 
#      lwd = 0, 
#      line = -0.25, 
#      cex.axis =0.7)
# 
# loess.fit <- loess(ei~proportion.total.workers_Intrastate, data = ei.df)
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")
# box()
# text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
#      par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
#      labels = "D", adj = c(0,1), xpd = T, cex = 1, font = 2)















# ei.df <- ei.df %>% 
#   mutate(total.workers = `Workers in Commuting Flow_International`+`Workers in Commuting Flow_Interstate`+`Workers in Commuting Flow_Intracounty`+`Workers in Commuting Flow_Intrastate`)


# plot(ei~population, data = ei.df, pch = 16, col = rgb(0,0,0,0.25))
# loess.fit <- loess(ei~population, data = ei.df)
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")

# plot(ei~total.workers, data = ei.df, pch = 16, col = rgb(0,0,0,0.25))
# loess.fit <- loess(ei~total.workers, data = ei.df)
# 
# lines(loess.fit$fitted[order(loess.fit$x)]~loess.fit$x[order(loess.fit$x)], col = "red")

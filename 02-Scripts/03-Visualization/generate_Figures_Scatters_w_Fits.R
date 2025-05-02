# script to make residual plots with fitted curves

## load data
load("./01-Data/02-Analytic-Data/regressions.rdata")


## packages
library(dplyr)
library(lme4)

## helper functions



## set up


ei <- ei.df %>% 
  group_by(season) %>% 
  mutate(seasmeanei=mean(ei, na.rm=T), 
         seassdei = sd(ei,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(eiseasnorm=(ei-seasmeanei)/seassdei) %>% 
  mutate(eisnr=(eiseasnorm-min(eiseasnorm, na.rm=T))/(max(eiseasnorm, na.rm=T)-min(eiseasnorm, na.rm=T)))# %>% 
  # group_by(region) %>% 
  # summarise(ei=mean(ei, na.rm=T),
  #           eimed=median(ei, na.rm=T), 
  #           ein=mean(eiseasnorm, na.rm=T),
  #           einmed=median(eiseasnorm, na.rm=T), 
  #           einr = mean(eisnr, na.rm=T), 
  #           einrmed = median(eisnr, na.rm=T)) %>% 
  # ungroup()


popscaled <- (ei.df$population-min(ei.df$population))%>%`/`(.,max(.))


ps5col <- colorRamp(c("green", "orange"))(popscaled/0.5) %>% 
  apply(., MARGIN = 1, (\(x){if(any(is.na(x))){NA}else{adjustcolor(do.call("rgb", as.list(x/255)), alpha.f = 0.5)}}))
ps10col <- colorRamp(c("orange", "purple"))((popscaled-0.5)/0.5) %>% 
  apply(., MARGIN = 1, (\(x){if(any(is.na(x))){NA}else{adjustcolor(do.call("rgb", as.list(x/255)), alpha.f = 0.5)}}))


pscol <- ifelse(is.na(ps5col), ps10col,ps5col)


popscaled <- ((ei.df$population-min(ei.df$population))+2)%>%`/`(.,max(.))





# ei.df <- ei.df %>%
#   filter(complete.cases(.))
# 
# ei.df$fitme <- predict(fitme)
# ei.df$fitme_resid <- ei.df$ei - ei.df$fitme
# 
# for(i in 1:length(myvars)){
#   ei.df[,paste0("fit",i)] <- predict(get(paste0("fit",i)))
# }
# 
# 
# ei.df <- ei.df %>%
#   mutate(across(matches("^fit[0-9]{1,2}$"), ~ei-.x, .names = "{.col}_resid"))


















png(filename = "./03-Output/02-Figures/modelcurve_byseason.png", width = 9, height = 5, pointsize = 10, units = "in", res = 300)

i=23
par(mfrow=c(3,3))
for(j in 1:length(unique(ei$season))){
  # plot(unlist(ei.df[which(ei.df$season%in%c(unique(ei.df$season)[j])),myvars[i]]), ei.df$ei[which(ei.df$season%in%c(unique(ei.df$season)[j]))],
  #      main = paste0("EI v ", myvars[i], ", ", unique(ei.df$season)[j], " Season"),
  #      pch = 16,
  #      col = pscol,
  #      cex = 3*popscaled,
  #      xlab = myvars[i],
  #      ylab = "Epidemic Intensity")
  plot(unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),myvars[i]])
       +ranef(get(paste0("fit",i)))$region[match(rownames(ranef(get(paste0("fit",i)))$region), unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"region"])),1]
       +coef(summary(get(paste0("fit", i))))[2,1]*unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"population"]), 
       ei$ei[which(ei$season%in%c(unique(ei$season)[j]))],
       main = paste0("EI v ", myvars[i], ", ", unique(ei$season)[j], " Season"),
       pch = 16,
       col = pscol,
       cex = 3*popscaled,
       xlab = myvars[i],
       ylab = "Epidemic Intensity", 
       ylim = c(0,1))
  
  intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))
  
  ranefint <- ranef(get(paste0("fit", i)))$season[j,1]
  coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]
  
  curve(intercept+ranefint+
          coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
        from = par('usr')[1],
        to = par('usr')[2],
        lty = 5,
        lwd = 3,
        col = "blue",
        add = TRUE)
}


dev.off()





## plot

if(!dir.exists("./03-Output/02-Figures/modelcurves")){dir.create("./03-Output/02-Figures/modelcurves")}
png(filename = "./03-Output/02-Figures/modelcurves/scatter_modelcurve%03d.png", width = 5, height = 5, pointsize = 10, units = "in", res = 300)

# par(mfrow = c(2,2))





for(i in 1:length(myvars)){
  
  
  intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))
  
  
  # plot(unlist(ei.df[,myvars[i]]), ei.df$ei,
  #      main = paste0("EI v ", myvars[i]), pch = 16, col = rgb(0,0,0,0.25),
  #      xlab = myvars[i],
  #      ylab = "Epidemic Intensity")
  plot(unlist(ei.df[,myvars[i]]), ei.df$ei,
       main = paste0("EI v ", myvars[i]), 
       pch = 16, 
       col = pscol, 
       cex = 3*popscaled,
       xlab = myvars[i],
       ylab = "Epidemic Intensity")
  # plot(unlist(ei.df[,myvars[i]]), ei.df$fitme_resid,#+intercept,#ei.df$ei,
  #      main = paste0("EI Residuals v ", myvars[i]), pch = 16, col = rgb(0,0,0,0.25),
  #      xlab = myvars[i],
  #      ylab = "Epidemic Intensity")
  
  legend(x = par('usr')[2], 
         y = par('usr')[4],
         xjust = 1,
         yjust = 0,
         legend = c("Small", "Medium", "Large"), 
         pch = 16, 
         pt.cex = c(1,2,3), 
         col = c("green", "orange", "purple"), 
         cex = 0.7, 
         title = "Population Size", 
         title.cex = 0.7, 
         bty = "n", 
         xpd = T, 
         y.intersp = c(1,1,1.75))
  
  # coefs <- coef(summary(get(paste0("fit", i))))[3:4,1]
  # 
  # curve(intercept+coefs[1]*x+coefs[2]*x^2, 
  #       from = par('usr')[1], 
  #       to = par('usr')[2], 
  #       lty = 5, 
  #       lwd = 3,
  #       col = "orchid", 
  #       add = TRUE)
  
  coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]
  
  curve(intercept+
          coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
        from = par('usr')[1],
        to = par('usr')[2],
        lty = 5,
        lwd = 3,
        col = "blue",
        add = TRUE)
  
  
}

dev.off()















# png(filename = "./03-Output/02-Figures/main_modelcurve_select.png", width = 6, height = 7, pointsize = 10, units = "in", res = 300)
svg(filename = "./03-Output/02-Figures/main_modelcurve_select.svg", width = 6, height = 7, pointsize = 10)


layout(matrix(c(1,1,2,1,1,3,1,1,4,6,5,5,7,5,5,8,5,5), byrow = TRUE, ncol=3), heights = c(1), widths = c(1))
layout.show(n=8)

i=23#25

plot(unlist(ei[,myvars[i]])
     +ranef(get(paste0("fit",i)))$region[match(rownames(ranef(get(paste0("fit",i)))$region), unlist(ei[,"region"])),1]
     +coef(summary(get(paste0("fit", i))))[2,1]*unlist(ei[,"population"]), 
     ei$ei,
     main = paste0("EI v ", "Internal Commutes Proportion"),
     pch = 16,
     col = pscol,
     cex = 3*popscaled,
     xlab = "Internal Commutes Proportion",
     # xlab = "Short-distance Commutes Proportion",
     ylab = "Epidemic Intensity", 
     ylim = c(0,1), 
     xlim = range(ei[,myvars[i]])+0.1*range(ei[,myvars[i]]))

intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))

ranefint <- ranef(get(paste0("fit", i)))$season[j,1]
coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]

curve(intercept+ranefint+
        coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
      from = par('usr')[1],
      to = par('usr')[2],
      lty = 5,
      lwd = 3,
      col = "blue",
      add = TRUE)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
box('fig')
legend(x = par('usr')[2], 
       y = par('usr')[4],
       xjust = 0.8,
       yjust = 0,
       legend = c("Small", "Medium", "Large"), 
       pch = 16, 
       pt.cex = c(1,2,3), 
       col = c("green", "orange", "purple"), 
       cex = 0.7, 
       title = "Population Size", 
       title.cex = 0.7, 
       bty = "n", 
       xpd = T, 
       y.intersp = c(1,1,1.75))

for(j in c(3,5,8)){
  # plot(unlist(ei.df[which(ei.df$season%in%c(unique(ei.df$season)[j])),myvars[i]]), ei.df$ei[which(ei.df$season%in%c(unique(ei.df$season)[j]))],
  #      main = paste0("EI v ", myvars[i], ", ", unique(ei.df$season)[j], " Season"),
  #      pch = 16,
  #      col = pscol,
  #      cex = 3*popscaled,
  #      xlab = myvars[i],
  #      ylab = "Epidemic Intensity")
  par(mar=c(1,1,2,1))
  plot(unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),myvars[i]])
       +ranef(get(paste0("fit",i)))$region[match(rownames(ranef(get(paste0("fit",i)))$region), unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"region"])),1]
       +coef(summary(get(paste0("fit", i))))[2,1]*unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"population"]), 
       ei$ei[which(ei$season%in%c(unique(ei$season)[j]))],
       main = paste0(unique(ei$season)[j], " Season"),
       pch = 16,
       col = pscol,
       cex = 3*popscaled,
       xlab = "",
       ylab = "", 
       axes = F,
       ylim = c(0,1), 
       xlim = range(ei[,myvars[i]])+0.1*range(ei[,myvars[i]]))
  
  intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))
  
  ranefint <- ranef(get(paste0("fit", i)))$season[j,1]
  coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]
  
  curve(intercept+ranefint+
          coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
        from = par('usr')[1],
        to = par('usr')[2],
        lty = 5,
        lwd = 3,
        col = "blue",
        add = TRUE)
  box('fig')
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[2:4][case_when(j==3~1,j==5~2,j==8~3)], adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}





i=30
par(mar=c(5.1,4.1,4.1,2.1))
plot(unlist(ei[,myvars[i]])
     +ranef(get(paste0("fit",i)))$region[match(rownames(ranef(get(paste0("fit",i)))$region), unlist(ei[,"region"])),1]
     +coef(summary(get(paste0("fit", i))))[2,1]*unlist(ei[,"population"]), 
     ei$ei,
     main = paste0("EI v ", "Ratio of Long-distance to Internal Commutes"),
     pch = 16,
     col = pscol,
     cex = 3*popscaled,
     xlab = "Ratio of Long-distance to Internal Commutes",
     ylab = "Epidemic Intensity", 
     ylim = c(0,1), 
     xlim = range(ei[,myvars[i]])+0.1*range(ei[,myvars[i]]))

intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))

ranefint <- ranef(get(paste0("fit", i)))$season[j,1]
coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]

curve(intercept+ranefint+
        coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
      from = par('usr')[1],
      to = par('usr')[2],
      lty = 5,
      lwd = 3,
      col = "blue",
      add = TRUE)
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "E", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
box('fig')
legend(x = par('usr')[2], 
       y = par('usr')[4],
       xjust = 0.8,
       yjust = 0,
       legend = c("Small", "Medium", "Large"), 
       pch = 16, 
       pt.cex = c(1,2,3), 
       col = c("green", "orange", "purple"), 
       cex = 0.7, 
       title = "Population Size", 
       title.cex = 0.7, 
       bty = "n", 
       xpd = T, 
       y.intersp = c(1,1,1.75))

for(j in c(3,5,8)){
  # plot(unlist(ei.df[which(ei.df$season%in%c(unique(ei.df$season)[j])),myvars[i]]), ei.df$ei[which(ei.df$season%in%c(unique(ei.df$season)[j]))],
  #      main = paste0("EI v ", myvars[i], ", ", unique(ei.df$season)[j], " Season"),
  #      pch = 16,
  #      col = pscol,
  #      cex = 3*popscaled,
  #      xlab = myvars[i],
  #      ylab = "Epidemic Intensity")
  par(mar=c(1,1,2,1))
  plot(unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),myvars[i]])
       +ranef(get(paste0("fit",i)))$region[match(rownames(ranef(get(paste0("fit",i)))$region), unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"region"])),1]
       +coef(summary(get(paste0("fit", i))))[2,1]*unlist(ei[which(ei$season%in%c(unique(ei$season)[j])),"population"]), 
       ei$ei[which(ei$season%in%c(unique(ei$season)[j]))],
       main = paste0(unique(ei$season)[j], " Season"),
       pch = 16,
       col = pscol,
       cex = 3*popscaled,
       xlab = "",
       ylab = "", 
       axes = F, 
       ylim = c(0,1), 
       xlim = range(ei[,myvars[i]])+0.1*range(ei[,myvars[i]]))
  
  intercept <- c(coef(summary(get(paste0("fit",i))))[1:2,1]%*%c(1,median(ei.df$population)))
  
  ranefint <- ranef(get(paste0("fit", i)))$season[j,1]
  coefs <- coef(summary(get(paste0("fit", i))))[3:5,1]
  
  curve(intercept+ranefint+
          coefs[1]*x+coefs[2]*x^2+coefs[3]*x^3,
        from = par('usr')[1],
        to = par('usr')[2],
        lty = 5,
        lwd = 3,
        col = "blue",
        add = TRUE)
  box('fig')
  text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
       par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
       labels = LETTERS[6:8][case_when(j==3~1,j==5~2,j==8~3)], adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}


dev.off()






























## save


## clean environment
rm(list=ls())
gc()










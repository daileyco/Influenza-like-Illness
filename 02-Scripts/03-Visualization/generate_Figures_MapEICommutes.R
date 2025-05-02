# Script to create figure showing total commuters vs population at origin

## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")
load("./01-Data/02-Analytic-Data/countycommutescat.rds")


# load("./01-Data/02-Analytic-Data/od.rds")
# load("./01-Data/02-Analytic-Data/parameters_gravity_distance_thresholds.rds")
# 
# load("./01-Data/01-Processed-Data/county_centers.rds")


## packages
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(sf)






odc <- od %>% 
  group_by(`State FIPS Residence`, `State Residence`, `County FIPS Residence`, `County Residence`, period, flow_type) %>%
  summarise(`Workers in Commuting Flow`=sum(`Workers in Commuting Flow`))%>%
  ungroup() %>%
  filter(period%in%c("2011-2015")) %>% 
  pivot_wider(names_from = flow_type, values_from = `Workers in Commuting Flow`) %>%
  mutate(across(6:8, ~ifelse(is.na(.x), 0, .x)))


ei <- ei.df %>% 
  group_by(season) %>% 
  mutate(seasmeanei=mean(ei, na.rm=T), 
         seassdei = sd(ei,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(eiseasnorm=(ei-seasmeanei)/seassdei) %>% 
  mutate(eisnr=(eiseasnorm-min(eiseasnorm, na.rm=T))/(max(eiseasnorm, na.rm=T)-min(eiseasnorm, na.rm=T))) %>% 
  group_by(region) %>% 
  summarise(ei=mean(ei, na.rm=T),
            eimed=median(ei, na.rm=T), 
            ein=mean(eiseasnorm, na.rm=T),
            einmed=median(eiseasnorm, na.rm=T), 
            einr = mean(eisnr, na.rm=T), 
            einrmed = median(eisnr, na.rm=T)) %>% 
  ungroup()




## load shapefile
county <- st_read("../Mobility-Models/01-Data/00-Raw-Data/Spatial/cb_2018_us_county_5m.shp")


# ak hi pr vi 
ak <- "02"
hi <- "15"
pr <- "72"
others <- c("78", "60", "66" , "69")

notmain <- c(ak, hi, pr, others)


mainland <- county %>% 
  filter(!STATEFP%in%notmain) %>%
  arrange(STATEFP, COUNTYFP)



mainlandstates <- mainland %>% 
  group_by(STATEFP) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  left_join(., 
            odc %>% 
              select(STATEFP=`State FIPS Residence`, 
                     region=`State Residence`) %>%
              filter(!duplicated(.)), 
            by = c("STATEFP")) %>%
  left_join(., 
            ei, 
            by = c("region"))




mainland <- mainland %>%
  left_join(., 
            odc, 
            by = c("STATEFP"="State FIPS Residence", "COUNTYFP"="County FIPS Residence"))




ei.breaks <- quantile(ei$einr, probs = 0:40/40)
# ei.breaks <- 0:40/40
ei.col <- viridis::viridis(length(ei.breaks)-1)


int.breaks <- quantile(odc$Internal, probs = 0:40/40, na.rm = T)
short.breaks <- quantile(odc$`Short Distance`, probs = 0:40/40, na.rm = T)
long.breaks <- quantile(odc$`Long Distance`, probs = 0:40/40, na.rm = T)

com.col <- gray.colors(length(int.breaks)-1)
com.col <- com.col[length(com.col):1]


# [1] "#E6E6E6" "#E3E3E3" "#E1E1E1" "#DEDEDE" "#DBDBDB" "#D9D9D9" "#D6D6D6"
# [8] "#D4D4D4" "#D1D1D1" "#CECECE" "#CBCBCB" "#C9C9C9" "#C6C6C6" "#C3C3C3"
# [15] "#C0C0C0" "#BDBDBD" "#BABABA" "#B6B6B6" "#B3B3B3" "#B0B0B0" "#ACACAC"
# [22] "#A9A9A9" "#A5A5A5" "#A2A2A2" "#9E9E9E" "#9A9A9A" "#969696" "#929292"
# [29] "#8E8E8E" "#898989" "#858585" "#808080" "#7B7B7B" "#767676" "#707070"
# [36] "#6A6A6A" "#646464" "#5D5D5D" "#555555" "#4D4D4D"







# png(filename = "./03-Output/02-Figures/main_figure_eicommaps.png", units = "in", res=300, width = 9, height = 5, pointsize=10)
svg(filename = "./03-Output/02-Figures/main_figure_eicommaps.svg", width = 9, height = 4.6, pointsize=10)


layout(matrix(c(1,1,1,1,2,3,4,5,6,6),byrow=TRUE,ncol=5),heights = c(4,2), widths = c(2.375,2.375,2.375,1,0.875))
# layout.show(n=6)

{
par(mar=c(0,0,0,0))
plot(mainlandstates$geometry, border = "gainsboro")
plot(mainlandstates["einr"], breaks = ei.breaks, pal = ei.col, add = TRUE)
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "A", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}
{
par(mar = c(3,5,3,1))
image(x=1, 
      y=as.numeric(factor(ei.breaks[-1])), 
      z=as.matrix(t((ei.breaks[-1] - diff(ei.breaks)/2))), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = ei.col, 
      breaks = ei.breaks)
box()
box('fig')
axis(side = 2, 
     at = c(1:length(ei.breaks))[seq(1,length(ei.breaks),by=4)], 
     labels = format(ei.breaks[seq(1,length(ei.breaks),by=4)], digits = 2), 
     tick = F,
     las = 1, 
     cex.axis = 1, 
     xpd = TRUE)
axis(side = 3, 
     at = 0.3, 
     xpd = TRUE,
     labels = "Average\nEpidemic Intensity", 
     tick = F)
}




{
par(mar=c(0,0,0,0))
plot(mainland$geometry, border = NA)
plot(mainland["Long Distance"], breaks = long.breaks, pal = com.col, add = TRUE, border = NA)
plot(mainlandstates$geometry, border = "black", lwd = 1, add = TRUE)
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "B", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}

{
par(mar=c(0,0,0,0))
plot(mainland$geometry, border = NA)
plot(mainland["Short Distance"], breaks = short.breaks, pal = com.col, add = TRUE, border = NA)
plot(mainlandstates$geometry, border = "black", lwd = 1, add = TRUE)
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "C", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}

{
par(mar=c(0,0,0,0))
plot(mainland$geometry, border = NA)
plot(mainland["Internal"], breaks = int.breaks, pal = com.col, add = TRUE, border = NA)
plot(mainlandstates$geometry, border = "black", lwd = 1, add = TRUE)
box()
text(par('usr')[1]-par('plt')[1]*diff(par('usr')[1:2])/diff(par('plt')[1:2]), 
     par('usr')[4]+(1-par('plt')[4])*diff(par('usr')[3:4])/diff(par('plt')[3:4]), 
     labels = "D", adj = c(-0.2,1.2), xpd = T, cex = 1, font = 2)
}



{
  par(mar = c(3,12,3,1))
  image(x=1, 
        y=as.numeric(factor(int.breaks[-1])), 
        z=as.matrix(t((int.breaks[-1] - diff(int.breaks)/2))), 
        xlab = "", 
        ylab = "",
        axes = F, 
        col = com.col, 
        breaks = int.breaks)
  
  axis(side = 2, 
       at = c(1:length(int.breaks))[seq(1,length(int.breaks),by=8)]-(4/3), 
       labels = FALSE, 
       tick = T,
       tcl = -12,
       lty = 3,
       col = "grey70",
       las = 1, 
       cex.axis = 1, 
       xpd = TRUE)
  
  box()
  box('fig')
  axis(side = 2, 
       at = c(1:length(int.breaks))[seq(1,length(int.breaks),by=8)], 
       labels = format(round(int.breaks[seq(1,length(int.breaks),by=8)])), 
       tick = F,
       las = 1, 
       cex.axis = 1, 
       xpd = TRUE)
  
  axis(side = 2, 
       at = c(1:length(short.breaks))[seq(1,length(short.breaks),by=8)], 
       labels = format(round(short.breaks[seq(1,length(short.breaks),by=8)])), 
       tick = F,
       las = 1, 
       cex.axis = 1, 
       line = 4, 
       xpd = TRUE)
  
  axis(side = 2, 
       at = c(1:length(short.breaks))[seq(1,length(short.breaks),by=8)], 
       labels = format(round(long.breaks[seq(1,length(long.breaks),by=8)])), 
       tick = F,
       las = 1, 
       cex.axis = 1, 
       line = 8, 
       xpd = TRUE)
  
  axis(side=2, 
       # at = length(short.breaks)+1.5, 
       at = -3,
       labels = c("Internal", "Short-distance", "Long-distance")[1], 
       tick = F, 
       line = c(0), 
       las = 1,
       xpd = TRUE, 
       font = 4)
  axis(side=2, 
       # at = length(short.breaks)+1.5,
       at = -3,
       labels = c("Internal", "Local", "Long-distance")[2], 
       tick = F, 
       line = c(4), 
       las = 1,
       xpd = TRUE, 
       font = 4)
  axis(side=2, 
       # at = length(short.breaks)+1.5, 
       at = -3,
       labels = c("Internal", "Short-distance", "Long")[3], 
       tick = F, 
       line = c(8), 
       las = 1,
       xpd = TRUE, 
       font = 4)
  
  # axis(side = 3, 
  #      at = 0.075, 
  #      xpd = TRUE,
  #      # hadj = 1,
  #      line = 0.5,
  #      labels = "Commuting Extent Frequencies", 
  #      tick = F)
  
  axis(side = 3, 
       at = par('usr')[1],
       hadj = 0.8,
       xpd = TRUE,
       # hadj = 1,
       line = 0.5,
       labels = "Commuting Extent Frequencies", 
       tick = F)
}


dev.off()





## clean environment
rm(list = ls())
gc()


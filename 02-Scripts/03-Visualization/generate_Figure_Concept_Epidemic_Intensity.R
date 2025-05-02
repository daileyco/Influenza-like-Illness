# script to make conceptual figure for epidemic intensity



## load data


## packages
library(dplyr)
library(RColorBrewer)


## helper functions

drawCircle <- function(cx,cy,r,ccol="black",nv=100,clwd=1){
  # cx <- 2
  # cy <- 2
  # 
  # r <- 1
  # 
  x <- seq(cx-r, cx+r, length.out = 50)
  
  y <- (sqrt((r^2)-(x-cx)^2)+cy)
  y <- c(y, cy+(cy-y))
  
  x <- c(x, x[length(x):1])
  
  polygon(x,y,border="black",col=ccol,xpd=T,lwd=clwd)
}









## generate two epidemic curves

x1 <- seq(0,pi,length.out = 100)
y1 <- (sin(x1)+0.1)^5
y2 <- (sin(x1)+0.1)^100


y1 <- y1 / sum(y1)
y2 <- y2 / sum(y2)


# plot(y2~x1)
# lines(x=x1, y=y1)



## plot




png(filename = "./03-Output/02-Figures/concept_epidemicintensity.png", width = 8, height = 4.5, units = "in", res = 300, pointsize = 10)
par(mar = c(3,3,2,2), mfrow = c(2,1))



plot.new()
plot.window(xlim = c(0,50), ylim = c(0,max(c(y1,y2))*1.1))
abline(h=0:100/100, lty = 3, col = "gainsboro")
barplot(height = y1, width = 0.5, space = 0, col = "lightblue", ylim = c(0,max(c(y1,y2))*1.1), 
        axes = F, add = T)
axis(1, at = c(0.75,49.25), labels = F, tcl = 0.2, pos = -0.01, xpd = T)
axis(1, at = mean(par('usr')[1:2]), labels = c("Flu Season"), pos = -0.01, tick = F, padj = -0.5)
axis(2, at = c(0, par('usr')[4]), labels = F, tcl = 0.2)
axis(2, at = mean(par('usr')[3:4]), labels = c("Proportion\nof Cumulative Incidence"), tick = F)

ent1 <- sum(y1*log(y1))*-1

text(par('usr')[2], y = par('usr')[4], 
     labels = paste0("Entropy = ", round(ent1,3)), 
     adj = c(1,1))

# text(par('usr')[2], y = par('usr')[4], 
#      labels = paste0("\nLess intense\nMore diffuse"), 
#      font = 3, 
#      adj = c(1,1))
text(par('usr')[1], y = par('usr')[4], 
     labels = paste0("Less intense\nMore diffuse"), 
     font = 3, 
     adj = c(-0.1,1))



plot.new()
plot.window(xlim = c(0,50), ylim = c(0,max(c(y1,y2))*1.1))
abline(h=0:100/100, lty = 3, col = "gainsboro")
barplot(height = y2, width = 0.5, space = 0, col = "darkred", ylim = c(0,max(c(y1,y2))*1.1), 
        axes = F, add = T)
axis(1, at = c(0.75,49.25), labels = F, tcl = 0.2, pos = -0.01, xpd = T)
axis(1, at = mean(par('usr')[1:2]), labels = c("Flu Season"), pos = -0.01, tick = F, padj = -0.5)
axis(2, at = c(0, par('usr')[4]), labels = F, tcl = 0.2)
axis(2, at = mean(par('usr')[3:4]), labels = c("Proportion\nof Cumulative Incidence"), tick = F)


ent2 <- sum(y2*log(y2))*-1

text(par('usr')[2], y = par('usr')[4], 
     labels = paste0("Entropy = ", round(ent2,3)), 
     adj = c(1,1))

# text(par('usr')[2], y = par('usr')[4], 
#      labels = paste0("\nMore intense\nLess diffuse"), 
#      font = 3, 
#      adj = c(1,1))
text(par('usr')[1], y = par('usr')[4], 
     labels = paste0("More intense\nLess diffuse"), 
     font = 3, 
     adj = c(-0.1,1))


dev.off()










## metapop configs


plot.new()
par(mar = c(0,0,0,0))
plot.window(xlim = c(0,10), ylim = c(0,10))


cx <- c(5,2,5,8,5)
cy <- c(5,5,8,5,2)
r <- c(1,0.5,0.5,0.5,0.5)

for(i in 1:length(cx)){
  
  drawCircle(cx[i],cy[i],r[i], ccol=NA, clwd=3)
  
}

arrows(x0=c(4,5,6,5),y0=c(5,6,5,4),x1=c(2.5,5,7.5,5),y1=c(5,7.5,5,2.5),
       length=0.1,angle=45,
       code=3, 
       lwd=3)

text(x=cx, y=cy,
     labels = LETTERS[1:length(cx)], 
     cex = 2, 
     font = 2)








plot.new()
par(mar = c(0,0,0,0))
plot.window(xlim = c(0,10), ylim = c(0,10))


cx <- c(1,4,7,9,7)
cy <- c(1,4,4,8,6+sqrt(2)/2)
r <- c(1,1,1,1,0.5)



# arrows(x0=c(2+sqrt(2)/2,5,7,7+sqrt(2)/2,7+sqrt(2)/2),x1=c(3-sqrt(2)/2,6,7,9-sqrt(2)/2),
#        y0=c(1+sqrt(2)/2,4,5,6+sqrt(2)),
#        y1=c(4-sqrt(2)/2,4,6+sqrt(2)/2-0.5,8-sqrt(2)/2),
#        length=0.1,angle=45,
#        code=3, 
#        lwd=3)


arrows(x0=cx[-length(cx)],
       x1=cx[-1],
       y0=cy[-length(cy)],
       y1=cy[-1],
       length=0.1,angle=45,
       code=3, 
       lwd=3)


for(i in 1:length(cx)){
  
  drawCircle(cx[i],cy[i],r[i], ccol="white", clwd=3)
  
}



text(x=cx, y=cy,
     labels = LETTERS[1:length(cx)], 
     cex = 2, 
     font = 2)








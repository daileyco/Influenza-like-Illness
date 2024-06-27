# Figure heatmap showing proportion of cumulative seasonal ILI by week and state


## load data
load("./01-Data/02-Analytic-Data/ili.rds")

## packages
library(dplyr)
library(tidyr)

## figure set up
x <- data.frame(date = unique(ili$week_start)) %>%
  arrange(date) %>%
  mutate(number = as.numeric(date))


y <- data.frame(region = unique(ili$region)) %>%
  arrange(desc(region)) %>%
  mutate(number = row_number())


z <- ili %>%
  select(region, week_start, p.cili) %>%
  arrange(desc(region)) %>%
  pivot_wider(names_from = region, values_from = p.cili) %>%
  arrange(week_start) %>%
  as.data.frame()

rownames(z) <- z$week_start

z <- z %>%
  select(-week_start) %>%
  as.matrix()


mybreaks <- c(seq(0,max(ili$p.cili, na.rm = T), by = 0.01), max(ili$p.cili, na.rm = T))







## two letter codes

codes <- data.frame(state = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                              "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                              "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                              "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                              "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
                              "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", 
                              "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                              "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", 
                              "Wyoming"), 
                    code = c("AL", "AK", "AZ", "AR", 
                             "CA", "CO", "CT", "DE", "DC", 
                             "FL", "GA", "HI", "ID", "IL", "IN",
                             "IA", "KS", "KY", "LA", "ME", "MD", 
                             "MA", "MI", "MN", "MS", "MO", 
                             "MT", "NE", "NV", "NH", "NJ", 
                             "NM", "NY", "NC", "ND", "OH", 
                             "OK", "OR", "PA", "PR", "RI", 
                             "SC", "SD", "TN", "TX", "UT", 
                             "VT", "VA", "WA", "WV", "WI", 
                             "WY"))












## figure
svg(filename = "./03-Output/02-Figures/epidemic_curve_heatmap.svg", width = 16, height = 9, pointsize = 10)

layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), widths = c(7,1), heights = 1)
# layout.show(n=2)
par(mar = c(3.1, 8.1, 1, 1))

### heatmap
image(x=x$date, 
      y=y$number, 
      z=z, 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(length(mybreaks)-1), 
      breaks = mybreaks)

axis(1, 
     at = seq(as.Date("2012/02/01", format = "%Y/%m/%d"), as.Date("2020/02/01", format = "%Y/%m/%d"), by = "year"), 
     labels = FALSE,
     hadj = 0.5, 
     lwd = 0,
     lwd.ticks = 0.5,
     line = 0)


axis(1, 
     at = seq(as.Date("2012/02/01", format = "%Y/%m/%d"), as.Date("2020/02/01", format = "%Y/%m/%d"), by = "year"), 
     labels = format(seq(as.Date("2012/02/01", format = "%Y/%m/%d"), as.Date("2020/02/01", format = "%Y/%m/%d"), by = "year"), "%b"), 
     hadj = 0.5, 
     lwd = 0,
     lwd.ticks = 0,
     line = -0.5)

axis(1, 
     at = seq(as.Date("2012/02/01", format = "%Y/%m/%d"), as.Date("2020/02/01", format = "%Y/%m/%d"), by = "year"), 
     labels = format(seq(as.Date("2012/02/01", format = "%Y/%m/%d"), as.Date("2020/02/01", format = "%Y/%m/%d"), by = "year"), "%Y"), 
     hadj = 0.5, 
     lwd = 0,
     lwd.ticks = 0,
     line = 0.5)

abline(v = unique(ili$week_start[which(ili$week==40)])[-1], 
       col = rgb(1,1,1,0.5), 
       lty = 3)



axis(2, 
     at = y$number, 
     labels = FALSE, 
     las = 2, 
     lwd = 0, 
     lwd.ticks = 0.5,
     padj = 0.5, 
     hadj = 1)

axis(2, 
     at = y$number, 
     labels = codes$code[match(codes$state, y$region)], 
     las = 2, 
     lwd = 0, 
     lwd.ticks = 0,
     padj = 0.5, 
     hadj = 1, 
     line = -0.5)




par(mar = c(10.2,6,5,2))
image(x=1, 
      y=as.numeric(factor(mybreaks)), 
      z=as.matrix(t(mybreaks)), 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(length(mybreaks)-1), 
      breaks = mybreaks)
box()
axis(side = 2, 
     at = c(1,24,47), 
     labels = round(mybreaks[c(1,24,47)],2), 
     tick = F,
     las = 1)
axis(side = 3, 
     at = 1, 
     labels = "Proportion of\nSeason Cumulative\nILI Cases", 
     tick = F)






dev.off()



## clean environment
rm(list = ls())
gc()





## old
# #least intense
# plot(ili$week_start[which(ili$year%in%c(2011, 2012) & ili$region%in%c("Louisiana"))], ili$ilitotal[which(ili$year%in%c(2011, 2012) & ili$region%in%c("Louisiana"))])
# #most intense
# plot(ili$week_start[which(ili$year%in%c(2014, 2015) & ili$region%in%c("Delaware"))], ili$ilitotal[which(ili$year%in%c(2014, 2015) & ili$region%in%c("Delaware"))])


# 
# 
# 
# 
# plot(unique(ili2$week_start), tapply(ili2$ilitotal, ili2$week_start, sum, na.rm = T), type = "l")
# plot(unique(ili2$week_start), tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T), type = "l")
# 
# plot(unique(ili2$week_start), tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T)/10, type = "l", ylim = c(0, max(tapply(ili2$total_patients, ili2$week_start, sum, na.rm = T))/10), xlab = "Week", ylab = "Count")
# lines(unique(ili2$week_start), tapply(ili2$ilitotal, ili2$week_start, sum, na.rm = T), lty = 2, col = "red")
# legend("topleft", lty = c(1,2), col = c("black", "red"), legend = c("Total Patients (unit=10s)", "Influenza-like Illness"))
# 
# 
# 















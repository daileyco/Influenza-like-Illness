









## load data
load("./01-Data/02-Analytic-Data/ili.rds")
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)
library(tidyr)

## figure set up
x <- data.frame(season = unique(ei.df$season)) %>%
  arrange(season) %>%
  mutate(number = as.numeric(factor(season)))


y <- data.frame(region = unique(ili$region)) %>%
  arrange(desc(region)) %>%
  mutate(number = row_number())


z <- ei.df %>%
  select(region, season, ei) %>%
  arrange(desc(region)) %>%
  pivot_wider(names_from = region, values_from = ei) %>%
  arrange(season) %>%
  as.data.frame()



rownames(z) <- z$season

z <- z %>%
  select(-season) %>%
  as.matrix()


mybreaks <- seq(0,1, by = 0.005)





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
svg(filename = "./03-Output/02-Figures/epidemic_intensity_heatmap.svg", width = 16, height = 9, pointsize = 10)

layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), widths = c(7,1), heights = 1)
# layout.show(n=2)
par(mar = c(3.1, 8.1, 1, 1))

### heatmap
image(x=x$number, 
      y=y$number, 
      z=z, 
      xlab = "", 
      ylab = "",
      axes = F, 
      col = viridis::viridis(length(mybreaks)-1), 
      breaks = mybreaks)

axis(1, 
     at = x$number, 
     labels = FALSE,
     hadj = 0.5, 
     lwd = 0,
     lwd.ticks = 0.5,
     line = 0)


axis(1, 
     at = x$number, 
     labels = sub("-[0-9]{2}", "-", x$season), 
     hadj = 0.5, 
     lwd = 0,
     lwd.ticks = 0,
     line = 0)



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
     at = as.numeric(factor(mybreaks))[which(mybreaks%in%(0:5/5))], 
     labels = mybreaks[which(mybreaks%in%(0:5/5))], 
     tick = F,
     las = 1)
axis(side = 3, 
     at = 1, 
     labels = "Epidemic\nIntensity", 
     tick = F)






dev.off()



## clean environment
rm(list = ls())
gc()

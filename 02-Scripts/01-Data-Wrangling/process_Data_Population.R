# Script to process population data from the census

## Load Data

## Packages
library(readxl)
library(tidyr)
library(dplyr)

## Helper Functions




## Population data
#2010-2019

pop <- read_xlsx("./01-Data/00-Raw-Data/Population/nst-est2019-01.xlsx", 
                 skip = 3)


#drop aggregates, blank row, and table footer notes
#drop cols that arent the yearly estimates

pop <- pop[-c(1:5, 57,59:63),-c(2:3)]



#name first col
names(pop)[1] <- "region"

#get rid of period at beginning of names
pop$region <- sub("[.]", "", pop$region)


#wide to long df


pop <- pivot_longer(pop, 2:11, names_to = "year", values_to = "population")




#2020-22

pop20 <- read_xlsx("./01-Data/00-Raw-Data/Population/NST-EST2022-POP.xlsx",
                   skip = 3)


#drop aggregates, blank row, and table footer notes
#drop cols that arent the yearly estimates

pop20 <- pop20[-c(1:5, 57,59:63),-2]



#name first col
names(pop20)[1] <- "region"

#get rid of period at beginning of names
pop20$region <- sub("[.]", "", pop20$region)


#wide to long df

pop20 <- pivot_longer(pop20, 2:4, names_to = "year", values_to = "population")




pop <- bind_rows(pop, pop20) %>%
  arrange(region, year)






## Save
save(pop, file = "./01-Data/01-Processed-Data/pop.rds")


## Clean Environment
rm(list = ls())
gc()
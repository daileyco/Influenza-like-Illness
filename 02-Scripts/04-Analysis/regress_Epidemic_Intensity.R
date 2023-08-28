# Regress epidemic intensity against covariates

## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)
library(tibble)
library(flextable)


## center and scale proportions

ei.df <- ei.df %>%
  mutate(across(matches("^proportion"), ~scale(.x, center=T, scale=T)))



## fit linear models

fit1 <- lm(ei~log(population), data=ei.df)
# summary(fit1)
# plot(fit1)

fit2 <- lm(ei~log(population)+proportion.total.workers_International+proportion.total.workers_Interstate+proportion.total.workers_Intrastate, data=ei.df)
# summary(fit2)
# plot(fit2)

fit3 <- lm(ei~log(population)+proportion.total.workers_International+proportion.total.workers_Interstate+proportion.total.workers_Intrastate+season, data=ei.df)
# summary(fit3)
# plot(fit3)





## aggregate coefficients

ests.fit1 <- cbind(coef(fit1), confint(fit1)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate)

ests.fit2 <- cbind(coef(fit2), confint(fit2)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate)

ests.fit3 <- cbind(coef(fit3), confint(fit3)) %>% 
  as.data.frame() %>%
  setNames(., nm = c("Beta", "LL", "UL")) %>% 
  
  rownames_to_column(., var = "Parameter") %>%
  mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
  select(Parameter, Estimate)



ests.fits <- full_join(ests.fit1, ests.fit2, by = "Parameter") %>%
  full_join(., ests.fit3, by = "Parameter")
  # mutate(across(2:4, ~ifelse(substr(.x,1,1)!="-", paste0(" ", .x), .x)))


### make prettier
ests.fits$Parameter <- c("(Intercept)", "log(Population)", "Proportion International Commutes", "Proportion Interstate Commutes", "Proportion Intrastate Commutes", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")

names(ests.fits) <- c("Parameter", "Model 1", "Model 2", "Model 3")

### add row for reference season
ests.fits <- ests.fits %>% 
  add_row(.after = 5, Parameter = "2011-2012", `Model 1`=NA, `Model 2`=NA, `Model 3`="1 (ref)")


## flextable
coef.estimates <- flextable(ests.fits)  %>%
  
  font(part = "all", fontname = "Arial") %>% 
  fontsize(part = "header", size = 11) %>%
  fontsize(part = "body", size = 10) %>% 
  
  align(part = "header", align = "center") %>%
  align(part = "body", j=1, align = "left") %>%
  align(part = "body", j=2:4, align = "right") %>%
  
  bold(part = "header", j=1:4) %>%
  bold(part = "body", i=c(1,2), j=2) %>%
  bold(part = "body", i=c(1:5), j=3) %>%
  bold(part = "body", i=c(1:5,7:9,11:12,14), j=4) %>%
  
  
  autofit(part = "body") %>%
  line_spacing(part = "all", space = 0.75) %>%
  
  border_remove() %>%
  border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) 




## save table
save_as_docx(coef.estimates, path = "./03-Output/01-Tables/model_coefficient_estimates.docx")

save(coef.estimates, file = "./03-Output/01-Tables/model_coefficient_estimates.rds")


## clean environment
rm(list=ls())
gc()

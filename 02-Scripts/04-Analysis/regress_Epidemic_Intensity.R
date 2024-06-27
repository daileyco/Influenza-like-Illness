# Regress epidemic intensity against covariates

## load data
load("./01-Data/02-Analytic-Data/ei_df.rdata")

## packages
library(dplyr)
library(tidyr)
library(tibble)
library(flextable)
library(lme4)





## final cleaning


ei.df <- ei.df %>%
  rename(state_area_km2_land = state_area_km2) %>% 
  mutate(state_area_km2 = state_area_km2_land + state_area_km2_water) %>%
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
                  state_area_km2_land,
                  state_area_km2_water,
                  pop_density, 
                  pop_density2), ~log(.x))) %>%
  mutate(across(c(ratio.si, 
                  ratio.ls, 
                  population, 
                  Total.Workers, 
                  Internal, 
                  `Short Distance`, 
                  `Long Distance`, 
                  Internal_prop, 
                  `Short Distance_prop`, 
                  `Long Distance_prop`, 
                  distance_mean_km, 
                  distance_mean_km_nozeros, 
                  county_area_km2_mean, 
                  state_area_km2,  
                  state_area_km2_land,
                  state_area_km2_water, 
                  pop_density, 
                  pop_density2), ~c(scale(.x, center = TRUE, scale = TRUE)), .names = "{.col}"))





## fit linear models

fitnull <- lm(ei~1, data = ei.df)

fitre<- lmer(ei~(1|region), data = ei.df)
fitre2<- lmer(ei~(1|region)+(1|season), data = ei.df)

fitme <- lmer(ei~(1|region)+(1|season) + population, data = ei.df)


# anova(fitme,fitre,fitre2, fitnull)

png("./03-Output/02-Figures/random_effects%03d.png", width = 10, height = 10, units = "in", res = 300, pointsize = 10)
dotplot.ranef.mer(ranef(fitme))
dev.off()






myvars <- names(ei.df)[c(4,19,23:24,20,25,21:22,8,17,18,9,14,10,15,11,16,12:13)]
# dput(myvars)
# c("peak_wk", "county_count", "pop_density", "pop_density2", "county_area_km2_mean", 
#   "state_area_km2", "state_area_km2_land", "state_area_km2_water", 
#   "Total.Workers", "distance_mean_km", "distance_mean_km_nozeros", 
#   "Internal", "Internal_prop", "Short Distance", "Short Distance_prop", 
#   "Long Distance", "Long Distance_prop", "ratio.si", "ratio.ls"
# )







## simple linear
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 myvars,
#                 ", data = ei.df)\n")))


## with quadratic terms
# 
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 myvars,
#                 "+I((",
#                 myvars,
#                 ")^2), data = ei.df)\n")))
{
fit1 <- lmer(ei~(1|region)+(1|season)+population+peak_wk+I((peak_wk)^2), data = ei.df)
  
fit2 <- lmer(ei~(1|region)+(1|season)+population+county_count+I((county_count)^2), data = ei.df)

fit3 <- lmer(ei~(1|region)+(1|season)+population+pop_density+I((pop_density)^2), data = ei.df)
fit4 <- lmer(ei~(1|region)+(1|season)+population+pop_density2+I((pop_density2)^2), data = ei.df)

fit5 <- lmer(ei~(1|region)+(1|season)+population+county_area_km2_mean+I((county_area_km2_mean)^2), data = ei.df)

fit6 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2+I((state_area_km2)^2), data = ei.df)
fit7 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2_land+I((state_area_km2_land)^2), data = ei.df)
fit8 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2_water+I((state_area_km2_water)^2), data = ei.df)

fit9 <- lmer(ei~(1|region)+(1|season)+population+Total.Workers+I((Total.Workers)^2), data = ei.df)

fit10 <- lmer(ei~(1|region)+(1|season)+population+distance_mean_km+I((distance_mean_km)^2), data = ei.df)
fit11 <- lmer(ei~(1|region)+(1|season)+population+distance_mean_km_nozeros+I((distance_mean_km_nozeros)^2), data = ei.df)

fit12 <- lmer(ei~(1|region)+(1|season)+population+Internal+I((Internal)^2), data = ei.df)
fit13 <- lmer(ei~(1|region)+(1|season)+population+Internal_prop+I((Internal_prop)^2), data = ei.df)

fit14 <- lmer(ei~(1|region)+(1|season)+population+`Short Distance`+I((`Short Distance`)^2), data = ei.df)
fit15 <- lmer(ei~(1|region)+(1|season)+population+`Short Distance_prop`+I((`Short Distance_prop`)^2), data = ei.df)

fit16 <- lmer(ei~(1|region)+(1|season)+population+`Long Distance`+I((`Long Distance`)^2), data = ei.df)
fit17 <- lmer(ei~(1|region)+(1|season)+population+`Long Distance_prop`+I((`Long Distance_prop`)^2), data = ei.df)

fit18 <- lmer(ei~(1|region)+(1|season)+population+ratio.si+I((ratio.si)^2), data = ei.df)
fit19 <- lmer(ei~(1|region)+(1|season)+population+ratio.ls+I((ratio.ls)^2), data = ei.df)
}

# quartic
## seems too overfit
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 myvars,
#                 "+I((",
#                 myvars,
#                 ")^2)", 
#                 "+I((", 
#                 myvars,
#                 ")^3), data = ei.df)\n")))




fits <- mget(ls(pattern = "^fit[0-9]{1,2}$"))







coefests <- lapply(fits, 
                   (\(x){
                     # pointests <- coef(x)$region %>% 
                     #   summarise(`(Intercept)` = paste0(round(median(`(Intercept)`),3), 
                     #                                    " [", 
                     #                                    paste0(round(quantile(`(Intercept)`, 
                     #                                                          probs = c(0.25,0.75)),3), 
                     #                                           collapse = ","), 
                     #                                    "]"), 
                     #             across(2:11, ~as.character(round(unique(.x),3)))) %>% 
                     #   pivot_longer(1:ncol(.), names_to = "Parameter", values_to = "Estimate")
                     pointests <- data.frame(Point = fixef(x)) %>%
                       rownames_to_column("Parameter")
                     
                     intervalests <- confint(x) %>%
                       as.data.frame() %>%
                       rownames_to_column("Parameter") %>%
                       mutate(across(2:3, ~round(.x, 3))) %>%
                       mutate(CI = paste0("(", `2.5 %`, ",", `97.5 %`, ")")) %>%
                       select(Parameter, CI)
                     
                     ests <- left_join(pointests, 
                                       intervalests, 
                                       by = "Parameter")
                     
                     return(ests)
                   })) %>% 
  bind_rows(.id = "Model")



coefests <- coefests %>%
  mutate(Estimate = paste0(round(Point,3), " ", CI)) %>%
  select(Model, Parameter, Estimate)






table.coefs <- full_join(coefests%>%filter(Parameter%in%c("(Intercept)"))%>%rename(`(Intercept)`=Estimate)%>%select(-Parameter), 
                         coefests%>%filter(Parameter%in%c("population"))%>%rename(Population=Estimate)%>%select(-Parameter), 
                         by = "Model") %>%
  full_join(., 
            coefests%>%
              filter(!Parameter%in%c("(Intercept)", "population"))%>%
              mutate(Term = ifelse(grepl(".+\\^2)$", Parameter), "Quadratic", "Linear"), 
                     Parameter = case_when(grepl("peak_wk", Parameter) ~ "Peak Week", 
                                           grepl("Long Distance_prop", Parameter) ~ "Proportion Commutes Long Distance", 
                                           grepl("distance_mean_km_nozeros", Parameter) ~ "Average Commute Distance (no zeros)", 
                                           grepl("distance_mean_km", Parameter) ~ "Average Commute Distance", 
                                           grepl("county_count", Parameter) ~ "County Count", 
                                           grepl("county_area_km2_mean", Parameter) ~ "Average County Area", 
                                           grepl("state_area_km2_water", Parameter) ~ "State Water Area", 
                                           grepl("state_area_km2", Parameter) ~ "State Land Area", 
                                           grepl("pop_density2", Parameter) ~ "Population Density (Total Area)", 
                                           grepl("pop_density", Parameter) ~ "Population Density (Land Area)", 
                                           grepl("Total.Workers", Parameter) ~ "Total Workers", 
                                           grepl("Internal_prop", Parameter) ~ "Proportion Commutes Internal/Intracounty", 
                                           grepl("Internal", Parameter) ~ "Total Commutes Internal/Intracounty", 
                                           grepl("Short Distance_prop", Parameter) ~ "Proportion Commutes Short Distance", 
                                           grepl("Short Distance", Parameter) ~ "Total Commutes Short Distance", 
                                           grepl("Long Distance", Parameter) ~ "Total Commutes Long Distance", 
                                           grepl("ratio.si", Parameter) ~ "Ratio of Short Distance to Internal Commutes", 
                                           grepl("ratio.ls", Parameter) ~ "Ratio of Long Distance to Short Distance Commutes")) %>%
              pivot_wider(names_from = "Term", values_from = "Estimate"), 
            by = "Model") %>%
  arrange(as.numeric(sub("fit", "", Model)))





## save
save(table.coefs, 
     file = "./03-Output/01-Tables/table_coefs.rds")


save(list = c("ei.df", 
              "fitme", 
              ls(pattern = "^fit[0-9]{1,2}$"), 
              "myvars"),
     file = "./01-Data/02-Analytic-Data/regressions.rdata")


## clean environment
rm(list=ls())
gc()








# 
# 
# #old
# 
# # 
# # 
# # 
# # fit1 <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population), data=ei.df)
# # # summary(fit1)
# # # plot(fit1)
# # 
# # # anova(fit1, fit000,fit00, fit0)
# # 
# # 
# # 
# # 
# # fit2a <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                Internal_prop, data=ei.df)
# # 
# # fit2a <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                 log(Internal), data=ei.df)
# # 
# # # anova(fit2a, fit1, fit000,fit00, fit0)
# # fit2b <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                `Short Distance_prop`, data=ei.df)
# # 
# # fit2b <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                 log(`Short Distance`), data=ei.df)
# # # anova(fit2b, fit1, fit000,fit00, fit0)
# # 
# # fit2c <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #              `Long Distance_prop`, data=ei.df)
# # 
# # fit2c <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                 log(`Long Distance`), data=ei.df)
# # # anova(fit2c, fit1, fit000,fit00, fit0)
# # 
# # fit2d <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                log(ratio.si), data=ei.df)
# # # anova(fit2d, fit1, fit000,fit00, fit0)
# # 
# # fit2e <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #              log(ratio.ls), data=ei.df)
# # # anova(fit2e, fit1, fit000,fit00, fit0)
# # 
# # 
# # 
# # fit2f <- lmer(sqrt(ei)~(1|region)+(1|season)+log(population)+
# #                ratio.ls:ratio.si, data=ei.df)
# # # anova(fit2f, fit1, fit000,fit00, fit0)
# # 
# # # summary(fit2)
# # # plot(fit2)
# # 
# # 
# # 
# # 
# # 
# # 
# # fit3 <- lm(ei~log(population)+proportion.total.workers_International+proportion.total.workers_Interstate+proportion.total.workers_Intrastate+season, data=ei.df)
# # # summary(fit3)
# # # plot(fit3)
# # 
# # 
# # 
# 
# 
# ## aggregate coefficients
# 
# ests.fit1 <- cbind(coef(fit1), confint(fit1)) %>% 
#   as.data.frame() %>%
#   setNames(., nm = c("Beta", "LL", "UL")) %>% 
#   
#   rownames_to_column(., var = "Parameter") %>%
#   mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
#   select(Parameter, Estimate)
# 
# ests.fit2 <- cbind(coef(fit2), confint(fit2)) %>% 
#   as.data.frame() %>%
#   setNames(., nm = c("Beta", "LL", "UL")) %>% 
#   
#   rownames_to_column(., var = "Parameter") %>%
#   mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
#   select(Parameter, Estimate)
# 
# ests.fit3 <- cbind(coef(fit3), confint(fit3)) %>% 
#   as.data.frame() %>%
#   setNames(., nm = c("Beta", "LL", "UL")) %>% 
#   
#   rownames_to_column(., var = "Parameter") %>%
#   mutate(Estimate = paste0(round(Beta, 3), " (", round(LL, 3), ", ", round(UL, 3), ")")) %>%
#   select(Parameter, Estimate)
# 
# 
# 
# ests.fits <- full_join(ests.fit1, ests.fit2, by = "Parameter") %>%
#   full_join(., ests.fit3, by = "Parameter")
# # mutate(across(2:4, ~ifelse(substr(.x,1,1)!="-", paste0(" ", .x), .x)))
# 
# 
# ### make prettier
# ests.fits$Parameter <- c("(Intercept)", "log(Population)", "Proportion International Commutes", "Proportion Interstate Commutes", "Proportion Intrastate Commutes", "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020")
# 
# names(ests.fits) <- c("Parameter", "Model 1", "Model 2", "Model 3")
# 
# ### add row for reference season
# ests.fits <- ests.fits %>% 
#   add_row(.after = 5, Parameter = "2011-2012", `Model 1`=NA, `Model 2`=NA, `Model 3`="1 (ref)")
# 
# 
# ## flextable
# coef.estimates <- flextable(ests.fits)  %>%
#   
#   font(part = "all", fontname = "Arial") %>% 
#   fontsize(part = "header", size = 11) %>%
#   fontsize(part = "body", size = 10) %>% 
#   
#   align(part = "header", align = "center") %>%
#   align(part = "body", j=1, align = "left") %>%
#   align(part = "body", j=2:4, align = "right") %>%
#   
#   bold(part = "header", j=1:4) %>%
#   bold(part = "body", i=c(1,2), j=2) %>%
#   bold(part = "body", i=c(1:5), j=3) %>%
#   bold(part = "body", i=c(1:5,7:9,11:12,14), j=4) %>%
#   
#   
#   autofit(part = "body") %>%
#   line_spacing(part = "all", space = 0.75) %>%
#   
#   border_remove() %>%
#   border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) 
# 
# 
# 
# 
# ## save table
# save_as_docx(coef.estimates, path = "./03-Output/01-Tables/model_coefficient_estimates.docx")
# 
# save(coef.estimates, file = "./03-Output/01-Tables/model_coefficient_estimates.rds")
# 

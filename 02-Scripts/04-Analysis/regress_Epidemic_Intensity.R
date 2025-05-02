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
  mutate(across(c(ei), ~sqrt(.x)), 
         across(c(ratio.si,
                  ratio.ls,
                  ratio.li
                  # , 
                  # state.crowding.dailychange,
                  # state.patchiness.dailychange
         ), ~sqrt(sqrt(.x))), 
         across(c(state.crowding.dailychange, 
                  state.patchiness.dailychange, 
                  state.crowding.dailychangeratio, 
                  state.patchiness.dailychangeratio), 
                ~((.x-min(.x))/(max(.x-min(.x))))^(1/4)),
         across(c(county.pop.mean, 
                  county.popday.mean,
                  population, 
                  populationday,
                  state.crowding,
                  state.patchiness, 
                  state.crowding.day,
                  state.patchiness.day,  
                  # state.crowding.dailychangeratio,
                  # state.patchiness.dailychangeratio,
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
                  ratio.li,
                  state.crowding.dailychange,
                  state.patchiness.dailychange, 
                  county.pop.mean,
                  county.popday.mean,
                  population, 
                  populationday,
                  state.crowding,
                  state.patchiness,
                  state.crowding.day,
                  state.patchiness.day,
                  state.crowding.dailychangeratio,
                  state.patchiness.dailychangeratio,
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






myvars <- c("peak_wk", 
            "pop_density", "pop_density2", 
            "county_count", "county_area_km2_mean", 
            "county.pop.mean", "county.popday.mean",
            "state.crowding", "state.patchiness",
            "state.crowding.day", "state.patchiness.day",
            "state.crowding.dailychange", "state.patchiness.dailychange",
            "state.crowding.dailychangeratio", "state.patchiness.dailychangeratio",
            "state_area_km2", "state_area_km2_land", "state_area_km2_water",
            "Total.Workers", "distance_mean_km", "distance_mean_km_nozeros",
            "Internal", "Internal_prop", "Short Distance", "Short Distance_prop",
            "Long Distance", "Long Distance_prop", 
            "ratio.si", "ratio.ls", "ratio.li"
)








## simple linear
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 ", data = ei.df)\n")))


## with quadratic terms
# 
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 "+I((",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 ")^2), data = ei.df)\n")))




# cubic
cat(dput(paste0("fit",
                1:length(myvars),
                " <- lmer(ei~(1|region)+(1|season)+population+",
                ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
                "+I((",
                ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
                ")^2)",
                "+I((",
                ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
                ")^3), data = ei.df)\n")))

{
  fit1 <- lmer(ei~(1|region)+(1|season)+population+peak_wk+I((peak_wk)^2)+I((peak_wk)^3), data = ei.df)
  fit2 <- lmer(ei~(1|region)+(1|season)+population+pop_density+I((pop_density)^2)+I((pop_density)^3), data = ei.df)
  fit3 <- lmer(ei~(1|region)+(1|season)+population+pop_density2+I((pop_density2)^2)+I((pop_density2)^3), data = ei.df)
  fit4 <- lmer(ei~(1|region)+(1|season)+population+county_count+I((county_count)^2)+I((county_count)^3), data = ei.df)
  fit5 <- lmer(ei~(1|region)+(1|season)+population+county_area_km2_mean+I((county_area_km2_mean)^2)+I((county_area_km2_mean)^3), data = ei.df)
  fit6 <- lmer(ei~(1|region)+(1|season)+population+county.pop.mean+I((county.pop.mean)^2)+I((county.pop.mean)^3), data = ei.df)
  fit7 <- lmer(ei~(1|region)+(1|season)+population+county.popday.mean+I((county.popday.mean)^2)+I((county.popday.mean)^3), data = ei.df)
  fit8 <- lmer(ei~(1|region)+(1|season)+population+state.crowding+I((state.crowding)^2)+I((state.crowding)^3), data = ei.df)
  fit9 <- lmer(ei~(1|region)+(1|season)+population+state.patchiness+I((state.patchiness)^2)+I((state.patchiness)^3), data = ei.df)
  fit10 <- lmer(ei~(1|region)+(1|season)+population+state.crowding.day+I((state.crowding.day)^2)+I((state.crowding.day)^3), data = ei.df)
  fit11 <- lmer(ei~(1|region)+(1|season)+population+state.patchiness.day+I((state.patchiness.day)^2)+I((state.patchiness.day)^3), data = ei.df)
  fit12 <- lmer(ei~(1|region)+(1|season)+population+state.crowding.dailychange+I((state.crowding.dailychange)^2)+I((state.crowding.dailychange)^3), data = ei.df)
  fit13 <- lmer(ei~(1|region)+(1|season)+population+state.patchiness.dailychange+I((state.patchiness.dailychange)^2)+I((state.patchiness.dailychange)^3), data = ei.df)
  fit14 <- lmer(ei~(1|region)+(1|season)+population+state.crowding.dailychangeratio+I((state.crowding.dailychangeratio)^2)+I((state.crowding.dailychangeratio)^3), data = ei.df)
  fit15 <- lmer(ei~(1|region)+(1|season)+population+state.patchiness.dailychangeratio+I((state.patchiness.dailychangeratio)^2)+I((state.patchiness.dailychangeratio)^3), data = ei.df)
  fit16 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2+I((state_area_km2)^2)+I((state_area_km2)^3), data = ei.df)
  fit17 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2_land+I((state_area_km2_land)^2)+I((state_area_km2_land)^3), data = ei.df)
  fit18 <- lmer(ei~(1|region)+(1|season)+population+state_area_km2_water+I((state_area_km2_water)^2)+I((state_area_km2_water)^3), data = ei.df)
  fit19 <- lmer(ei~(1|region)+(1|season)+population+Total.Workers+I((Total.Workers)^2)+I((Total.Workers)^3), data = ei.df)
  fit20 <- lmer(ei~(1|region)+(1|season)+population+distance_mean_km+I((distance_mean_km)^2)+I((distance_mean_km)^3), data = ei.df)
  fit21 <- lmer(ei~(1|region)+(1|season)+population+distance_mean_km_nozeros+I((distance_mean_km_nozeros)^2)+I((distance_mean_km_nozeros)^3), data = ei.df)
  fit22 <- lmer(ei~(1|region)+(1|season)+population+Internal+I((Internal)^2)+I((Internal)^3), data = ei.df)
  fit23 <- lmer(ei~(1|region)+(1|season)+population+Internal_prop+I((Internal_prop)^2)+I((Internal_prop)^3), data = ei.df)
  fit24 <- lmer(ei~(1|region)+(1|season)+population+`Short Distance`+I((`Short Distance`)^2)+I((`Short Distance`)^3), data = ei.df)
  fit25 <- lmer(ei~(1|region)+(1|season)+population+`Short Distance_prop`+I((`Short Distance_prop`)^2)+I((`Short Distance_prop`)^3), data = ei.df)
  fit26 <- lmer(ei~(1|region)+(1|season)+population+`Long Distance`+I((`Long Distance`)^2)+I((`Long Distance`)^3), data = ei.df)
  fit27 <- lmer(ei~(1|region)+(1|season)+population+`Long Distance_prop`+I((`Long Distance_prop`)^2)+I((`Long Distance_prop`)^3), data = ei.df)
  fit28 <- lmer(ei~(1|region)+(1|season)+population+ratio.si+I((ratio.si)^2)+I((ratio.si)^3), data = ei.df)
  fit29 <- lmer(ei~(1|region)+(1|season)+population+ratio.ls+I((ratio.ls)^2)+I((ratio.ls)^3), data = ei.df)
  fit30 <- lmer(ei~(1|region)+(1|season)+population+ratio.li+I((ratio.li)^2)+I((ratio.li)^3), data = ei.df)
}



# quartic
# cat(dput(paste0("fit",
#                 1:length(myvars),
#                 " <- lmer(ei~(1|region)+(1|season)+population+",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 "+I((",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 ")^2)",
#                 "+I((",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 ")^3)",
#                 "+I((",
#                 ifelse(grepl(" ", myvars), sub("(.+)", "`\\1`", myvars), myvars),
#                 ")^4), data = ei.df)\n")))










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
                       mutate(across(2:3, ~round(.x, 3)), 
                              sig = ifelse(sign(`2.5 %`)==sign(`97.5 %`), "*", "")) %>%
                       mutate(CI = paste0("(", `2.5 %`, ",", `97.5 %`, ")", sig)) %>%
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
              mutate(Term = case_when(grepl(".+\\^4)$", Parameter) ~ "Quartic", 
                                      grepl(".+\\^3)$", Parameter) ~ "Cubic", 
                                      grepl(".+\\^2)$", Parameter) ~ "Quadratic", 
                                      TRUE ~ "Linear"), 
                     Parameter = case_when(grepl("peak_wk", Parameter) ~ "Peak Week", 
                                           grepl("county[.]pop[.]mean", Parameter) ~ "Average County Population", 
                                           grepl("county[.]popday[.]mean", Parameter) ~ "Average County Population during Day", 
                                           
                                           grepl("state[.]crowding[.]dailychange", Parameter) ~ "Daily Change in State Crowding",
                                           grepl("state[.]crowding[.]day", Parameter) ~ "State Crowding during Day",
                                           grepl("state[.]crowding", Parameter) ~ "State Crowding", 
                                           grepl("state[.]patchiness[.]dailychange", Parameter) ~ "Daily Change in State Patchiness", 
                                           grepl("state[.]patchiness[.]day", Parameter) ~ "State Patchiness during Day", 
                                           grepl("state[.]patchiness", Parameter) ~ "State Patchiness", 
                                           
                                           grepl("Long Distance_prop", Parameter) ~ "Proportion Commutes Long Distance", 
                                           grepl("distance_mean_km_nozeros", Parameter) ~ "Average Commute Distance (no zeros)", 
                                           grepl("distance_mean_km", Parameter) ~ "Average Commute Distance", 
                                           grepl("county_count", Parameter) ~ "County Count", 
                                           grepl("county_area_km2_mean", Parameter) ~ "Average County Area", 
                                           grepl("state_area_km2_water", Parameter) ~ "State Water Area", 
                                           grepl("state_area_km2_land", Parameter) ~ "State Land Area", 
                                           grepl("state_area_km2", Parameter) ~ "State Area Total", 
                                           grepl("pop_density2", Parameter) ~ "Population Density (Total Area)", 
                                           grepl("pop_density", Parameter) ~ "Population Density (Land Area)", 
                                           grepl("Total.Workers", Parameter) ~ "Total Workers", 
                                           grepl("Internal_prop", Parameter) ~ "Proportion Commutes Internal/Intracounty", 
                                           grepl("Internal", Parameter) ~ "Total Commutes Internal/Intracounty", 
                                           grepl("Short Distance_prop", Parameter) ~ "Proportion Commutes Short Distance", 
                                           grepl("Short Distance", Parameter) ~ "Total Commutes Short Distance", 
                                           grepl("Long Distance", Parameter) ~ "Total Commutes Long Distance", 
                                           grepl("ratio.si", Parameter) ~ "Ratio of Short Distance to Internal Commutes", 
                                           grepl("ratio.ls", Parameter) ~ "Ratio of Long Distance to Short Distance Commutes", 
                                           grepl("ratio.li", Parameter) ~ "Ratio of Long Distance to Internal Commutes")) %>%
              pivot_wider(names_from = "Term", values_from = "Estimate"), 
            by = "Model") %>%
  arrange(as.numeric(sub("fit", "", Model)))


table.coefs <- full_join(table.coefs, 
                         lapply(fits, 
                                (\(x){
                                  temp<-anova(x,fitme)
                                  return(data.frame(`Model P`=temp$`Pr(>Chisq)`[2], 
                                                    AIC=temp$AIC[2], 
                                                    BIC=temp$BIC[2]))
                                }))%>%
                           bind_rows(.id = "Model")%>%
                           mutate(across(where(is.numeric), 
                                         ~round(.x,3))), 
                         by = "Model")


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

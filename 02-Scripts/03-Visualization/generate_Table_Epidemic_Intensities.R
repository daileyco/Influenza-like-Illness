# Create a table for the epidemic intensities

## load the data
load("./01-Data/02-Analytic-Data/ili_ei.rds")


## packages
library(dplyr)
library(tidyr)
library(flextable)
library(officer)



## calculate an average epidemic intensity
## pivot the dataframe wider (more like a table)
ei <- ili.ei %>%
  group_by(region) %>%
  mutate(Average = mean(ei, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(season) %>%
  # mutate(ei = round(ei,3), 
  #        Average = round(Average,3)) %>%
  pivot_wider(names_from = season, values_from = ei) %>%
  arrange(region) %>%
  select(Region=region, matches("^2"), Average)
  

ei <- bind_rows(ei, 
                ei %>% 
                  summarise(across(matches("^[2|A]"), ~mean(.x, na.rm=T))) %>% 
                  mutate(Region="Average")) %>%
  mutate(across(matches("^[2|A]"), ~round(.x,3)))



## create flextable object styling table
ei.table <- flextable(ei) %>%
  
  font(part = "all", fontname = "Arial") %>% 
  fontsize(part = "header", size = 11) %>%
  fontsize(part = "body", size = 10) %>% 
  
  align(part = "header", align = "center") %>%
  align(part = "body", j=1, align = "left") %>%
  align(part = "body", j=2:10, align = "right") %>% 
  align(part = "body", j=11, align = "center") %>%
  # align(part = "body", i=53, align = "center") %>%
  
  bold(part = "header", j=1:10) %>%
  italic(part = "all", j=11) %>%
  italic(part = "body", i=53) %>%
  
  
  # which(as.matrix(ei[,2:10])==0, arr.ind = T)
  compose(part="body", j=2, i=10, value = as_paragraph("0")) %>%
  # which(as.matrix(ei[,2:10])==1, arr.ind = T)
  compose(part="body", j=5, i=8, value = as_paragraph("1")) %>%
  
  # autofit(part = "header") %>%
  # autofit(part = "body") %>%
  width(j=1, width = 1.35) %>%
  width(j=2:11, width = 0.9) %>%
  # line_spacing(part = "all", space = 0.75) %>%
  
  border_remove() %>%
  border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) %>%
  
  border(part = "all", j = 11, border.left = fp_border_default(color = "black", style = "dashed", width = 1)) %>%
  border(part = "body", i = 53, border.top = fp_border_default(color = "black", style = "dashed", width = 1)) 



## save in word doc
save_as_docx(ei.table, path = "./03-Output/01-Tables/epidemic_intensities.docx", 
             pr_section = prop_section(page_size = page_size(orient="landscape")))

## save as r object
save(ei.table, file = "./03-Output/01-Tables/epidemic_intensities.rds")


## clean environment
rm(list = ls())
gc()





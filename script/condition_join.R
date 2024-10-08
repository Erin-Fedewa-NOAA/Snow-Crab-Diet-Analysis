#Goal: Combine sample-level DNA metabarcoding data with energetic condition, 
  #fatty acid, and survey haul level data 

#Author: Erin Fedewa

# load ----
library(tidyverse)
library(sf)
library(ggmap)
library(gganimate)
library(viridis)
library(ggridges)
library(RColorBrewer)
library(broom)

condition <- read.csv("./data/FA_biomarker_master.csv")
stomach <- read.csv("./data/crabstomach_taxontable_20231129.csv")

########################################
#tidy FA data
tidy_FA <- condition %>%
    filter(year == 2022,
         project == "NPRB") %>%
  mutate(vial_ID = str_replace_all(vial_id, "2022-AKK", "162")) %>% #this is goofy, will fix....
  select(-vial_id) %>%
  mutate(vial_id = str_replace_all(vial_ID, "2022-VA", "94")) %>%
    select(mid_latitude, mid_longitude, lme, vial_id, Diatom_Indicator_percWT, 
           Total_FA_Conc_WWT, gear_temperature)
   
    
#tidy stomach data 
stomach %>%
  filter(taxon != "Decapoda", #host DNA
         !(alternative_ID %in% c("NC1", "NC2", "NC3"))) %>% #extraction blank
  select(-collection_month, -collection_day, -extraction_ID, -primer,
         -location3, -sample_type) %>%
  rename(vial_id = alternative_ID) %>%
  group_by(Sample_ID) %>%
  mutate(ReadsPerSample = sum(reads)) %>%
  mutate(read_prop = reads/ReadsPerSample) %>%
#join to FA data
  left_join(tidy_FA, by="vial_id") -> diet

#write csv
write.csv(diet, "./output/diet_haul_FA_master.csv")



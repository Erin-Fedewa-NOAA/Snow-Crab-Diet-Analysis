#Goal: Preliminary maps and figures linking stomach contents of ~20 snow 
  #crab collected in 2022 to individual level FA and condition data

#Note: Prey items have been grouped roughly by class- see Taxon metadata for 
  #description of groupings by taxonomic level 

# load ----
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(ggridges)
library(RColorBrewer)
library(broom)
library(pals)

crab_diet <- read.csv("./output/diet_haul_FA_master.csv")

####################################
#Diet plots
  #Note: each sample ID represents a PCR replicate (usually two, sometimes more)

crab_diet %>%
  ggplot(aes(x=Sample_ID, y=reads, fill=assigned_taxon_class)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    y = "sequencing reads",
    x = "sample ID",
    title = "snow crab stomach")  +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.95),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 6))  

crab_diet %>%
  filter(ReadsPerSample > 1000) %>%
  ggplot(aes(x=Sample_ID, y=read_prop, fill=assigned_taxon_class)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    y = "proportion of sequencing reads",
    x = "Vial ID")  +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.95),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 6)) +
  scale_fill_manual(values=as.vector(kelly(n=14))) 

###################
#Maps 

#plot sampling locations  
world <- ne_countries(scale = "medium", returnclass = "sf")

crab_diet %>% 
  group_by(vial_id, mid_latitude, mid_longitude) %>%
  summarise(n_crab=n()) -> plot

ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot, aes(x = mid_longitude, y = mid_latitude, size=n_crab), color= "light blue")+
  coord_sf(xlim = c(-180, -160), ylim = c(56, 66), expand = FALSE) +
  theme_bw() 
ggsave("./figures/data exploration/n_year.png")

#point size relative to prop of sequencing reads with diatoms 
crab_diet %>% 
  group_by(Sample_ID, vial_id, mid_latitude, mid_longitude) %>%
  summarise(prop_diatom = read_prop[assigned_taxon_class=="Bacillariophyceae"]) -> plot2

ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot2, aes(x = mid_longitude, y = mid_latitude, size=prop_diatom), color= "light blue")+
  coord_sf(xlim = c(-180, -160), ylim = c(56, 66), expand = FALSE) +
  theme_bw() 



crab_diet %>% 
  group_by(Sample_ID, vial_id, Total_FA_Conc_WWT, Diatom_Indicator_percWT,
           mid_latitude, mid_longitude) %>%
  summarise(prop_diatom = read_prop[assigned_taxon_class=="Bacillariophyceae"]) -> dat

dat %>% 
  ggplot() +
  geom_point(aes(prop_diatom, Total_FA_Conc_WWT))

dat %>% 
  ggplot() +
  geom_point(aes(prop_diatom, Diatom_Indicator_percWT))

# Libraries
library(tidyverse)
library(plyr)
library(faraway)
library(cowplot)
library(reshape2)
library(corrplot)
library(scales)
library(ggthemes)
library(gridExtra)
library(patchwork)
library(gghighlight)
library(ggdark)
library(viridis)
library(DT)
library(plotly)
library(readxl)
library(knitr)
library(magick)

# Data path
#path <- "data/"

# read site data
sites <- read_excel("C:/Users/wclagett/Documents/IAQ-V-VOC/data/site_info.xlsx") %>% 
  select(1:18)

#sites
#site 040
site_040 <- sites %>% 
  filter(site_id == "040")
#SITE 063 A
site_063A <- sites %>% 
  filter(name == "High Desert A")
#SITE 063 B
site_063B <- sites %>% 
  filter(name == "High Desert B")
#SITE 066
site_066 <- sites %>% 
  filter(site_id == "066")
# SITE 079
site_079 <- sites %>% 
  filter(site_id == "079")
# SITE 085
site_085 <- sites %>% 
  filter(site_id == "085")
# SITE 086
site_086 <- sites %>% 
  filter(site_id == "086")
# SITE 099
site_099 <- sites %>% 
  filter(site_id == "099")
# SITE 103
site_103 <- sites %>% 
  filter(site_id == "103")
# SITE 107
site_107 <- sites %>% 
  filter(site_id == "107")
# site 108
site_108 <- sites %>% 
  filter(site_id == "108")
# site 094
site_094 <- sites %>% 
  filter(site_id == "094")
#site 106
site_106 <- sites %>% 
  filter(site_id == "106")
# site 105
site_105 <- sites %>% 
  filter(site_id == "105")
# site 104
site_104 <- sites %>% 
  filter(site_id == "104")
#site 089
site_089 <- sites %>% 
  filter(site_id == "089")
# site 002
site_002 <- sites %>% 
  filter(site_id == "002")
# site 101
site_101 <- sites %>% 
  filter(site_id == "101")
# site 109
site_109 <- sites %>% 
  filter(site_id == "109")

#locations
table(sites$type)
#outdoor
outdoor <- sites %>% 
  filter(type == "Outdoor")
#kitchens
kitchens <- sites %>% 
  filter(type =="kitchen/dining")
#staff locations
offices <- sites %>% 
  filter(type %in% c("office" , "lounge"))
#classrooms
classrooms <- sites %>% 
  filter(type == "classroom")
#temp living
temp_living <- sites %>% 
  filter(type == "barrack")
#apartments
apartments <- sites %>% 
  filter(type == "apartment")
#lobbys
lobby <- sites %>% 
  filter(type %in% c("lobby", "entrance"))
#others
other <- sites %>% 
  filter(type %in% c("other" , "corridor"))
#recreation
recreation <- sites %>% 
  filter(type == "recreation")
#medical
medical <- sites %>% 
  filter(type %in% c("exam" , "therapy"))
#indoor locations
indoor <- sites %>% 
  filter(type != "Outdoor")

#analyte category objects
#Alcohol
alcohol <- sites %>%
  filter(category == "alcohol")
#Aldehyde
aldehyde <- sites %>% 
  filter(category == "aldehyde")
#Straight Chain
straight_chain <- sites %>% 
  filter(category == "straight chain")
#Aromatic
aromatic <- sites %>% 
  filter(category == "aromatic")
#Btex
btex <- sites %>% 
  filter(category == "btex")
#chlorinated
chlorinated <- sites %>% 
  filter(category == "chlorinated")
#Ketone
ketone <- sites %>% 
  filter(category == "ketone")
#Other
other <- sites %>% 
  filter(category == "other")


# # function to plot total voc conc by room
p_conc_room <- function(df, site_id){

ggplot(df, aes(x = reorder(room, conc.),
               y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle(paste0("Site: ", site_id,  " VOC Samples by Canister Location")) +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))") +
  theme_bw() +
  theme(axis.text.y = element_blank())
}

#location type plots
p_locations <- function(df, type){
  
  ggplot(df, aes(x = reorder(analyte, conc.),
                         y = conc., color = site_id,
                         text = paste("Analyte: ", analyte,
                                      "<br> Conc. :", conc.,
                                      "<br> Class: ", category))) +
    geom_point(shape = 18, size = 5, alpha = 0.5) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    ggtitle(paste0("VOC Concentrations in ", type, " Locations in CO")) +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    labs(x = "Ananlyte", y = "Concentration") +
    scale_color_manual(name = "Site ID",
                       values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                  "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                  "#b9cda1", "#096013", "#afe642", "#3aa609",
                                  "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                  "#d9c937", "#9f04fc"))
  
}

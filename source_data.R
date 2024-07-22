#-----------
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
library(corrr)


#-----------
# read site data
sites <- read_excel("C:/Users/wclagett/Documents/IAQ-V-VOC/data/site_info.xlsx") %>% 
  select(1:19)

#load limit of detection
lod <- read_excel("C:/Users/wclagett/Documents/IAQ-V-VOC/data/summa_lod.xlsx") %>% 
  select(2:4)


#-----------
#sites
#site 040
site_040 <- sites %>% 
  filter(site_id == "040")
#indoor group
indoor_040 <- site_040 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_040 <- site_040 %>% 
  filter(room_name =="Outdoor")

#SITE 063 A
site_063A <- sites %>% 
  filter(name == "High Desert A")
#indoor group
indoor_063A <- site_063A %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_063A <- site_063A %>% 
  filter(room_name =="Outdoor")

#SITE 063 B
site_063B <- sites %>% 
  filter(name == "High Desert B")
#indoor group
indoor_063B <- site_063B %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_063B <- site_063B %>% 
  filter(room_name =="Outdoor")

#SITE 066
site_066 <- sites %>% 
  filter(site_id == "066")
#indoor group
indoor_066 <- site_066 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_066 <- site_066 %>% 
  filter(room_name =="Outdoor")

# SITE 079
site_079 <- sites %>% 
  filter(site_id == "079")
#indoor group
indoor_079 <- site_079 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_079 <- site_079 %>% 
  filter(room_name =="Outdoor")

# SITE 085
site_085 <- sites %>% 
  filter(site_id == "085")
#indoor group
indoor_085 <- site_085 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_085 <- site_085 %>% 
  filter(room_name =="Outdoor")

# SITE 086
site_086 <- sites %>% 
  filter(site_id == "086")
#indoor group
indoor_086 <- site_086 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_086 <- site_086 %>% 
  filter(room_name =="Outdoor")

# SITE 099
site_099 <- sites %>% 
  filter(site_id == "099")
#indoor group
indoor_099 <- site_099 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_099 <- site_099 %>% 
  filter(room_name =="Outdoor")

# SITE 103
site_103 <- sites %>% 
  filter(site_id == "103")
#indoor group
indoor_103 <- site_103 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_103 <- site_103 %>% 
  filter(room_name =="Outdoor")

# SITE 107
site_107 <- sites %>% 
  filter(site_id == "107")
#indoor group
indoor_107 <- site_107 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_107 <- site_107 %>% 
  filter(room_name =="Outdoor")

# site 108
site_108 <- sites %>% 
  filter(site_id == "108")
#indoor group
indoor_108 <- site_108 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_108 <- site_108 %>% 
  filter(room_name =="Outdoor")

# site 094
site_094 <- sites %>% 
  filter(site_id == "094")
#indoor group
indoor_094 <- site_094 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_094 <- site_094 %>% 
  filter(room_name =="Outdoor")

#site 106
site_106 <- sites %>% 
  filter(site_id == "106")
#indoor group
indoor_106 <- site_106 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_106 <- site_106 %>% 
  filter(room_name =="Outdoor")

# site 105
site_105 <- sites %>% 
  filter(site_id == "105")
#indoor group
indoor_105 <- site_105 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_105 <- site_105 %>% 
  filter(room_name =="Outdoor")

# site 104
site_104 <- sites %>% 
  filter(site_id == "104")

#site 089
site_089 <- sites %>% 
  filter(site_id == "089")
#indoor group
indoor_089 <- site_089 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_089 <- site_089 %>% 
  filter(room_name =="Outdoor")

# site 002
site_002 <- sites %>% 
  filter(site_id == "002")
#indoor group
indoor_002 <- site_002 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_002 <- site_002 %>% 
  filter(room_name =="Outdoor")

# site 101
site_101 <- sites %>% 
  filter(site_id == "101")
#indoor group
indoor_101 <- site_101 %>% 
  filter(room_name != "Outdoor")
#outdoor group
outdoor_101 <- site_101 %>% 
  filter(room_name =="Outdoor")

# site 109
site_109 <- sites %>% 
  filter(site_id == "109")



#-----------
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
#corridors
corridors <- sites %>% 
  filter(type %in% c("balcony" , "corridor"))
#recreation
recreation <- sites %>% 
  filter(type == "recreation")
#medical
medical <- sites %>% 
  filter(type %in% c("exam" , "therapy"))
#indoor locations
indoor <- sites %>% 
  filter(type != "Outdoor")



#-----------
#analyte categories
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



#-----------
#functions

#data table object
data_table <- function(sites, site_id){
  filtered_table <- sites %>%
    filter(site_id == !!site_id) %>% 
    select(4, 7, 9, 18)
  
  return(filtered_table)
  
}

#data table
site_dt <- function(data, site_id) {
  datatable(data, colnames = c("Location", "Analyte", "Concentration", "Category"),
            options = list(pageLength = 10), rownames = FALSE,
            caption = paste("Site"
                            , site_id, "Table, Concentrations: ppb(v) or methane ppb(v)"))
}

#Get top n analytes
top_n_analytes <- function(data, n = 61) {
  top_analytes <- data %>% 
    group_by(analyte) %>% 
    arrange(desc(conc.)) %>% 
    ungroup() %>% 
    top_n(n, conc.)
  
  return(top_analytes)
  
}

#Get top 10 ratios
top_n_or <- function(data, room_name, n = 61) {
  top_n_or <- data %>% 
  filter(room_name == room_name) %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio)) %>% 
    ungroup() %>% 
    top_n_or(n, od_ratio)
  
  return(top_n_or)

}


#from chat gpt for the above
get_top_analytes <- function(data, room_name, n = 5) {
  top_analytes <- data %>% 
    filter(room_name == room_name) %>% 
    group_by(analyte) %>% 
    arrange(desc(od_ratio)) %>% 
    ungroup() %>% 
    top_n(n, od_ratio)
  
  return(top_analytes)
}

#-----------
#plot functions

#box plot 
box_plot <- function(df){
   
  ggplot(df, aes(x = reorder(analyte, conc.), y = conc.)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  ggtitle("Boxplot for All Ananlytes")
  
}


#all analytes plots by room id
p_site <- function(df, site_id){
  
  ggplot(df, aes(x = reorder(analyte, conc.),
                              y = conc., color = room_name,
                 text = paste("Analyte: ", analyte,
                              "<br> Conc. :", conc.,
                              "<br> Class: ", category))) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes", y = "Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle(paste0("Site: ", site_id, " Summa Canister Deployment")) +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) 
  
}


#top 10 analytes plot for site
top_plot <- function(df, site_id, fill){
  
  ggplot(df, aes(x = reorder(analyte, conc.), y = conc.)) +
    geom_bar(stat = "identity", fill = fill) +
    labs(x = "Analyte", y = "Concentration (ppb)") +
    ggtitle(paste0("Site: ", site_id, " Top 10 Analytes")) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1))
  
}


#top 10 analytes plot for loactions
loc_top_plot <- function(df, fill, location){
  
  ggplot(df, aes(x = reorder(analyte, conc.), y = conc.)) +
    geom_bar(stat = "identity", fill = fill) +
    labs(x = "", y = "") +
    ggtitle(location) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1))
  
}

#ratios box plot
r_box_plot <- function(df){
  
  ggplot(df, aes(x = reorder(analyte, conc.), y = conc.)) +
    geom_boxplot() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    labs(x = "Ananlyte", y = "Concentration") +
    ggtitle("Boxplot for All Indoor Ananlytes")
  
}

#ratio plot for all locations
r_p_site <- function(df, site_id){
  
  ggplot(df, aes(x = reorder(analyte, od_ratio),
                 y = od_ratio, color = room_name,
                 text = paste("Analyte: ", analyte,
                              "<br> Conc. :", conc.,
                              "<br> Class: ", category))) +
    geom_point(shape = 18, size = 5, alpha = 0.5) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    labs(x = "Analytes", y = "Concentration\n(VOC ppbv or methane ppmv)") +
    ggtitle(paste0("Site: ", site_id, " Indoor to Outdoor Ratios")) +
    scale_color_manual(name = "Room ID",
                       values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                  "tomato2", "midnightblue")) 
  
}


#to plot total voc conc by room
p_conc_room <- function(df, site_id){

ggplot(df, aes(x = reorder(room_name, conc.),
               y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = after_stat(y)), fun = "sum", geom = "text",
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
    geom_point(shape = 18, size = 3, alpha = 0.5) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    ggtitle(paste0("VOC Concentrations in ", type, " Locations in CO")) +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    labs(x = "Ananlyte", y = "Concentration ppb(v)") +
    scale_color_manual(name = "Site ID",
                       values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                  "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                  "#b9cda1", "#096013", "#afe642", "#3aa609",
                                  "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                  "#d9c937", "#9f04fc"))
  
}

#analyte category plots
p_category <- function(df, category){
  
  ggplot(df, aes(x = reorder(analyte, conc.),
                      y = conc., color = site_id,
                      text = paste("Site: ", site_id,
                                   "<br> Conc. :", conc.))) +
    geom_point(shape = 18, size = 3, alpha = 0.5) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10)) +
    labs(x = "", y = "") +
    ggtitle(paste0("VOC Concentrations for ", category)) +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    labs(x = "Ananlyte", y = "Concentration ppb(v)") +
    scale_color_manual(name = "Room ID",
                       values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                  "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                  "#b9cda1", "#096013", "#afe642", "#3aa609",
                                  "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                  "#d9c937", "#9f04fc")) +
    theme(panel.grid.minor = element_line(linetype = "dashed"))
  
}


#facet wrap plot by cansiter location
fct_wrap <- function(df, site_id){
  ggplot(df, aes(x = reorder(analyte, conc.),
                     y = conc.)) +
  geom_point(color = "#50C878", size = 3, shape = 18, alpha = 0.5) +
  xlab("Analytes") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~room_name, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle(paste0("Site ", site_id," Summa Cannister Deployment",
          "Grouped by Cannister Location"))
  
}

#facet wrap plot by analyte category
cat_fct_wrap <- function(df, site_id){
  ggplot(df, aes(x = reorder(analyte, conc.),
                 y = conc.)) +
    geom_point(color = "#50C878", size = 3, shape = 18, alpha = 0.5) +
    xlab("Analytes") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    facet_wrap(~category, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
    labs(x = "Analytes",
         y = expression(atop("Concentration",
                             paste("(VOC ppbv or methane ppmv)")))) +
    ggtitle(paste0("Site ", site_id," Summa Cannister Deployment",
                   "Grouped by Analyte Category"))
  
}

#facet wrap plot for ratios
r_fct_wrap <- function(df, site_id){
  
  ggplot(df, aes(x = reorder(analyte, od_ratio),
                         y = conc.)) +
    geom_point(color = "#50C878", size = 3, shape = 18, alpha = 0.5) +
    xlab("Analytes") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    facet_wrap(~ category, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
    labs(x = "Analytes",
         y = expression(atop("Indoor to Otdoor Ratios",
                             paste("(VOC ppbv or methane ppmv)")))) +
    ggtitle(paste0("Site ", site_id," Indoor to Outdoor Ratios"))
  
  
}

#plot for indiviudal rooms

room_plot <- function(df, site_id, location){
  
  ggplot(df, aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
    geom_point(color = "midnightblue", shape = 18, size = 5) +
    scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
    theme(legend.position = "none") +
    labs(x = "Analytes",
         y = expression(atop("Concentration",
                             paste("(VOC ppbv or methane ppmv)")))) +
    ggtitle(paste0("Site ", site_id, paste0(" - ", (location))))
  
}

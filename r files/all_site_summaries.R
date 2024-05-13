#packages
library(tidyverse)
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

#load data
sites <- read_excel("data/voc_samples_all.xlsx")

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
  filter(type == "classrrom")
#temp living
temp_living <- sites %>% 
  filter(type == "barrack")
#apartment
apartment <- sites %>% 
  filter(type == "apartment")
#lobbys
lobby <- sites %>% 
  filter(type == "lobby")
#others
other <- sites %>% 
  filter(type == "other")
#recreation
recreation <- sites %>% 
  filter(type == "recreation")
#medical
medical <- sites %>% 
  filter(type == "exam")

#VOC classes as objects
alc <- c("isopropanol", "butanol")
ald <- c("acetaldehyde")
aro <- c("cyclopentane", "cyclohexane", "methylcyclohexane")
btx <- c("benzene", "toluene", "ethylbenzene", "m+p-xylene", "o-xylene",
         "isopropylbenzene", "n-propylbenzene", "3-ethyltoluene",
         "4-ethyltoluene", "1,3,5-trimethylbenzene", "2-ethyltoluene",
         "1,2,4-trimethylbenzene", "1,2,3-trimethylbenzene",
         "1,3-diethylbenzene", "1,4-diethylbenzene")
chl <- c("C2HCl3", "C2Cl4")
kt <- c("acetone")
oth <- c("isoprene", "styrene", "acetonitrile", "methylethylketone",
         "a-pinene", "b-pinene", "limonene", "camphene", "methane")
stc <- c("ethane", "propane", "i-butane", "n-butane", "i-pentane",
         "n-pentane", "n-hexane", "2,4 dimethylpentane", "n-heptane",
         "2,3-dimethylpentane", "2-methylhexane", "3-methylhexane",
         "2,2,4-trimethylpentane", "2,3,4-trimethylpentane",
         "2-methylheptane", "3-methylheptane", "n-octane", "n-nonane",
         "n-decane", "ethene", "propene", "t-2-butene", "1-butene",
         "c-2-butene", "t-2-pentene", "1-pentene", "cis-2-pentene",
         "ethyne")

#data table
sites_table <- sites %>% 
  select(1:3,6,7,16)
datatable(sites_table, colnames = c("Site ID", "Name", "Location", "Analyte",
                              "Concentration", "Category"),
          options = list(pageLenght = 10), rownames = FALSE,
          caption = "All Sites Table, Concentrations: ppb(v) or methane ppb(v)")

#site summaries concentartion sums
#site 040
ggplot(site_040, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 040 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 063 A
ggplot(site_063A, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063A VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 063 B
ggplot(site_063B, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063B VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 066
ggplot(site_066, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 066 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 079
ggplot(site_079, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 079 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 085
ggplot(site_085, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 085 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 086
ggplot(site_086, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 086 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 099
ggplot(site_099, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 099 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 103
ggplot(site_103, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 103 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 107
ggplot(site_107, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 107 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#sites 108
ggplot(site_108, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 108 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())
#site 094
ggplot(site_094, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 094 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#outdoor concentrations
pc_outdoor <- ggplot(outdoor, aes(x = reorder(analyte, conc.),
                                  y = conc., color = site_id,
                     text = paste("Analyte: ", analyte,
                                  "<br> Conc. :", conc.,
                                  "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Outdoor Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_outdoor, tooltip = "text")

#kitchen/dinning locations
pc_kitchens <- ggplot(kitchens, aes(x = reorder(analyte, conc.),
                                  y = conc., color = site_id,
                                  text = paste("Analyte: ", analyte,
                                               "<br> Conc. :", conc.,
                                               "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Kitchen & Dining Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_kitchens, tooltip = "text")

#staff locations
pc_offices <- ggplot(offices, aes(x = reorder(analyte, conc.),
                                  y = conc., color = site_id,
                                  text = paste("Analyte: ", analyte,
                                               "<br> Conc. :", conc.,
                                               "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Staff Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_offices, tooltip = "text")












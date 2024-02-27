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

new <- read_csv("./data/VOC samples all.csv")
goofy <- new %>% 
  filter(site == "040")

ggplot(goofy, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun.y = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 040 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())


sites <- (read_csv(file = "./data/OSHA_voc.csv")) %>%
  filter(analyte %in% c("benzene", "toluene", "ethylbenzene", "m+p-xylene",
         "o-xylene", "styrene", "toluene", "acetaldehyde", "acetone",
         "n-hexane", "C2Cl4", "C2HCl3")) %>% 
  mutate(haz_ratio = conc./OSHA_8hr)

site_040 <- sites %>% 
  filter(site_id == "040") 

bears <- site_40 %>% 
  filter(room_name == "Bears")

offices <- sites %>% 
  filter(room_type == "office")

outdoor <- sites %>% 
  filter(room_type == "outdoor")

kitchens <- sites %>% 
  filter(room_type =="kitchen/dining")

p_site_040 <- ggplot(site_040, aes(x = reorder(analyte, haz_ratio),
                  y = haz_ratio, color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Hazard Ratio") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))


#Office haz ratios
ph_offices <- ggplot(offices, aes(x = reorder(analyte, haz_ratio),
                    y = haz_ratio, color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("Office Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Hazard Ratio") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#office concentrations
pc_offices <- ggplot(offices, aes(x = reorder(analyte, conc.),
                                  y = conc., color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Office Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#kitchen haz ratios
ph_kitchens <- ggplot(kitchens, aes(x = reorder(analyte, haz_ratio),
                y = haz_ratio, color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Haz Ratios: Non-Residential Kitchens in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Hazard Ratio") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#kitchen concentrations
pc_kitchens <- ggplot(kitchens, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Concentrations: Non-Residential Kitchens in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#kitchens facet wrap, grouped by city (haz ratio)
ph_kitchens_fctw <- ggplot(kitchens, aes(x = reorder(analyte, haz_ratio),
                                       y = haz_ratio, color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~city, scales = "free_y") +
  theme_bw() +
  ggtitle("Kitchen Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Haz Ratio") +
  scale_color_manual(name = "Site Name",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue", "red", "purple",
                                "cyan", "green"))

ph_kitchcat_fctg <- ggplot(kitchens, aes(x = reorder(analyte, haz_ratio),
                                         y = haz_ratio, color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(~category, scales = "free_y") +
  theme_bw() +
  ggtitle("Kitchen Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Haz Ratio") +
  scale_color_manual(name = "Site Name",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue", "red", "purple",
                                "cyan", "green"))

#kitchens facet wrap, grouped by city (concentration)
pc_kitchens_fctw <- ggplot(kitchens, aes(x = reorder(analyte, conc.),
                                         y = conc., color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~city, scales = "free_y") +
  theme_bw() +
  ggtitle("Kitchen Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site Name",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue", "red", "purple",
                                "cyan", "green"))

#outdoor haz ratios
ph_outdoor <- ggplot(outdoor, aes(x = reorder(analyte, haz_ratio),
                                  y = haz_ratio, color = city)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Haz Ratios: Outdoor Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Hazard Ratio") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#outdoor concentrations
pc_outdoor <- ggplot(outdoor, aes(x = reorder(analyte, conc.),
                                  y = conc., color = city)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Concentrations: Outdoor Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))
  

#outdoor facet wrap, grouped by city (Haz Ratio)
ph_outdoor_fctw <- ggplot(outdoor, aes(x = reorder(analyte, haz_ratio),
                                    y = haz_ratio, color = site_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~city, scales = "free_y") +
  theme_bw() +
  ggtitle("Outdoor Locations in CO grouped by City") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Haz Ratio") +
  scale_color_manual(name = "Site Name",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue", "red", "purple",
                                "cyan", "green"))

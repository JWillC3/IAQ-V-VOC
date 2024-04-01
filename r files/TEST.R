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

# SITE 040 
path <- "C:/Users/lippmann/OneDrive - Colostate/Summa Canisters/VOC_Data/data/"
#path <- "C:/Users/clagett/OneDrive - Colostate/Summa Canisters/VOC_Data/data/"

sites <- read_csv(paste0(path, "voc_samples_all.csv"))

site_040 <- sites %>% 
  filter(site == "040")

ggplot(site_040, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 040 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 063 A
site_063A <- new %>% 
  filter(name == "High Desert A")

ggplot(site_063A, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063A VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

site_063B <- new %>% 
  filter(name == "High Desert B")

ggplot(site_063B, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063B VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 066
site_066 <- new %>% 
  filter(site == "066")

ggplot(site_066, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 066 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 079
site_079 <- new %>% 
  filter(site == "079")

ggplot(site_079, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 079 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 085
site_085 <- new %>% 
  filter(site == "085")

ggplot(site_085, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 085 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 086
site_086 <- new %>% 
  filter(site == "086")

ggplot(site_086, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 086 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 099
site_099 <- new %>% 
  filter(site == "099")

ggplot(site_099, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 099 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 103
site_103 <- new %>% 
  filter(site == "103")

ggplot(site_103, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 103 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 107
site_107 <- new %>% 
  filter(site == "107")

ggplot(site_107, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 107 VOC Samples by Canister Location") +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())




# QTrak Data
data_qtrak <- read_rds(paste0(path_data, "/data_qtrak.rds")) %>%
  filter(datetime >= ymd_hms(datetime_start, tz = "US/Mountain")) %>%
  filter(datetime < ymd_hms(datetime_end, tz = "US/Mountain")) %>%
  left_join(inst_log$qtrak, by = "id_inst") %>%
  select(-level, date_installed, date_uninstalled)

voc <- data_qtrak %>%
  select(id_room, datetime, room, val, var) %>%
  mutate(location = gsub("_", " ", str_to_title(room))) %>%
  filter(grepl('totalvoc low24_ppm', var)) %>%
  mutate(voc = val*1000)

baseline <- data_qtrak %>%
  group_by(id_room, room) %>%
  mutate(min_time = min(datetime)) %>%
  #filter(datetime <= min_time + minutes(20)) %>%
  filter(grepl('totalvoc low24_ppm', var)) %>%
  summarise(baseline = median(val)*1000) %>%
  ungroup %>%
  mutate(avg_baseline = mean(baseline)) %>% group_by(id_room) %>%
  mutate(baseline_fix = avg_baseline - baseline)

# Join the baseline data frame with the voc data frame
voc <- voc %>%
  left_join(baseline, by = "id_room")

# Subtract the baseline from the voc values
voc <- voc %>%
  mutate(voc_baseline_subtracted = voc + baseline_fix) 

# SITE 040 ROOM PAIR CORRELATIONS
SITE_040 <- new %>%
  filter(site == "040")

#bears & frogs 
bears_data <- subset(SITE_040, room == "Bears")
frogs_data <- subset(SITE_040, room == "Frogs")

# Select only the columns containing concentration data
bears_conc <- bears_data[, grepl("^conc\\.", names(bears_data))]
frogs_conc <- frogs_data[, grepl("^conc\\.", names(frogs_data))]

# Calculate correlations for each pair of analytes within Bears and Frogs
bears_cor <- cor(bears_conc, method = "spearman")
frogs_cor <- cor(frogs_conc, method = "spearman")

print("Correlation matrix for Bears:")
print(bears_cor)
print("Correlation matrix for Frogs:")
print(frogs_cor)

#bears & lesson prep
#bears & monkeys
#bears & outdoor
#bears & office 
#frogs & lesson prep
#frogs & monkeys
#frogs & office
#frogs & outdoor
#lesson prep & monkeys 
#lesson prep & office 
#lesson prep & outdoor 
#monkeys & office
#monkeys & outdoor
#office & outdoor 

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

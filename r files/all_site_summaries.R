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
  filter(site_id == "063 A")
#SITE 063 B
site_063B <- sites %>% 
  filter(site_id == "063 B")
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
#apartments
apartments <- sites %>% 
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


#Jade TWA TVOC Scatter Plots by room type

# Function to read and process each site's data
read_site_data <- function(site_name) {
  # Read Excel file for the site
  sites <- read_csv(paste0(path, "voc_samples_all.csv"))
  
  # Add site name is a column
  sites$site <- site_name
  
  # Select necessary columns
  sites <- select(sites, type, analyte, conc.,site)
  
  # Return processed data
  return(sites)
}

# List of site names
site_names <- c(
  "site_040", "site_063A", "site_063B", "site_066",
  "site_079", "site_085", "site_086", "site_099",
  "site_103", "site_107"
)

# Read data for each site and combine into a single dataframe
all_data <- map_dfr(site_names, read_site_data)

#Plot dimensions
options(repr.plot.width = 10, repr.plot.height = 6)

# Plot scatter plot
ggplot(site_040, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 040 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )


ggplot(site_063A, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 063A Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_063B, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 063B Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_066, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 066 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_079, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 079 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_085, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 085 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_086, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 086 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_099, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 099 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_103, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 103 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(site_107, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size="none")+
  labs(x = "Sum VOC", y = "TWA TVOC") +
  ggtitle("Site 107 Scatter Plot")+
  theme_minimal() +
  #facet_wrap(~site_id) +
  ylim(0,100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#all data into one plot
ggplot(all_data, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  labs(x = "Sum VOC", y = "TWA TVOC", color= "Type of Room") +
  ggtitle("Scatter Plot of All Sites")
  theme_minimal() +
  ylim(0,200) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Jade Correlations working 

# QTrak Data
data_qtrak_040 <- read_rds("data/data_qtrak.rds") %>%
  filter(var=="totalvoc low24_ppm") %>%
  filter(datetime >= "2023-10-24 11:11:00")
  #Data for site 40 
  #filter(datetime >= ymd_hms(datetime_start, tz = "US/Mountain")) %>%
  #filter(datetime < ymd_hms(datetime_end, tz = "US/Mountain")) %>%
  #left_join(inst_log$qtrak, by = "id_inst") %>%
  #select(-level, date_installed, date_uninstalled)

baseline <- data_qtrak_040 %>%
  mutate(min_time = min(datetime)) %>%
  #filter(datetime <= min_time + minutes(20)) %>%
  filter(grepl('totalvoc low24_ppm', var)) %>%
  summarise(baseline = median(val)*1000) %>%
  ungroup %>%
  mutate(avg_baseline = mean(baseline)) %>% group_by(id_inst) %>%
  mutate(baseline_fix = avg_baseline - baseline)

# Join the baseline data frame with the voc data frame
voc <- voc %>%
  left_join(baseline, by = "id_room")

# Subtract the baseline from the voc values
voc <- voc %>%
  mutate(voc_baseline_subtracted = voc + baseline_fix) 

# SITE 040 ROOM PAIR CORRELATIONS

#bears & frogs 
bears_data <- subset(site_040, room == "Bears")
frogs_data <- subset(site_040, room == "Frogs")

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






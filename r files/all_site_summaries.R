source("functions.R")
#load data
#sites <- read_excel("data/voc_samples_all.xlsx")

#mean conc. for all analytes across ALL indoor locations
indoor %>% 
  select(analyte, conc.) %>% 
  group_by(analyte) %>% 
  summarise(conc._mean = mean(conc.)) %>% 
  print(n = 61)

#mean conc. for all analytes across ALL outdoor locations
outdoor %>% 
  select(analyte, conc.) %>% 
  group_by(analyte) %>% 
  summarise(conc._mean = mean(conc.)) %>% 
  print(n = 61)

#data table
sites_table <- sites %>% 
  select(1:3,6,7,16)
datatable(sites_table, colnames = c("Site ID", "Name", "Location", "Analyte",
                              "Concentration", "Category"),
          options = list(pageLenght = 10), rownames = FALSE,
          caption = "All Sites Table, Concentrations: ppb(v) or methane ppb(v)")

#site voc concentration sums
#site 040
site_040_sum <- ggplot(site_040, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 040") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 063 A
site_063a_sum <- ggplot(site_063A, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063A") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 063 B
site_063b_sum <- ggplot(site_063B, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 063B") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#SITE 066
site_066_sum <- ggplot(site_066, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 066") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 079
site_079_sum <- ggplot(site_079, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 079") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 085
site_085_sum <- ggplot(site_085, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 085") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 086
site_086_sum <- ggplot(site_086, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 086") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 099
site_099_sum <- ggplot(site_099, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 099") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 103
site_103_sum <- ggplot(site_103, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 103") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

# SITE 107
site_107_sum <- ggplot(site_107, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 107") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

#sites 108
site_108_sum <- ggplot(site_108, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 108") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())
#site 094
site_094_sum <- ggplot(site_094, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle("Site 094") +
  labs(x = "", y = "")+
  theme_bw() +
  theme(axis.text.y = element_blank())

grid.arrange(site_094_sum, site_063a_sum, site_063b_sum, site_066_sum, 
             site_079_sum, site_085_sum, site_086_sum, site_099_sum,
             site_103_sum, site_107_sum, site_108_sum, site_094_sum,
             ncol = 3, nrow = 4,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#box plot for all indoor locations
bp_indoor <- ggplot(indoor, aes(x = reorder(analyte, conc.), 
                                y = conc., #fill = analyte
)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: All Indoor Locations in CO\n (n = 11)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration")
bp_indoor 

#site_040 boxplot
donald <- site_040 %>% 
  ggplot(aes(x = reorder(analyte, conc.), y = conc.)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration")
donald

#apartments
pc_apartments <- ggplot(apartments, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Semi-Perminant Living Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_apartments, tooltip = "text")

#temp living locations
pc_temp_living <- ggplot(temp_living, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Temporary Living Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_temp_living, tooltip = "text")

#classroom locations
pc_lassrooms <- ggplot(classrooms, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Classroom Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_classrooms, tooltip = "text")

#lobby locations
pc_lobbys <- ggplot(lobbys, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Lobby Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_lobbys, tooltip = "text")

#healthcare room locations
pc_medical <- ggplot(medical, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Healthcare Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_medical, tooltip = "text")

#recreation locations
pc_recreation <- ggplot(recreation, aes(x = reorder(analyte, conc.),
                                    y = conc., color = site_id,
                                    text = paste("Analyte: ", analyte,
                                                 "<br> Conc. :", conc.,
                                                 "<br> Class: ", category))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: Rec & Activity Locations in CO") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))
ggplotly(pc_recreation, tooltip = "text")

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


#Jade's analysis
# Plot scatter plots of Average TVOC Values Recorded 
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
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 063A Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )

ggplot(site_063B, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 063B Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_066, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 066 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_079, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 079 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_085, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 085 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_086, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 086 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_099, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 099 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_103, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 103 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_107, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 107 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )

# SITE 040 ROOM PAIR CORRELATIONS

# select bears & frogs rooms from site data sheeet 
bears_data <- subset(site_040, room == "Bears")
frogs_data <- subset(site_040, room == "Frogs")

# Select only the columns containing concentration data associated with each analyte 
bears_conc <- select(bears_data, conc., analyte)
frogs_conc <- select(frogs_data, conc., analyte)

# Select columns containing "conc." and ensure they are numeric
bears_numeric <- bears_conc %>%
  select(where(is.numeric))
frogs_numeric <- frogs_conc %>%
  select(where(is.numeric))

# Calculate correlations for each pair of analytes within Bears and Frogs
#Currently not calculating correctly, giving value of 1 for correlation. 
#Should I instead choose one analyte of interest, and gather correlations between rooms from there? 
bears_cor <- cor(bears_numeric, method = "spearman")
frogs_cor <- cor(frogs_numeric, method = "spearman")

print("Correlation matrix for Bears:")
print(bears_cor)
print("Correlation matrix for Frogs:")
print(frogs_cor)

#all possible room pairs from site 040
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


# Attempting to pull in qtrak data
path_data <- "/data_qtrak.rds"

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


#Ben's help
# Define the file path for the Excel file
file_path <- "./data/summa_date_time.xlsx"
base_path <- "T:/Projects/iaqv/data_rds"  # Base path for data files

# Read the Excel file into a dataframe
data <- read_excel(file_path)

# Convert to dataframe (although read_excel already returns a tibble, which is a type of dataframe)
df <- as.data.frame(data)

# Extract unique site_ids
site_ids <- unique(df$site_id)

# Function to read the data for each site_id
read_site_data <- function(site_id, base_path) {
  # Construct the paths for each file
  qtrak_path <- file.path(base_path, site_id, "data_qtrak.rds")
  instlog_path <- file.path(base_path, site_id, "data_instlog.rds")
  site_data_path <- file.path(base_path, site_id, "data_site.rds")
  
  # Initialize a list to store data for this site
  site_data <- list()
  
  # Read the data_qtrak.rds file if it exists
  if (file.exists(qtrak_path)) {
    site_data$data_qtrak <- readRDS(qtrak_path)
  } else {
    site_data$data_qtrak <- NULL
  }
  
  # Read the data_instlog.rds file if it exists
  if (file.exists(instlog_path)) {
    site_data$data_instlog <- readRDS(instlog_path)
  } else {
    site_data$data_instlog <- NULL
  }
  
  
  
  return(site_data)
}

# Loop through each site_id and read the data
site_data_list <- lapply(site_ids, read_site_data, base_path = base_path)



# Assign names to the list elements based on site_id
names(site_data_list) <- site_ids

# Display the site_data_list to verify contents
print(site_data_list)

# Initialize an empty vector to store unique 'var' values
unique_vars <- c()

# Loop through each site_id and extract unique 'var' values
for (site_id in names(site_data_list)) {
  site_data <- site_data_list[[site_id]]
  
  # Check if data_qtrak is not NULL
  if (!is.null(site_data$data_qtrak)) {
    # Extract unique 'var' values and combine them with the existing ones
    unique_vars <- unique(c(unique_vars, site_data$data_qtrak$var))
  }
}

# Display the unique 'var' values
print(unique_vars)

# List of VOC-related variables with ppm or equivalent units
voc_ppm_vars <- c(
  "voclow1_ppm", "voclow12_ppm", "totalvoc low24_ppm",
  "voclow4_ppb", "voclow12_ppb"
)

# Function to filter data_qtrak by VOC-related variables with ppm or equivalent units
filter_voc_ppm <- function(data_qtrak) {
  # Check if data_qtrak is not NULL
  if (!is.null(data_qtrak)) {
    # Filter the data_qtrak dataframe
    filtered_data <- data_qtrak %>%
      filter(var %in% voc_ppm_vars)
    return(filtered_data)
  } else {
    return(NULL)
  }
}

# Assuming site_data_list is already populated
# Loop through each site_id and filter data_qtrak
filtered_site_data <- lapply(site_data_list, function(site_data) {
  site_data$data_qtrak <- filter_voc_ppm(site_data$data_qtrak)
  return(site_data)
})

# Function to extract and transform data from each site
# Function to extract and transform data from each site
extract_data <- function(site_data, site_id) {
  excluded_sites <- c("107", "089")  # List of site_ids to exclude
  if (!(site_id %in% excluded_sites)) {  # Ignore site_ids in the list
    if (!is.null(site_data$data_qtrak) && nrow(site_data$data_qtrak) > 0) {
      data <- site_data$data_qtrak
      if ("room" %in% colnames(data)) {
        data <- data %>%
          mutate(site_id = site_id) %>%  # Include site_id
          select(datetime, site_id, id_inst, var, val, room)
      } else {
        data <- data %>%
          mutate(site_id = site_id) %>%  # Include site_id
          select(datetime, site_id, id_inst, var, val) %>%
          mutate(room = NA)
      }
      return(data)
    }
  }
  return(NULL)
}

# Use map2 to iterate over names and elements of the list and apply the function
all_data <- map2_df(filtered_site_data, names(filtered_site_data), extract_data)

# Convert dates and times to POSIXct
df <- df %>%
  mutate(
    start_datetime = as.POSIXct(paste(start_date, format(start_time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
    end_datetime = as.POSIXct(paste(end_date, format(end_time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S")
  )

# Convert datetime columns to POSIXct if they are not already
all_data$datetime <- as.POSIXct(all_data$datetime, format = "%Y-%m-%d %H:%M:%S")
df$start_datetime <- as.POSIXct(df$start_datetime, format = "%Y-%m-%d %H:%M:%S")
df$end_datetime <- as.POSIXct(df$end_datetime, format = "%Y-%m-%d %H:%M:%S")

# Perform the join and filter based on datetime range
filtered_data <- all_data %>%
  left_join(df, by = c("site_id" = "site_id", "room" = "room")) %>%
  filter(datetime >= start_datetime & datetime <= end_datetime)

# Convert filtered_site_data$`079`$data_qtrak to a dataframe and get the first 5 rows for each id_inst
check <- filtered_data %>%
  group_by(site_id, room) %>%
  slice_head(n = 1)

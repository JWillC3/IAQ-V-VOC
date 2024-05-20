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
bp_indoor <- ggplot(indoor, aes(x = analyte, y = conc., #fill = analyte
)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: All Indoor Locations in CO\n (n = 11)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration")
bp_indoor 

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











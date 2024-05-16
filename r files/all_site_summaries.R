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

#box plot
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
# + scale_fill_manual(values = c("#069668", "#003c70", "#b38677", "#4ba40b",
#                                "#e16d70", "#6599a2", "#ea6b12", "#5b1915",
#                                "#1f84ec", "#042c17", "#fe2b1c", "#ab7b05"))


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












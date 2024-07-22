#packages
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "Teaching Tree (Post)" and Fort Collins"
#change [location info] for where the canisters were located
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"
#make sure to slice and select the correct rows! Will not be the same for all sites
#load data


#calculate ratios
indoor_040 <- indoor_040 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_040$conc./outdoor_040$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON Location 1, 2, etc.!
#location order should be alphabetical

#bears
bears <- site_040 %>% 
  filter(room_name == "Bears")
#frogs
frogs <- site_040 %>% 
  filter(room_name == "Frogs")
#lesson prep
lesson_prep <- site_040 %>% 
  filter(room_name == "Lesson Prep")
#monkeys
monkeys <- site_040 %>% 
  filter(room_name == "Monkeys")
#office
office <- site_040 %>% 
  filter(room_name == "Office")

#data table

site_040_table <- sites %>% 
  select(4,7,9,18)

datatable(site_040_table, colnames = c("Location", "Analyte", "Concentration",
                                    "Category"),
          options = list(pageLenght = 10), rownames = FALSE,
          caption = "Site 040 Table, Concentrations: ppb(v) or methane ppb(v)")


#boxplot for all analytes

bp_040 <- site_040 %>% 
  ggplot(aes(x = reorder(analyte, conc.), y = conc.)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  ggtitle("Boxplot for All Ananlytes")
bp_040


#plot all VOCs at site
p_040 <- ggplot(site_040, aes(x = reorder(analyte, conc.),
                            y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes", y = "Concentration\n(VOC ppbv or methane ppmv)",
       title = "Site 040 - Teaching Tree (Post), Summa Cannister Deployment\nOct. 24 - Oct. 31, 2023. Fort Collins, CO",
       caption = "6 canisters deployed throughout site\n5 indoor: 4 first floor, 1 second floor\n1 outdoor, roof top")
  
p_040 +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  theme(legend.title = element_text(color = "black", face = "bold", size = 10),
        legend.text = element_text(color = "#261e1b"),
        legend.background = element_rect(color = "black"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13))
        #panel.grid.minor = element_line("black""))
        #panel.grid.major = element_line("black"),
        #plot.background = element_rect("#DADBDD"))


#plotly output but y axis not formatted correctly and don't know how to fix
#WHEN ADDING THIS TO RMD MAKE SURE TO CHANGE THE DF TO INLCUDE LONG CAT NAMES
p_040_2 <- ggplot(site_040, aes(x = reorder(analyte, conc.),
                              y = conc., color = room_name,
                              text = paste("Analyte: ", analyte,
                                           "<br> Conc. :", conc.,
                                           "<br> Class: ", category))) +
  geom_point(shape = 18, size = 4, alpha = 0.5) +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() + #try using different themes here
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment, Oct. 24 - 31, 2023")

ggplotly(p_040_2, tooltip = "text")

#top 10 analyte concentrations for all locations
site_040_top <- site_040 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
site_040_top <- top_n(ungroup(site_040_top), 25, conc.)

site_040_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "chocolate4") +
  labs(x = "Analyte", y = "Concentration") +
  ggtitle("Site 040 (post) Top 10 Analytes") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 

#facet wrap by location
p_040_fctw <- ggplot(site_040, aes(x = reorder(analyte, conc.),
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
            "Grouped by Cannister location")
p_040_fctw

#leave this out for now
#Two plots together not joined
p_040 +
  p_040_fctw +
  plot_layout(nrow = 2, heights = c(1, 2))

#facet wrap all analytes grouped by class

p_040_cat_fctw <- ggplot(site_040, aes(x = reorder(analyte, conc.),
                                   y = conc.)) +
  geom_point(color = "orange", size = 3, shape = 18, alpha = 0.5) +
  xlab("Analytes") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~ category, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
          "Grouped by Analyte Category")
p_040_cat_fctw

#indoor facet wrap by analyte class

p_040_cat_fctw <- ggplot(indoor_040, aes(x = reorder(analyte, conc.),
                                       y = conc.)) +
  geom_point(color = "#50C878", size = 3, shape = 18, alpha = 0.5) +
  xlab("Analytes") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~ category, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Indoor Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
          "Grouped by Analyte Category")
p_040_cat_fctw

#plots for each room
#outdoor
p_040od <- outdoor_040 %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "midnightblue", shape = 18, size = 5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Outdoor (Rooftop)")
p_040od

#bears
p_bears <- bears %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "orchid", shape = 18, size = 5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Bears",
          "first floor classroom")
p_bears

#frogs
p_frogs <- frogs %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "chocolate4", shape = 18, size = 5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Frogs",
          "first floor classroom")
p_frogs

#lesson_prep
p_lesson_prep <- lesson_prep %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "goldenrod2", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6)) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Lesson Prep",
          "second floor office")
p_lesson_prep

#monkeys
p_monkeys <- monkeys %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "#50C878", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6, color ="red")) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 040 - Teaching Tree (Post), Monkeys",
          "first floor classroom")
p_monkeys

#office
p_office <- office %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(color = "tomato2", shape = 18, size = 5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("040 - Site Teaching Tree (Post), Office",
          "first floor office")
p_office

#all rooms

grid.arrange(p_bears, p_frogs, p_monkeys, p_lesson_prep, p_office, p_040od, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#bears
#testing something new for scale y in this plot
bears_top <- bears %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
bears_top <- top_n(ungroup(bears_top), 5, conc.) 

p_bears_top <- bears_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "orchid") +
  labs(x = "Bears", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_bears_top

#frogs
frogs_top <- frogs %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
frogs_top <- top_n(ungroup(frogs_top), 5, conc.)

p_frogs_top <- frogs_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "chocolate4") +
  labs(x = "Frogs", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_frogs_top

#lesson_prep
lesson_prep_top <- lesson_prep %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
lesson_prep_top <- top_n(ungroup(lesson_prep_top), 5, conc.)

p_lesson_prep_top <- lesson_prep_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "goldenrod2") +
  labs(x = "Lesson Prep", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_lesson_prep_top

#monkeys
monkeys_top <- monkeys %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
monkeys_top <- top_n(ungroup(monkeys_top), 5, conc.)

p_monkeys_top <- monkeys_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "#50C878") +
  labs(x = "Monkeys", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_monkeys_top

#office
office_top <- office %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
office_top <- top_n(ungroup(office_top), 6, conc.)

p_office_top <- office_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "tomato2") +
  labs(x = "Office", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_office_top

#outdoor
od_top <- outdoor %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
od_top <- top_n(ungroup(od_top), 10, conc.)

p_od_top <- od_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "midnightblue") +
  labs(x = "Outdoor", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_od_top

#all locations
grid.arrange(p_bears_top, p_frogs_top, p_lesson_prep_top,
             p_monkeys_top, p_office_top, p_od_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_040_bp <- indoor_040 %>% 
  ggplot(aes(x = reorder(analyte, od_ratio), y = od_ratio)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration") +
  ggtitle("Boxplot for All Indoor to Outoodr Ratios")

p_040_bp

#indoor to outdoor ratio plot for all analytes
p_040_ratio <- ggplot(indoor_040, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios\n Concentration Ratios") +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
          "Oct. 24 - Oct. 31, 2023. Fort Collins, CO")
p_040_ratio +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  theme(legend.title = element_text(color = "black", face = "bold", size = 10),
        legend.text = element_text(color = "#261e1b"),
        legend.background = element_rect(color = "black"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13))
      #panel.grid.minor = element_line("black""))#,
      #panel.grid.major = element_line("black"),
      #plot.background = element_rect("#DADBDD"))

#plotly ratio
p_040_r <- ggplot(indoor_040, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = room_name,
                              text = paste("Analyte: ", analyte,
                                           "<br> Conc. :", od_ratio,
                                           "<br> Class: ", category))) +
  geom_point(shape = 18, size = 4, alpha = 0.5) +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() + #try using different themes here
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios\n Concentration Ratios") +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment: Oct. 24 - Oct. 31, 2023. Fort Collins, CO")

ggplotly(p_040_r, tooltip = "text")

#ratio plot with facet wrap

p_040_r_fctw <- ggplot(indoor_040, aes(x = reorder(analyte, od_ratio),
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
          "Grouped by Analyte Category")
p_040_r_fctw

#create object of the top 5 analytes by outdoor ratio then plot
#bears od ratio
bears_top_or <- indoor_040 %>% 
  filter(room_name == "Bears") %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
bears_top_or <- top_n(ungroup(bears_top_or), 5, od_ratio) 

p_bears_top_or <- bears_top_or %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "orchid") +
  labs(x = "Bears", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_bears_top_or

#frogs od ratio
frogs_top_or <- indoor_040 %>% 
  filter(room_name == "Frogs") %>%
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
frogs_top_or <- top_n(ungroup(frogs_top_or), 5, od_ratio)

p_frogs_top_or <- frogs_top_or %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "chocolate4") +
  labs(x = "Frogs", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_frogs_top_or

#lesson_prep od ratio
lesson_prep_top_or <- indoor_040 %>% 
  filter(room_name == "Lesson Prep") %>%
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
lesson_prep_top_or <- top_n(ungroup(lesson_prep_top_or), 5, od_ratio)

p_lesson_prep_top_or <- lesson_prep_top_or %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "goldenrod2") +
  labs(x = "Lesson Prep", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_lesson_prep_top_or

#monkeys od ratio
monkeys_top_or <- indoor_040 %>% 
  filter(room_name == "Monkeys") %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
monkeys_top_or <- top_n(ungroup(monkeys_top_or), 5, od_ratio)

p_monkeys_top_or <- monkeys_top_or %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "#50C878") +
  labs(x = "Monkeys", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_monkeys_top_or

#office od ratio
office_top_or <- indoor_040 %>% 
  filter(room_name == "Office") %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
office_top_or <- top_n(ungroup(office_top_or), 6, od_ratio)

p_office_top_or <- office_top_or %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "tomato2") +
  labs(x = "Office", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_office_top_or

#all locations od ratio
grid.arrange(p_bears_top_or, p_frogs_top_or, p_lesson_prep_top_or,
             p_monkeys_top_or, p_office_top_or,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),
                                 y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Alcohols") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),
                                   y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Aldehydes") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),
                                               y = conc., color = room_name)) +
  geom_point(shape = 18, size = 4, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Straight Chains") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),
                                   y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Aromatics") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  theme(legend.title = element_text(color = "black", face = "bold", size = 10),
        legend.text = element_text(color = "#261e1b"),
        legend.background = element_rect(color = "black"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13))

#btex
p_btex <- btex %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),
                           y = conc., color = room_name)) +
  geom_point(shape = 18, size = 4, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Btex") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))


#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Chlorinated") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))


#ketone
p_ketone <- ketone %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.),y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Ketones") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))


#others
p_other <- other %>% 
  filter(site_id == "040") %>% 
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = room_name)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Other") +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  theme(legend.title = element_text(color = "black", face = "bold", size = 10),
        legend.text = element_text(color = "#261e1b"),
        legend.background = element_rect(color = "black"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13))


#categories plotted together
grid.arrange(p_alcohol, p_aldehyde, p_straight_chain, p_aromatic,
             ncol = 2, nrow = 2,
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")

grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")


# #correlations
cor(frogs$conc., bears$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_040 %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(office$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_bears <- p.analytes %>%
  filter(ID == "Bears")
sum(tvoc_bears$conc.)
tvoc_frogs <- p.analytes %>% 
  filter(ID == "Frogs")
sum(tvoc_frogs$conc., na.rm = TRUE)
tvoc_lessonprep <- p.analytes %>% 
  filter(ID == "Lesson Prep")
sum(tvoc_lessonprep$conc., na.rm = TRUE)  
tvoc_monkeys <- p.analytes %>% 
  filter(ID == "Monkeys")
sum(tvoc_monkeys$conc., na.rm = TRUE)
tvoc_office <- p.analytes %>% 
  filter(ID == "Office")
sum(tvoc_office$conc., na.rm = TRUE)
tvoc_outdoor <- p.analytes %>% 
  filter(ID == "Outdoor")
sum(tvoc_outdoor$conc., na.rm = TRUE)


#convert select rows to numeric
voc_cor[2:62] <- lapply(voc_cor[2:62], as.numeric)
#run the correlation
vcor <- cor(voc_cor[, unlist(lapply(voc_cor, is.numeric))], method = "spearman")
#create the matrix
#if working with smaller set can move values inside of matrix using:
#addCoef.col = "black"
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.75)
#lower
corrplot(vcor, method = "color", type = "lower")
#upper w/ diag lables
corrplot(vcor, method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.75)
#red theme
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.5,
         col=colorRampPalette(c("blue","white","red"))(200))
#another method for correlation
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)
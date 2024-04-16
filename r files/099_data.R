#packages
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

#replace the site number and name first
#change the dates for plots "Sep. 20 - Sep. 27, 2023."
#change the site location "[Name]" to "Rocky Ford"
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"
#make sure to slice and select the correct rows! Will not be the same for all sites
#load data
site_099 <- (read_csv(file = "./data/site_099_summa_data.csv")) %>% 
  slice(2:7) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_099 %>% 
  pivot_longer(- ID,
               names_to = "analyte",
               values_to = "conc.")
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
#new 'voc' object becomes the new df for plots
#add voc categories to df
voc <- p.analytes %>% 
  mutate(category = case_when(p.analytes$analyte %in% alc ~ "Alc.",
                              p.analytes$analyte %in% ald ~ "Ald.",
                              p.analytes$analyte %in% stc ~ "Stc.",
                              p.analytes$analyte %in% aro ~ "Aro.",
                              p.analytes$analyte %in% btx ~ "Btx.",
                              p.analytes$analyte %in% chl ~ "Chl.",
                              p.analytes$analyte %in% kt ~ "Ktn.",
                              p.analytes$analyte %in% oth ~ "Oth."))

#create outdoor group
outdoor <- voc %>% 
  filter(ID =="Outdoor")
#change conc. to numeric
voc$conc. <- as.numeric(as.character(voc$conc.))

outdoor$conc. <- as.numeric(as.character(outdoor$conc.))

#calculate ratios
voc <- voc %>%
  group_by(ID, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = round(as.numeric(voc$conc.)/as.numeric(outdoor$conc.), 2))
#highlight methane
methane <- voc %>% 
  filter(analyte == "methane")

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON Location 1, 2, etc.!
#location order should be alphabetical
#subgroups,
#indoor
indoor <- voc %>% 
  filter(ID != "Outdoor")
#exam1
exam1 <- voc %>% 
  filter(ID == "Exam 1")
#exam2
exam2 <- voc %>% 
  filter(ID == "Exam 2")
#lobby
lobby <- voc %>% 
  filter(ID == "Lobby")
#nurse_station
nurse_station <- voc %>% 
  filter(ID == "Nurse Station")
#office
office <- voc %>% 
  filter(ID == "Office")

#data table
#for data table
voc2 <- p.analytes %>% 
  mutate(category = case_when(p.analytes$analyte %in% alc ~ "Alcohol",
                              p.analytes$analyte %in% ald ~ "Aldehyde",
                              p.analytes$analyte %in% stc ~ "Straight Chain",
                              p.analytes$analyte %in% aro ~ "Aromatic",
                              p.analytes$analyte %in% btx ~ "Btex",
                              p.analytes$analyte %in% chl ~ "Chlorinated",
                              p.analytes$analyte %in% kt ~ "Ketone",
                              p.analytes$analyte %in% oth ~ "Other"))
voc2 <- voc2 %>%
  group_by(ID, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = round(as.numeric(voc$conc.)/as.numeric(outdoor$conc.), 2))

datatable(voc2, colnames = c("Location", "Analyte", "Concentration",
                             "Category", "Outdoor Ratio"),
          options = list(pageLength = 10), rownames = FALSE)

voc2$conc. <- as.numeric(as.character(voc2$conc.))

indoor2 <- voc2 %>% 
  filter(ID != "Outdoor")

#create category objects
#Alcohol
calcohol <- voc2 %>%
  filter(category == "Alcohol")
#Aldehyde
aldehyde <- voc2 %>% 
  filter(category == "Aldehyde")
#Straight Chain
straight_chain <- voc2 %>% 
  filter(category == "Straight Chain")
#Aromatic
aromatic <- voc2 %>% 
  filter(category == "Aromatic")
#Btex
btex <- voc2 %>% 
  filter(category == "Btex")
#Chlorinated
chlorinated <- voc2 %>% 
  filter(category == "Chlorinated")
#Ketone
ketone <- voc2 %>% 
  filter(category == "Ketone")
#Other
other <- voc2 %>% 
  filter(category == "Other")


#plot all VOCs at site
voc_plot <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes", y = "Concentration\n(VOC ppbv or methane ppmv)",
       title = "Site 099 - Radiant, Summa Cannister Deployment\nSep. 20 - Sep. 27, 2023. Rocky Ford, CO",
       caption = "6 canisters deployed throughout site\n5 indoor, first floor\n1 outdoor, rooftop")
  
voc_plot +
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
        #panel.grid.minor = element_line("maroon")),
        #panel.grid.major = element_line("black"),
        #plot.background = element_rect("#DADBDD"))


#plotly output but y axis not formatted correctly and don't know how to fix
#WHEN ADDING THIS TO RMD MAKE SURE TO CHANGE THE DF TO INLCUDE LONG CAT NAMES
voc_plot2 <- ggplot(voc2, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID,
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
  ggtitle("Site 099 - Radiant, Summa Cannister Deployment. Sep 20 - 27, 2023")
ggplotly(voc_plot2, tooltip = "text")


#combo__plot 
# add after 'group = ID' if want the line thicker" , linewidth = 1, alpha = 0.5
# voc_combo_col <- ggplot(indoor, aes(fill = ID, x = analyte, y = conc.)) +
#   geom_col(position = position_dodge(1.5)) +
#   geom_line(data = outdoor, aes(x = analyte, y =  conc.,
#                                 group = ID)) +
#   #guides(linewidth = "none", alpha = "none") +
#   scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# voc_combo_col
#combo point plot

# voc_combo_point <- ggplot(indoor, aes(color = ID, x = analyte, y = conc.)) +
#   geom_point(data = indoor, aes(x = analyte, y = conc.),
#              shape = 18, alpha = 0.5) +
#   geom_line(data = outdoor, aes(x = analyte, y =  conc.,
#                                 group = ID, alpha = 0.5)) +
#   guides(alpha = "none") +
#   xlab("Analytes") +
#   ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
#   scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
#   ggtitle("Site 099 Summa Cannister Deployment",
#           "Sep. 20 - Sep. 27, 2023. Rocky Ford, CO")
# voc_combo_point

#facet wrap by location
loc_fctw <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc.)) +
  geom_point(color = "#50C878", size = 5, shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.,
                                 color ="red", size  = 5)) +
  guides(size = "none", color = "none") +
  xlab("Analytes") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_wrap(~ID, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099 - Radiant, Summa Cannister Deployment",
            "Grouped by Cannister location")
loc_fctw

#Two plots together not joined
voc_plot +
  loc_fctw +
  plot_layout(nrow = 2, heights = c(1, 2))


#facet grid all analytes grouped by class
voc_fctg <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(data = voc, aes(x = reorder(analyte, conc.), y = conc.),
             size = 4, shape = 18, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped by VOC Categories",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099: Radiant, Summa Cannister Deployment",
          "Grouped by Analyte Class")
voc_fctg +
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

#facet grid indoor analytes grouped by class
indoor_fctg <- ggplot(indoor, aes(x = reorder(analyte, conc.),
                               y = conc., color = ID)) +
  geom_point(data = indoor, aes(x = reorder(analyte, conc.), y = conc.),
             size = 5, shape = 18, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped by VOC Categories",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099: Radiant, Indoor Summa Cannister Deployment",
          "Grouped by Analyte Class")
indoor_fctg +
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

#plots for each room
#outdoor
od_plot <- outdoor %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "midnightblue", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6, color ="red")) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("099 - Radiant: Outdoor (Rooftop)")
od_plot

#exam1
l1_plot <- exam1 %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "orchid", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6)) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("099 - Radiant: Exam Room 1")
l1_plot

#exam2
l2_plot <- exam2 %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "chocolate4", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6)) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("099 - Radiant: Exam Room 2")
l2_plot

#lobby
l3_plot <- lobby %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
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
  ggtitle("099 - Radiant: Lobby")
l3_plot

#nurse_station
l4_plot <- nurse_station %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
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
  ggtitle("099 Radiant: Nurse's Station")
l4_plot

#office
l5_plot <- office %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "tomato2", shape = 18, size = 5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                size = 6)) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("099 - Radiant: Heather's Office")
l5_plot

#indoor, outdoor ratio 
#without facet
ratio_plot <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes", y = "Indoor/Outdoor Ratios\n(VOC ppbv or methane ppmv)",
       title = "Site 099 - Radiant, Summa Cannister Deployment\nSep. 20 - Sep. 27, 2023. Rocky Ford, CO",
       caption = "6 canisters deployed throughout site\n5 indoor, first floor\n1 outdoor, rooftop")
ratio_plot +
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
      #panel.grid.minor = element_line("maroon"))#,
      #panel.grid.major = element_line("black"),
      #plot.background = element_rect("#DADBDD"))

#plotly ratio
ratio_plotly <- ggplot(indoor2, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID,
                              text = paste("Analyte: ", analyte,
                                           "<br> Conc. :", od_ratio,
                                           "<br> Class: ", category))) +
  geom_point(shape = 18, size = 4, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() + #try using different themes here
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue")) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor\n Concentration Ratios") +
  ggtitle("Site 099 - Radiant, Summa Cannister Deployment")
ggplotly(ratio_plotly, tooltip = "text")

#ratio plot with facet grid
fctg_ratio_plot <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                  y = od_ratio, color = ID)) +
  geom_point(size = 5, shape = 18, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped by VOC Categories",
       y = expression(atop("Outdoor Ratio Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099 - Radiant, Indoor Summa Cannister Deployment",
          "Grouped by Analyte Class")
fctg_ratio_plot +
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
      #panel.grid.minor = element_line("maroon"))#,
      #panel.grid.major = element_line("black"),
      #plot.background = element_rect("#DADBDD"))

#category plots
p_alcohol <- ggplot(alcohol, aes(x = reorder(analyte, conc.),
                                 y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Alcohols")
p_alcohol +
  scale_color_manual(name = "Room ID",
                    values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                               "tomato2", "midnightblue"))

p_aldehyde <- ggplot(aldehyde, aes(x = reorder(analyte, conc.),
                                 y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Aldehydes")
p_aldehyde +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

p_straight_chain <- ggplot(straight_chain, aes(x = reorder(analyte, conc.),
                                        y = conc., color = ID)) +
  geom_point(shape = 18, size = 4, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Straight Chains")
p_straight_chain +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

p_aromatic <- ggplot(aromatic, aes(x = reorder(analyte, conc.),
                                   y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Aromatics")
p_aromatic +
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

p_btex <- ggplot(btex, aes(x = reorder(analyte, conc.),
                               y = conc., color = ID)) +
  geom_point(shape = 18, size = 4, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Btex")
p_btex +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

p_chlorinated <- ggplot(chlorinated, aes(x = reorder(analyte, conc.),
                                      y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Chlorinated")
p_chlorinated +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

p_ketone <- ggplot(ketone, aes(x = reorder(analyte, conc.),
                                 y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x = "", y = "") +
  ggtitle("Ketones")
p_ketone +
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"))

p_other <- ggplot(other, aes(x = reorder(analyte, conc.),
                                y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "", y = "") +
  ggtitle("Other")
p_other +
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
             top = "Analytes Grouped by Category",
             left = "Concentraion\n (VOC ppbv or methane ppmv")
grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "Analytes Grouped by Category",
             left = "Concentraion\n (VOC ppbv or methane ppmv")

#create object of the top 5 analyte conc. for the indoor locations then plot
#exam1
#testing something new for scale y in this plot
exam1_top <- exam1 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
exam1_top <- top_n(ungroup(exam1_top), 5, conc.) 

p_exam1_top <- exam1_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "orchid") +
  labs(x = "Exam Rm. 1", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_exam1_top

#exam2
exam2_top <- exam2 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
exam2_top <- top_n(ungroup(exam2_top), 5, conc.)

p_exam2_top <- exam2_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "chocolate4") +
  labs(x = "Exam Rm. 2", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_exam2_top

#lobby
lobby_top <- lobby %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
lobby_top <- top_n(ungroup(lobby_top), 6, conc.)

p_lobby_top <- lobby_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "goldenrod2") +
  labs(x = "Lobby", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_lobby_top

#nurse_station
nurse_station_top <- nurse_station %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
nurse_station_top <- top_n(ungroup(nurse_station_top), 5, conc.)

p_nurse_station_top <- nurse_station_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "#50C878") +
  labs(x = "Nurse's Station", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_nurse_station_top

#office
office_top <- office %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
office_top <- top_n(ungroup(office_top), 5, conc.)

p_office_top <- office_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "tomato2") +
  labs(x = "Heather's Office", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size  =13, angle = 45, hjust = 1)) 
p_office_top

#outdoor
od_top <- outdoor %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
od_top <- top_n(ungroup(od_top), 5, conc.)

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
grid.arrange(p_exam1_top, p_exam2_top, p_lobby_top,
             p_nurse_station_top, p_office_top, p_od_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
                left = "Concentration\n(VOC ppbv or methane ppmv)")

#correlations
cor(outdoor$conc., exam1$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_099 %>% 
  slice(-2)
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
#DID NOT WORK FOR THIS SITE
library(corrr)
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)


#use this plot theme!
voc_plot_min <- ggplot(voc, aes(x = reorder(analyte, conc.),
                                y = conc., color = ID)) +
  geom_point(shape = 18, size = 8, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.position = c(0.1,0.78)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099 Summa Cannister Deployment",
          "Sep. 20 - Sep. 27, 2023. Rocky Ford, CO")
voc_plot_min + 
  scale_color_manual(name = "Room ID",
                     values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                                "tomato2", "midnightblue"),
                                     labels = c("Exam 1", "Exam 2",
                                                "Lobby",
                                                "Nurse Station",
                                                "Office", "Outdoor")) +
  theme(legend.title = element_text(color = "black", face = "bold", size = 10),
        legend.text = element_text(color = "#261e1b"),
        legend.background = element_rect(fill = "#DADBDD", color = "maroon"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 13),
        panel.grid.minor = element_line("maroon"),
        #panel.grid.major = element_line("black"),
        plot.background = element_rect("#DADBDD"))


#theme minimal
voc_plot_min <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 8, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 099 Summa Cannister Deployment",
          "Sep. 20 - Sep. 27, 2023. Rocky Ford, CO")
voc_plot_min + 
  scale_color_brewer(palette = "Dark2", name = "Room ID",
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6"))

#let's try a custom theme from the theme_minimal
library(magick)
logo <- image_read("./graphics/csu_cee_logo.png")

theme_voc <- function() {
  font <- "Georgia"
  
  theme_minimal() %+replace%
    
    theme(
      
      panel.grid.minor = element_line(size = 0.5, linetype = 2),
      
      plot.title = element_text(family = font, size = 20, face = "bold",
                                hjust = 0, vjust = 2),
      
      plot.subtitle = element_text(family = font, size = 14),
      
      plot.caption = element_text(family = font, size = 9, hjust = 1),
      
      axis.line = element_line(color = "black"),
      
      axis.title = element_text(family = font, size = 10),
      
      axis.text = element_text(family = font),
      
      axis.text.x = element_text(size = 10, angle = 45),
      
      axis.text.y = element_text(size = 13)
      
    )
}

donald +theme_voc()

donald <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Analytes", y = "Concentration\n(VOC ppbv or methane ppmv)",
       title = "Site 099 - Radiant, Summa Cannister Deployment\nSep. 20 - Sep. 27, 2023. Rocky Ford, CO",
       caption = "6 canisters deployed throughout site\n1 outdoor\n5 indoor") +
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
  

donald + theme_voc()
grid::grid.raster(logo, x = 0.09, y = 0, just = c('left', 'bottom'), width = unit(2, 'inches'))

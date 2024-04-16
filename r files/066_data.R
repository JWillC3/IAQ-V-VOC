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
#change the dates for plots "Mmm. dd - Mmm. dd, YYYY."
#change the site location "Homeward Bound" and Grand Junction"
#change [location info] for where the canisters were located
#"Location X]" for all indoor locations
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"
#make sure to slice and select the correct rows! Will not be the same for all sites
#load data
site_066 <- (read_csv(file = "../data/site_066_summa_data.csv")) %>% 
  slice(2:5, 7:8) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_066 %>% 
  pivot_longer(- ID,
               names_to = "analyte",
               values_to = "conc.")

p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

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

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON Location 1, 2, etc.!
#location order should be alphabetical
#subgroups,
#location 1
barracks_n <- voc %>% 
  filter(ID == "Barracks North")
#location 2
barracks_s <- voc %>% 
  filter(ID == "Barracks South")
#location 3
dining <- voc %>% 
  filter(ID == "Dining")
#location 4
office <- voc %>% 
  filter(ID == "Office")
#location 5
outdoor <- voc %>% 
  filter(ID == "Outdoor")
#location 6
welcome <- voc %>% 
  filter(ID == "Welcome Room")

# #indoor group
indoor <- voc %>%
  filter(ID != "Outdoor")

#highlight methane
methane <- voc %>% 
  filter(analyte == "methane")

#calculate ratios
voc <- voc %>%
  group_by(ID, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = round(as.numeric(voc$conc.)/as.numeric(outdoor$conc.), 2))

#data table
#for data table
voc2 <- voc %>% 
  mutate(category = case_when(p.analytes$analyte %in% alc ~ "Alcohol",
                              p.analytes$analyte %in% ald ~ "Aldehyde",
                              p.analytes$analyte %in% stc ~ "Straight Chain",
                              p.analytes$analyte %in% aro ~ "Aromatic",
                              p.analytes$analyte %in% btx ~ "Btex",
                              p.analytes$analyte %in% chl ~ "Chlorinated",
                              p.analytes$analyte %in% kt ~ "Ketone",
                              p.analytes$analyte %in% oth ~ "Other"))

datatable(voc2, colnames = c("Location", "Analyte", "Concentration",
                             "Category", "Outdoor Ratio"),
          options = list(pageLength = 10), rownames = FALSE)

#indoor group for ratio plots
indoor <- voc %>% 
  filter(ID != "Outdoor")
#indoor group for plotly ratio plots
indoor2 <- voc2 %>% 
  filter(ID != "Outdoor")

#create category objects
#Alcohol
alcohol <- voc2 %>%
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
       title = "Site 066 - Homeward Bound, Summa Cannister Deployment\nMmm. dd - Mmm. dd, YYYY. Grand Junction, CO",
       caption = "X canisters deployed throughout site\nX indoor, [location info]\nX outdoor, [location info]")
  
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
        #panel.grid.minor = element_line("maroon"))
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
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment, [Add date]")
ggplotly(voc_plot2, tooltip = "text")


#combo__plot (not working ATM)
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
#   ggtitle("Site 066 Summa Cannister Deployment",
#           "Mmm. dd - Mmm. dd, YYYY. City, CO")
# voc_combo_point

#facet wrap by location
loc_fctw <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc.)) +
  geom_point(color = "#50C878", size = 3, shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.,
                                 color ="red", size = 5)) +
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
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment",
            "Grouped by Cannister location")
loc_fctw

#leave this out for now
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
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment",
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

#indoor facet grid by analyte class
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
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment",
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
#barracks_n
l1_plot <- barracks_n %>%
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
  ggtitle("Site 066 - Homeward Bound, Barracks North",
          "[additional info if needed]")
l1_plot

#barracks_s
l2_plot <- barracks_s %>%
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
  ggtitle("Site 066 - Homeward Bound, Barracks South",
          "[additional info if needed]")
l2_plot

#dining
l3_plot <- dining %>%
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
  ggtitle("Site 066 - Homeward Bound, Dining",
          "[additional info if needed]")
l3_plot

#office
l4_plot <- office %>%
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
  ggtitle("Site 066 - Homeward Bound, Office",
          "[additional info if needed]")
l4_plot

#outdoor
l5_plot <- outdoor %>%
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
  ggtitle("066 - Site Homeward Bound, Outdoor",
          "[additional info if needed]")
l5_plot

#welcome
l6_plot <- welcome %>%
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
  ggtitle("Site 066 - Homeward Bound, Welcome Room ([location of can])")
l6_plot

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
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios\n Concentration Ratios") +
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. Grand Junction, CO")
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
  ylab("Indoor to Outdoor Ratios\n Concentration Ratios") +
  ggtitle("Site 066 - Homeward Bound, Summa Cannister Deployment, [Add date]")
ggplotly(ratio_plotly, tooltip = "text")

#ratio plot with facet grid
fctg_ratio_plot <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                      y = od_ratio, color = ID)) +
  geom_point(size = 5, shape = 18, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped by VOC Categories",
       y = expression(atop("Outdoor Ratio Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 066 - Homeward Bound, Indoor Summa Cannister Deployment",
          "Grouped by Analyte Class")
fctg_ratio_plot

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
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")

grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")

#create object of the top 5 analytes by conc. for the indoor locations then plot
#location 1
#testing something new for scale y in this plot
barracks_n_top <- barracks_n %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
barracks_n_top <- top_n(ungroup(barracks_n_top), 5, conc.) 

p_barracks_n_top <- barracks_n_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "orchid") +
  labs(x = "[Location1]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_barracks_n_top

#barracks_s
barracks_s_top <- barracks_s %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
barracks_s_top <- top_n(ungroup(barracks_s_top), 5, conc.)

p_barracks_s_top <- barracks_s_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "chocolate4") +
  labs(x = "[Location2]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_barracks_s_top

#dining
dining_top <- dining %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
dining_top <- top_n(ungroup(dining_top), 6, conc.)

p_dining_top <- dining_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "goldenrod2") +
  labs(x = "[Location3]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_dining_top

#office
office_top <- office %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
office_top <- top_n(ungroup(office_top), 6, conc.)

p_office_top <- office_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "#50C878") +
  labs(x = "[Location4]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_office_top

#outdoor
outdoor_top <- outdoor %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
outdoor_top <- top_n(ungroup(outdoor_top), 5, conc.)

p_outdoor_top <- outdoor_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "tomato2") +
  labs(x = "[Location5]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_outdoor_top

#welcome
welcome_top <- welcome %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
welcome_top <- top_n(ungroup(welcome_top), 6, conc.)

p_welcome_top <- welcome_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "midnightblue") +
  labs(x = "Welcome Room", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_welcome_top

#all locations
grid.arrange(p_barracks_n_top, p_barracks_s_top, p_dining_top,
             p_office_top, p_outdoor_top, p_welcome_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
              left = "Concentration\n(VOC ppbv or methane ppmv)")


#correlations
cor(outdoor$conc., barracks_n$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_066 %>% 
  slice(-X)
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
library(corrr)
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)


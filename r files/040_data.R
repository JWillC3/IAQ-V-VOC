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
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "Teaching Tree (Post)" and Fort Collins"
#change [location info] for where the canisters were located
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"
#make sure to slice and select the correct rows! Will not be the same for all sites
#load data
site_040 <- (read_csv(file = "./data/site_040_summa_data.csv")) %>% 
  slice(2:7) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_040 %>% 
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
#bears
bears <- voc %>% 
  filter(ID == "Bears")
#frogs
frogs <- voc %>% 
  filter(ID == "Frogs")
#lesson prep
lesson_prep <- voc %>% 
  filter(ID == "Lesson Prep")
#monkeys
monkeys <- voc %>% 
  filter(ID == "Monkeys")
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

#for ratio plots
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
       title = "Site 040 - Teaching Tree (Post), Summa Cannister Deployment\nOct. 24 - Oct. 31, 2023. Fort Collins, CO",
       caption = "6 canisters deployed throughout site\n5 indoor: 4 first floor, 1 second floor\n1 outdoor, roof top")
  
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
        #panel.grid.minor = element_line("black""))
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment, Oct. 24 - 31, 2023")
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
#   ggtitle("Site 040 Summa Cannister Deployment",
#           "Oct. 24 - Oct. 31, 2023. City, CO")
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
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
  ggtitle("Site 040 - Teaching Tree (Post), Outdoor (Rooftop)")
od_plot

#bears
l1_plot <- bears %>%
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
  ggtitle("Site 040 - Teaching Tree (Post), Bears",
          "first floor classroom")
l1_plot

#frogs
l2_plot <- frogs %>%
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
  ggtitle("Site 040 - Teaching Tree (Post), Frogs",
          "first floor classroom")
l2_plot

#lesson_prep
l3_plot <- lesson_prep %>%
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
  ggtitle("Site 040 - Teaching Tree (Post), Lesson Prep",
          "second floor office")
l3_plot

#monkeys
l4_plot <- monkeys %>%
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
  ggtitle("Site 040 - Teaching Tree (Post), Monkeys",
          "first floor classroom")
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
  ggtitle("040 - Site Teaching Tree (Post), Office",
          "first floor office")
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
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios\n Concentration Ratios") +
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment",
          "Oct. 24 - Oct. 31, 2023. Fort Collins, CO")
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
      #panel.grid.minor = element_line("black""))#,
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
  ggtitle("Site 040 - Teaching Tree (Post), Summa Cannister Deployment: Oct. 24 - Oct. 31, 2023. Fort Collins, CO")
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
  ggtitle("Site 040 - Teaching Tree (Post), Indoor Summa Cannister Deployment",
          "Grouped by Analyte Class")
fctg_ratio_plot

#create object of the top 5 analytes by outdoor ratio then plot
#bears od ratio
bears_top_or <- bears %>% 
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
frogs_top_or <- frogs %>% 
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
lesson_prep_top_or <- lesson_prep %>% 
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
monkeys_top_or <- monkeys %>% 
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
office_top_or <- office %>% 
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
grid.arrange(p_bears_top, p_frogs_top, p_lesson_prep_top,
             p_monkeys_top, p_office_top, p_od_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
              left = "Concentration\n(VOC ppbv or methane ppmv)")


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
library(corrr)
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)
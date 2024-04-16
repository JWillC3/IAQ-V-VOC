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


#replace the site number and name first, select match case
#change the dates for plots "Mmm. dd - Mmm. dd, YYYY."
#change [Add date] for plotlys
#change the site location "[Name]" and [City]"
#change [location info] for where the canisters were located
#"Location X]" for all indoor locations
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"
#make sure to slice and select the correct rows! Will not be the same for all sites
#load data
site_XXX <- (read_csv(file = "./data/site_XXX_summa_data.csv")) %>% 
  slice(2:5, 7:8) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_XXX %>% 
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
location1 <- voc %>% 
  filter(ID == "[Location1]")
#location 2
location2 <- voc %>% 
  filter(ID == "[Location2]")
#location 3
location3 <- voc %>% 
  filter(ID == "[Location3]")
#location 4
location4 <- voc %>% 
  filter(ID == "[Location4]")
#location 5
location5 <- voc %>% 
  filter(ID == "[Location5]")
#location 6
location6 <- voc %>% 
  filter(ID == "[Location6]")

#indoor
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

#for ratio plots
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
       title = "Site XXX - [Name], Summa Cannister Deployment\nMmm. dd - Mmm. dd, YYYY. [City], CO",
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment, [Add date]")
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
#   ggtitle("Site XXX Summa Cannister Deployment",
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment",
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment",
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment",
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
#location1
l1_plot <- location1 %>%
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
  ggtitle("Site XXX - [Name], [Location1]",
          "[additional info if needed]")
l1_plot

#location2
l2_plot <- location2 %>%
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
  ggtitle("Site XXX - [Name], [Location2]",
          "[additional info if needed]")
l2_plot

#location3
l3_plot <- location3 %>%
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
  ggtitle("Site XXX - [Name], [Location3]",
          "[additional info if needed]")
l3_plot

#location4
l4_plot <- location4 %>%
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
  ggtitle("Site XXX - [Name], [Location4]",
          "[additional info if needed]")
l4_plot

#location5
l5_plot <- location5 %>%
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
  ggtitle("XXX - Site [Name], [Location5]",
          "[additional info if needed]")
l5_plot

#location6
l6_plot <- location6 %>%
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
  ggtitle("Site XXX - [Name], [Location6]",
          "([location of can])")
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. [City], CO")
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
  ggtitle("Site XXX - [Name], Summa Cannister Deployment, [Add date]")
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
  ggtitle("Site XXX - [Name], Indoor Summa Cannister Deployment",
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
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")

grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "Analytes Grouped by Category", left = "Concentraion\n (VOC ppbv or methane ppmv")

#create object of the top 5 analytes by conc. for the indoor locations then plot
#location 1
location1_top <- location1 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location1_top <- top_n(ungroup(location1_top), 5, conc.) 

p_location1_top <- location1_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "orchid") +
  labs(x = "[Location1]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location1_top

#location2
location2_top <- location2 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location2_top <- top_n(ungroup(location2_top), 5, conc.)

p_location2_top <- location2_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "chocolate4") +
  labs(x = "[Location2]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location2_top

#location3
location3_top <- location3 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location3_top <- top_n(ungroup(location3_top), 5, conc.)

p_location3_top <- location3_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "goldenrod2") +
  labs(x = "[Location3]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location3_top

#location4
location4_top <- location4 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location4_top <- top_n(ungroup(location4_top), 5, conc.)

p_location4_top <- location4_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "#50C878") +
  labs(x = "[Location4]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location4_top

#location5
location5_top <- location5 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location5_top <- top_n(ungroup(location5_top), 5, conc.)

p_location5_top <- location5_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "tomato2") +
  labs(x = "[Location5]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location5_top

#location6
location6_top <- location6 %>% 
  group_by(analyte) %>% 
  arrange(desc(conc.))
location6_top <- top_n(ungroup(location6_top), 5, conc.)

p_location6_top <- location6_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, conc.), y = conc.),
           stat = "identity", fill = "midnightblue") +
  labs(x = "[Location6]", y = "") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) 
p_location6_top

#all locations
grid.arrange(p_location1_top, p_location2_top, p_location3_top,
             p_location4_top, p_location5_top, p_location6_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes at Each Location",
              left = "Concentration\n(VOC ppbv or methane ppmv)")


#correlations
cor(outdoor$conc., location1$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_XXX %>% 
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

----------------------------------------------------------------------
  
#AES Test Lab
#add theme in line 408 to keep axis label angle
#point
#theme minimal
voc_plot_min <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 1.5, alpha = 0.5) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
voc_plot_min + 
 scale_color_brewer(palette = "Dark2")
  #scale_color_manual(values = c("orchid", "chocolate4", "goldenrod2","#50C878",
                              # "tomato2", "midnightblue"))

#bar/col, YUCK!
voct1 <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_col(position = "dodge", fill = "lightblue", color = "darkblue") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
voct1

#location1 vs outdoor
goofy <- ggplot() +
  geom_point(data = location1, aes(x = analyte, y = conc., color = "location1")) +
  geom_point(data = outdoor, aes(x = analyte, y = conc., color = "Outdoor")) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 4, angle = 45, hjust = 1)) +
  scale_color_manual(name = "[Location]", 
                     values = c("#0041C2", "#FFDB58"),
                     labels = c("[Location]", "Outdoor"))
goofy +theme(
  # Change legend background color
  legend.background = element_rect(fill = "darkgray"),
  legend.key = element_rect(fill = NA, color = NA))  

#working with custom legend
donald <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  dark_theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")

#dark theme with viridis colors manual
donald +
  scale_color_manual(values = c("#fde725", "#7ad151",
                                  "#22a884", "#2a788e", "#414487",
                                  "#440154"),
                       labels = c("Location 1", "Location 2",
                                       "Location 3\nInfo", "Location 4",
                                       "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "cyan", size = 10),
                 legend.text = element_text(color = "white"))

#dark w/ lava theme
donald +
  scale_color_manual(values = c("#c33b18", "#351d19",
                                "#f28525", "#fbd449", "#ae5c5f",
                                "#7f3029"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "#6c7476", size = 10),
        legend.text = element_text(color = "#f3d18c"))

#gray lava
gr_lava <- ggplot(voc, aes(x = reorder(analyte, conc.),
                y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
gr_lava +
  scale_color_manual(values = c("#bc2e16", "#7a2a1c",
                                "#f64009", "#af6f70", "#e66d1d",
                                "#fbc117"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "#d1644b", size = 10),
        legend.text = element_text(color = "#261e1b"))


#summer theme with
summer <- ggplot(voc, aes(x = reorder(analyte, conc.),
                          y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
summer +
  scale_color_manual(values = c("#065143", "#839b5d",
                                "#df5e21", "#3f88c5", "#aab6cb",
                                "#bc2e16"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#alaska air theme
summer +
  scale_color_manual(values = c("#065143", "#b1d887",
                                "#00b2d6", "#007cba", "#01416e",
                                "#aaaaaa"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#70s theme
summer +
  scale_color_manual(values = c("#FFE5B4", "#CD853F",
                                "#EB5406", "#007cba", "#191970",
                                "#659EC7"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))

#summer colorful
summer +
  scale_color_manual(values = c("#3090C7", "#FFE87C",
                                "#A74AC7", "#12AD2B", "#C04000",
                                "#F98B88"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black"))


#pink foam
pink_foam <- ggplot(voc, aes(x = reorder(analyte, conc.),
                           y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_gray() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
pink_foam +
  scale_color_manual(values = c("#54bebe", "#006A4E",
                                "#e27c7c", "#503f3f", "#df979e",
                                "#c80064"),
                     labels = c("Location 1", "Location 2",
                                "Location 3\nInfo", "Location 4",
                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#ratio w/ yellowrainbow
yellowrainbow <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                 y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  #theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")

yellowrainbow + scale_color_manual(values = c("#EDDA74", "#ffc501",
                                              "#ff9801", "#037d50", "#024b30"),
                                   labels = c("Location 1", "Location 2",
                                              "Location 3\nInfo", "Location 4",
                                              "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#icecream
yellowrainbow + scale_color_manual(values = c("orchid", "chocolate4",
                                              "goldenrod2", "tomato2", "midnightblue"),
                                   labels = c("Location 1", "Location 2",
                                              "Location 3\nInfo", "Location 4",
                                              "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#viridis discrete
yellowrainbow + scale_colour_viridis(discrete = TRUE,
                                     labels = c("Location 1", "Location 2",
                                                "Location 3\nInfo", "Location 4",
                                                "Location 5", "Location 6")) +
  theme(legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "#261e1b"))

#viridis magama
viridismagma <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                    y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  dark_theme_gray() +
  scale_color_viridis(option = "plasma", discrete = TRUE) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site XXX Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
viridismagma

#google docs
googledocs <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                   y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_gdocs() +
  scale_color_gdocs() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site XXX, [Name] Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. City, CO")
googledocs
#tableau
tableau <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                 y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_igray() +
  scale_color_tableau() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site XXX, [Name] Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. [City], CO")
tableau

#economist
eco <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                      y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_economist() +
  scale_color_economist() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 0)) +
  xlab("Analytes") +
  ylab(expression(atop("Indoor to Outdoor Ratios"))) +
  ggtitle("Site XXX, [Name] Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. [City], CO")

eco + theme(legend.key.size = unit(2, "cm"))

#solarized
solarized <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                          y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_solarized() +
  scale_color_solarized() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site XXX, [Name] Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. [City], CO")
solarized + theme(
  # Change legend background color
  legend.background = element_rect(fill = NA),
  legend.key = element_rect(fill = NA, color = NA))  

#solarized plotly
solarized2 <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                                y = od_ratio, color = ID,
                                text = paste("Analyte: ", analyte,
                                             "<br> Conc. :", conc.,
                                             "<br> Class: ", category))) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_solarized() +
  scale_color_solarized() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site XXX, [Name] Summa Cannister Deployment",
          "Mmm. dd - Mmm. dd, YYYY. [City], CO")
solarized2 + theme(
  # Change legend background color
  legend.background = element_rect(fill = NA),
  legend.key = element_rect(fill = NA, color = NA))
ggplotly(solarized2, tooltip = "text")

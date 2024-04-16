#packages
library(readr)
library(dplyr)
library(plyr)
library(forcats)
library(ggplot2)
library(tidyr)
library(purrr)
library(faraway)
library(cowplot)
library(reshape2)
library(corrplot)
library(tibble)
library(scales)
library(ggthemes)
library(stringr)
library(gridExtra)
library(patchwork)
library(gghighlight)
library(ggdark)
library(viridis)
library(DT)
library(plotly)


#replace the site number and name first
# change the dates for plots "Jul. 27 - Aug. 03, 2023."
#change the site location "Alamosa"
#load data
#change the names of locaions in csv and double check rows and cols to keep
site_085 <- (read_csv(file = "./data/site_085_summa_data.csv")) %>% 
  slice(2:4) %>%
  select(1,7:67)

#pivot data for plots
p.analytes <- site_085 %>% 
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
#add voc categories to dfs
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
  filter(ID =="outdoor")
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

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON dinning, 2, etc.!
#subgroups,
#indoor
indoor <- voc %>% 
  filter(ID != "outdoor")
#dinning
dinning <- voc %>% 
  filter(ID == "dinning")
#room_5
room_5 <- voc %>% 
  filter(ID == "room_5")

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

#top 5 ratios 
# goofy <- clinic_waiting %>% 
#   group_by(analyte) %>% 
#   arrange(desc(od_ratio))
# 
# goofy <- top_n(ungroup(goofy), 5, od_ratio)
 

#plot all VOCs at site

voc_plot <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_calc() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  scale_colour_tableau() +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 085 Summa Cannister Deployment",
          "Jul. 27 - Aug. 03, 2023. Alamosa, CO")
voc_plot


#plotly output but y axis not formatted correctly and don't know how to fix
#WHEN ADDING THIS TO RMD MAKE SURE TO CHANGE THE DF TO INLCUDE LONG CAT NAMES
voc_plot2 <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID,
                            text = paste("Analyte: ", analyte,
                                         "<br> Conc. :", conc.,
                                         "<br> Class: ", category))) +
  geom_point(shape = 18, size = 5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_calc() + #try using different themes here
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  scale_colour_tableau() +
  xlab("Analytes") +
  ylab("Concentration\n(VOC ppbv or methane ppmv)") +
  ggtitle("Site 085: La Puente Summa Cannister Deployment")
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

voc_combo_point <- ggplot(indoor, aes(color = ID, x = analyte, y = conc.)) +
  geom_point(data = indoor, aes(x = analyte, y = conc.),
             shape = 18, alpha = 0.5) +
  geom_line(data = outdoor, aes(x = analyte, y =  conc.,
                                group = ID, alpha = 0.5)) +
  guides(alpha = "none") +
  xlab("Analytes") +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  ggtitle("Site 085 Summa Cannister Deployment",
          "Jul. 27 - Aug. 03, 2023. Alamosa, CO")
voc_combo_point

#facet wrap by location
loc_fctw <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc.)) +
  geom_point(color = "#50C878", shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.,
                                 color ="red")) +
  guides(size = "none", color = "none") +
  xlab("Analytes") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_wrap(~ID, scales = "free_y") +
  theme(axis.text.x = element_text(size = 3, angle = 45, hjust = 1)) +
  ggtitle("Site 085 Summa Cannister Deployment",
            "Grouped by Cannister location")
loc_fctw

#Two plots together not joined
voc_plot +
  loc_fctw +
  plot_layout(nrow = 2, heights = c(1, 2))


#facet grid by analyte
voc_fctg <- ggplot(voc, aes(x = reorder(analyte, conc.),
                            y = conc., color = ID)) +
  geom_point(shape = 18, alpha = 0.5) +
  geom_point(data = methane, aes(x = analyte, y = conc.)) +
  guides(size = "none") +
  facet_grid(~ category, scales = "free_x") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) 
voc_fctg

#indoor facet grid
indoor_fctg <- ggplot(indoor, aes(color = ID, x = analyte, y = conc.)) +
  geom_point(data = indoor, aes(x = reorder(analyte, conc.), y = conc.),
             size = 3, shape = 18, alpha = 0.5) +
  facet_grid(.~ category, scales = "free", space = "free") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab(expression(atop("Concentration",paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("Site 085 Summa Cannister Deployment",
          "Grouped by Analyte Class")
indoor_fctg

#plots for each room
#outdoor
od_plot <- outdoor %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("La Puente Outdoor (rooftop)")
od_plot

#dinning
dinning_plot <- dinning %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("La Puente Dinning")
dinning_plot

#room_5
room_5_plot <- room_5 %>%
  ggplot(aes(x = reorder(analyte, conc.), y = conc., color = ID)) +
  geom_point(color = "#50C878", shape = 18, size = 3) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(x = "Analytes",
       y = expression(atop("Concentration",
                           paste("(VOC ppbv or methane ppmv)")))) +
  ggtitle("La Puente room_5",
          "2nd Floor Men's Wing")
room_5_plot

#indoor, outdoor ratio 
#without facet
ratio_plot <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 085 Summa Cannister Deployment",
          "Jul. 27 - Aug. 03, 2023. Alamosa, CO")
ratio_plot

#plotly ratio plot
ratio_plotly <- ggplot(indoor, aes(x = reorder(analyte, od_ratio),
                              y = od_ratio, color = ID),
                              text = paste("Analyte: ", voc$analyte,
                                           "<br> Conc. :", voc$od_ratio,
                                           "<br> Class: ", voc$category)) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc., size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(breaks = c(10e3, 10e2, 10e1, 10e0, 10e-1, 10e-2, 10e-3),
                labels = trans_format(`log10`, math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  xlab("Analytes") +
  ylab("Indoor to Outdoor Ratios") +
  ggtitle("Site 085 Summa Cannister Deployment: Jul. 27 - Aug. 03, 2023. Alamosa, CO")
ggplotly(ratio_plotly)

#with facet grid
fctg_ratio_plot <- ggplot(indoor) +
  geom_point(aes(x = reorder(analyte, od_ratio), y = od_ratio, color = ID),
             size = 3, shape = 18, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                                 size = 1.5)) +
  # guides(size = "none") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  facet_grid(.~ category, scales = "free", switch = "x", space = "free_x") +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories", y = "outdoor ratio")
fctg_ratio_plot

#create object of the top 5 analytes for the indoor locations then plot
#dinning
dinning_top <- dinning %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
dinning_top <- top_n(ungroup(dinning_top), 5, od_ratio) 

p_dinning_top <- dinning_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "dinning", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_dinning_top

#room_5
room_5_top <- room_5 %>% 
  group_by(analyte) %>% 
  arrange(desc(od_ratio))
room_5_top <- top_n(ungroup(room_5_top), 5, od_ratio)

p_room_5_top <- room_5_top %>% 
  ggplot() +
  geom_bar(aes(x = reorder(analyte, od_ratio), y = od_ratio),
           stat = "identity", fill = "darkblue") +
  labs(x = "room_5", y = "") +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
p_room_5_top

grid.arrange(p_dinning_top, p_room_5_top,
             ncol = 3, nrow = 2,
             top = "Top 5 Analytes in Each Location", left = "Outdoor Ratio")

#with facet wrap
ratio_fctw <-  ggplot(data = indoor, aes(x = reorder(analyte, od_ratio),
                                             y = od_ratio, color = ID)) +
  geom_point(shape = 18, size = 3.5, alpha = 0.5) +
  # geom_point(data = methane, aes(x = analyte, y = conc.,
  #                 alpha = 0.5)) +
  # guides(size = "none", alpha = "none") +
  facet_wrap(~ category, scales = "free_x", labeller = label_parsed) +
  scale_y_log10(labels = trans_format(`log10`, math_format(10^.x))) +
  theme(strip.placement = "outside", strip.text = element_text(size  = 8),
        strip.background = element_blank()) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(x = "Analytes Grouped in VOC Categories", y = "outdoor ratio")
ratio_fctw

#correlations 
cor(outdoor$conc., dinning$conc., method = "spearman")
cor(outdoor$conc., room_5$conc., method = "spearman")
#create a new df for correlation. Slice out "outdoor" col. num.
voc_cor <- site_085 %>% 
  slice(-2)

#(not working for this site!!!)
#convert select rows to numeric 
voc_cor[2:62] <- lapply(voc_cor[2:62], as.numeric)
#run the correlation
vcor <- cor(voc_cor[, unlist(lapply(voc_cor, is.numeric))], method = "spearman")
#create the matrix
#if working with smaller set can have values inside of matrix using:
#addCoef.col = "black"
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.5)
#lower
corrplot(vcor, method = "color", type = "lower")
#upper w/ diag lables
corrplot(vcor, method = "color", type = "upper", tl.col = "black",
         tl.srt = 45, tl.cex = 0.5)
#red theme
corrplot(vcor, method = "color", tl.col = "black", tl.cex = 0.5,
         col=colorRampPalette(c("blue","white","red"))(200))
#another method for correlation
library(corrr)
cor2 <- correlate(voc_cor, method = "spearman", diagonal = 1)
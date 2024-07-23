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
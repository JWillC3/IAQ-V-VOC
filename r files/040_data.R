#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "Teaching Tree (Post)" and Fort Collins"
#change [location info] for where the canisters were located
#for plot colors: Location 1: "orchid", Location 2: "chocolate4",
#Location 3: "goldenrod2",Location 4: "#50C878",
#Location 5: "tomato2", Location 6: "midnightblue"

#calculate ratios
indoor_040 <- indoor_040 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_040$conc./outdoor_040$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
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
site_040_table <- data_table(sites, "040")

site_dt(site_040_table, "040")

#box plot for all analytes
bp_040 <- box_plot(site_040)
bp_040

#plot all VOCs at site
p_040 <- p_site(site_040, "040")
p_040

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_040, tooltip = "text")

#top 10 analyte concentrations for all locations
site_040_top <- top_n_analytes(site_040, n = 45)

site_040_top <- top_plot(site_040_top, "040", fill = "blue")
site_040_top

#facet wrap by location
p_040_fctw <- fct_wrap(site_040, "040")
p_040_fctw 

#leave this out for now
#Two plots together not joined
# p_040 +
#   p_040_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

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
p_040_cat_fctw <- cat_fct_wrap(indoor_040, "040")
p_040_cat_fctw

#plots for each room
#outdoor
p_040od <- room_plot(outdoor_040, "040", "midnightblue", "Outdoor, rooftop")

p_040od

#bears
p_bears <- room_plot(bears, "040", "orchid", "Bears, first floor classroom")
p_bears

#frogs
p_frogs <- room_plot(frogs, "040", "chocolate4", "Frogs, first floor classroom")
p_frogs

#lesson_prep
p_lesson_prep <- room_plot(lesson_prep, "040", "goldenrod2",
                           "Lesson Prep, second floor office")
p_lesson_prep

#monkeys
p_monkeys <- room_plot(monkeys, "040", "#50c878",
                       "Monkeys, first floor classroom")
p_monkeys

#office
p_office <- room_plot(office, "040", "tomato2", "Large Office, first floor")
p_office

#all rooms
grid.arrange(p_bears, p_frogs, p_monkeys, p_lesson_prep, p_office, p_040od, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#bears
#testing something new for scale y in this plot
bears_top <- top_n_analytes(bears, n = 10)

p_bears_top <- loc_top_plot(bears_top, "orchid", "Bears")
p_bears_top

#frogs
frogs_top <- top_n_analytes(frogs, n = 10)

p_frogs_top <- loc_top_plot(frogs_top, "chocolate4", "Frogs")
p_frogs_top

#lesson_prep
lesson_prep_top <- top_n_analytes(lesson_prep, n = 10)

p_lesson_prep_top <- loc_top_plot(lesson_prep_top, "goldenrod2", "Lesson Prep")
p_lesson_prep_top

#monkeys
monkeys_top <- top_n_analytes(monkeys, n = 10)

p_monkeys_top <- loc_top_plot(monkeys_top, "#50c878", "Monkeys")
p_monkeys_top

#office
office_top <- top_n_analytes(office, n = 10)

p_office_top <- loc_top_plot(office_top, "tomato2", "Office")
p_office_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_040, n = 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor")
p_outdoor_top

#all locations
grid.arrange(p_bears_top, p_frogs_top, p_lesson_prep_top,
             p_monkeys_top, p_office_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_040_bp <- r_box_plot(indoor_040)
p_040_bp

#indoor to outdoor ratio plot for all analytes
p_040_ratio <- r_p_site(indoor_040, "040")
p_040_ratio

#plotly ratio
ggplotly(p_040_ratio, tooltip = "text")

#ratio plot with facet wrap
p_040_r_fctw <- r_fct_wrap(indoor_040, "040")
p_040_r_fctw

#create object of the top 5 analytes by outdoor ratio then plot
#bears outdoor ratio
bears_top_or <- top_n_or(indoor_040, "Bears", 10)

p_bears_top_or <- or_top_plot(bears_top_or, "orchid", "Bears")
p_bears_top_or

#frogs outdoor ratio
frogs_top_or <- top_n_or(indoor_040, "Bears", 10)

p_frogs_top_or <- or_top_plot(frogs_top_or, "chocolate4", "Frogs")
p_frogs_top_or

#lesson_prep outdoor ratio
lesson_prep_top_or <- top_n_or(indoor_040, "Lesson Prep", 10)

p_lesson_prep_top_or <- or_top_plot(lesson_prep_or, "goldenrod2", "Lesson Prep")
p_lesson_prep_top_or

#monkeys outdoor ratio
monkeys_top_or <- top_n_or(indoor_040, "Monkeys", 10)

p_monkeys_top_or <- or_top_plot(monkeys_top_or, "#50C878", "Monkeys")
p_monkeys_top_or

#office outdoor ratio
office_top_or <- top_n_or(indoor_040, "Office", 10)

p_office_top_or <- or_top_plot(office_top_or, "tomato2", "Office")
p_office_top_or

#all locations outdoor ratio
grid.arrange(p_bears_top_or, p_frogs_top_or, p_lesson_prep_top_or,
             p_monkeys_top_or, p_office_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#alcohol
p_alcohol <- p_category(alcohol, "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- p_category(aldehyde, "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- p_category(straight_chain, "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- p_category(aromatic, "Aromatics")
p_aromatic

#btex
p_btex <- p_category(btex, "Btex")
p_btex

#chlorinated
p_chlorinated <- p_category(chlorinated, "Chlorinated")
p_chlorinated

#ketone
p_ketone <- p_category(ketone, "Ketones")
p_ketone

#others
p_other <- p_category(other, "Other")
p_other

#categories plotted together
grid.arrange(p_alcohol, p_aldehyde, p_straight_chain, p_aromatic,
             ncol = 2, nrow = 2,
             top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")

grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")

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
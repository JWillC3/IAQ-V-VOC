#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "Mango House " 
#balcony, clinic_waiting, ect. for objects and plots
#Balcony, Clinic Waiting, ect. for name in plot titles or axis
#change "[floor] [room type]" for where the canisters were located


#calculate ratios
indoor_079 <- indoor_079 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_079$conc./outdoor_079$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
#location order should be alphabetical

#balcony
balcony <- site_079 %>% 
  filter(room_name == "Balcony")
#clinic_waiting
clinic_waiting <- site_079 %>% 
  filter(room_name == "Clinic Waiting")
#entrance
entrance <- site_079 %>% 
  filter(room_name == "Entrance")
#food_court_n
food_court_n <- site_079 %>% 
  filter(room_name == "Food Court N")
#food_court_s
food_court_s <- site_079 %>% 
  filter(room_name == "Food Court S")

#data table
site_079_table <- data_table(sites, "079")

site_dt(site_079_table, "079")

#box plot for all analytes
bp_079 <- box_plot(site_079)
bp_079

#plot all VOCs at site
p_079 <- p_site(site_079, "079")
p_079

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_079, tooltip = "text")

#TVOC for site by room
p_079_sum <- p_conc_room(site_079, "079 Mango House")
p_079_sum

#top 10 analyte concentrations for all locations
site_079_top <- top_n_analytes(site_079, 45)

site_079_top <- top_plot(site_079_top, "079", fill = "darkgreen")
site_079_top

#facet wrap by location
p_079_fctw <- fct_wrap(site_079, "079")
p_079_fctw 

#facet wrap by indoor location
p_079i_fctw <- fct_wrap(indoor_079, "079")
p_079i_fctw 

#leave this out for now
#Two plots together not joined
# p_079 +
#   p_079_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

#plots for each room
#outdoor
p_079outdoor <- room_plot(outdoor_079, "079", "midnightblue", "Outdoor, rooftop")
p_079outdoor

#balcony
p_balcony <- room_plot(balcony, "079", "orchid", "Balcony, [floor] [room type]")
p_balcony

#clinic_waiting
p_clinic_waiting <- room_plot(clinic_waiting, "079", "chocolate4", "Clinic Waiting, [floor] [room type]")
p_clinic_waiting

#entrance
p_entrance <- room_plot(entrance, "079", "goldenrod2", "Entrance, [floor] [room type]")
p_entrance

#food_court_n
p_food_court_n <- room_plot(food_court_n, "079", "#50c878", "Food Court N, [floor] [room type]")
p_food_court_n

#food_court_s
p_food_court_s <- room_plot(food_court_s, "079", "tomato2", "Food Court S, [floor] [room type]")
p_food_court_s

#all rooms
grid.arrange(p_balcony, p_clinic_waiting, p_food_court_n, p_entrance,
             p_food_court_s, p_079outdoor, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#balcony
#testing something new for scale y in this plot
balcony_top <- top_n_analytes(balcony, 10)

p_balcony_top <- loc_top_plot(balcony_top, "orchid", "Balcony top 10")
p_balcony_top

#clinic_waiting
clinic_waiting_top <- top_n_analytes(clinic_waiting, 10)

p_clinic_waiting_top <- loc_top_plot(clinic_waiting_top, "chocolate4", "Clinic Waiting top 10")
p_clinic_waiting_top

#entrance
entrance_top <- top_n_analytes(entrance, 10)

p_entrance_top <- loc_top_plot(entrance_top, "goldenrod2", "Entrance top 10")
p_entrance_top

#food_court_n
food_court_n_top <- top_n_analytes(food_court_n, 10)

p_food_court_n_top <- loc_top_plot(food_court_n_top, "#50c878", "Food Court N top 10")
p_food_court_n_top

#food_court_s
food_court_s_top <- top_n_analytes(food_court_s, 10)

p_food_court_s_top <- loc_top_plot(food_court_s_top, "tomato2", "Food Court S top 10")
p_food_court_s_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_079, 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
p_outdoor_top

#all locations
grid.arrange(p_balcony_top, p_clinic_waiting_top, p_entrance_top,
             p_food_court_n_top, p_food_court_s_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_079_bp <- r_box_plot(indoor_079)
p_079_bp

#indoor to outdoor ratio plot for all analytes
p_079_ratio <- r_p_site(indoor_079, "079")
p_079_ratio

#plotly ratio
ggplotly(p_079_ratio, tooltip = "text")

#ratio plot with facet wrap
p_079_r_fctw <- r_fct_wrap(indoor_079, "079")
p_079_r_fctw

#create object of the top 10 analytes by outdoor ratio then plot
#balcony outdoor ratio
balcony_top_or <- top_n_or(indoor_079, "Balcony", 10)

p_balcony_top_or <- or_top_plot(balcony_top_or, "orchid",
                                  "Balcony top 10 I/O Ratios")
p_balcony_top_or

#clinic_waiting outdoor ratio
clinic_waiting_top_or <- top_n_or(indoor_079, "Balcony", 10)

p_clinic_waiting_top_or <- or_top_plot(clinic_waiting_top_or, "chocolate4",
                                  "Clinic Waiting top 10 I/O Ratios")
p_clinic_waiting_top_or

#entrance outdoor ratio
entrance_top_or <- top_n_or(indoor_079, "Entrance", 10)

p_entrance_top_or <- or_top_plot(entrance_top_or, "goldenrod2",
                                  "Entrance top 10 I/O Ratios")
p_entrance_top_or

#food_court_n outdoor ratio
food_court_n_top_or <- top_n_or(indoor_079, "Food Court N", 10)

p_food_court_n_top_or <- or_top_plot(food_court_n_top_or, "#50C878",
                                  "Food Court N top 10 I/O Ratios")
p_food_court_n_top_or

#food_court_s outdoor ratio
food_court_s_top_or <- top_n_or(indoor_079, "Food Court S", 10)

p_food_court_s_top_or <- or_top_plot(food_court_s_top_or, "tomato2",
                                  "Food Court S top 10 I/O Ratios")
p_food_court_s_top_or

#all locations outdoor ratio
grid.arrange(p_balcony_top_or, p_clinic_waiting_top_or, p_entrance_top_or,
             p_food_court_n_top_or, p_food_court_s_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_079_cat_fctw <- cat_fct_wrap(site_079, "079 (All Locations)")
p_079_cat_fctw

#indoor facet wrap by analyte class
p_079i_cat_fctw <- cat_fct_wrap(indoor_079, "079 (Indoor Locations)")
p_079i_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "079")

p_category(p_alcohol, "079 ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "079")

p_category(p_aldehyde, "079 ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "079")

p_category(p_straight_chain, "079 ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "079")

p_category(p_aromatic, "079 ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "079")

p_category(p_btex, "079 ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "079")

p_category(p_chlorinated, "079 ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "079")

p_category(p_ketone, "079 ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "079")

p_category(p_other, "079 ", "Other")
p_other

#categories plotted together
grid.arrange(p_alcohol, p_aldehyde, p_straight_chain, p_aromatic,
             ncol = 2, nrow = 2,
             top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")

grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
             top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")


#----
#SRA medians
#compute median I/O for each analyte in each indoor location.

median_079 <- filter_and_summarize(indoor_079, analytes)

#----
# #correlations
cor(clinic_waiting$conc., balcony$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_079 %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(food_court_s$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_balcony <- p.analytes %>%
  filter(ID == "Balcony")
sum(tvoc_balcony$conc.)
tvoc_clinic_waiting <- p.analytes %>% 
  filter(ID == "Clinic Waiting")
sum(tvoc_clinic_waiting$conc., na.rm = TRUE)
tvoc_lessonprep <- p.analytes %>% 
  filter(ID == "Entrance")
sum(tvoc_lessonprep$conc., na.rm = TRUE)  
tvoc_food_court_n <- p.analytes %>% 
  filter(ID == "Food Court N")
sum(tvoc_food_court_n$conc., na.rm = TRUE)
tvoc_food_court_s <- p.analytes %>% 
  filter(ID == "Food Court S")
sum(tvoc_food_court_s$conc., na.rm = TRUE)
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
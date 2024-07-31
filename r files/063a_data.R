#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "High Desert Assisted Living B1 " 
#admin, dining, ect. for objects and plots
#Admin, Kitchen/Dining, ect. for name in plot titles or axis
#change "[floor] [room type]" for where the canisters were located


#calculate ratios
indoor_063A <- indoor_063A %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_063A$conc./outdoor_063A$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
#location order should be alphabetical

#admin
admin <- site_063A %>% 
  filter(room_name == "Admin")
#dining
dining <- site_063A %>% 
  filter(room_name == "Dining/Kitchen")
#room_09
room_09 <- site_063A %>% 
  filter(room_name == "Room 09")

#data table
site_063A_table <- sites %>% 
  filter(name == "High Desert A") %>% 
  select(4, 7, 9, 18)

site_dt(site_063A_table, "063A")


#box plot for all analytes
bp_063A <- box_plot(site_063A)
bp_063A

#plot all VOCs at site
p_063A <- p_site(site_063A, "063A")
p_063A

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_063A, tooltip = "text")

#TVOC for site by room
p_063A_sum <- p_conc_room(site_063A, "063A High Desert Assisted Living B1")
p_063A_sum

#top 10 analyte concentrations for all locations
site_063A_top <- top_n_analytes(site_063A, 45)

site_063A_top <- top_plot(site_063A_top, "063A", fill = "darkgreen")
site_063A_top

#facet wrap by location
p_063A_fctw <- fct_wrap(site_063A, "063A")
p_063A_fctw 

#facet wrap by indoor location
p_063Ai_fctw <- fct_wrap(indoor_063A, "063A")
p_063Ai_fctw 

#leave this out for now
#Two plots together not joined
# p_063A +
#   p_063A_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

#plots for each room
#outdoor
p_063Aoutdoor <- room_plot(outdoor_063A, "063A", "midnightblue", "Outdoor, rooftop")
p_063Aoutdoor

#admin
p_admin <- room_plot(admin, "063A", "orchid", "Admin, [floor] [room type]")
p_admin

#dining
p_dining <- room_plot(dining, "063A", "chocolate4", "Kitchen/Dining, [floor] [room type]")
p_dining

#room_09
p_room_09 <- room_plot(room_09, "063A", "goldenrod2", "Room 09, [floor] [room type]")
p_room_09

#all rooms
grid.arrange(p_admin, p_dining, p_room_09, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#admin
#testing something new for scale y in this plot
admin_top <- top_n_analytes(admin, 10)

p_admin_top <- loc_top_plot(admin_top, "orchid", "Admin top 10")
p_admin_top

#dining
dining_top <- top_n_analytes(dining, 10)

p_dining_top <- loc_top_plot(dining_top, "chocolate4", "Kitchen/Dining top 10")
p_dining_top

#room_09
room_09_top <- top_n_analytes(room_09, 10)

p_room_09_top <- loc_top_plot(room_09_top, "goldenrod2", "Room 09 top 10")
p_room_09_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_063A, 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
p_outdoor_top

#all locations
grid.arrange(p_admin_top, p_dining_top, p_room_09_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_063A_bp <- r_box_plot(indoor_063A)
p_063A_bp

#indoor to outdoor ratio plot for all analytes
p_063A_ratio <- r_p_site(indoor_063A, "063A")
p_063A_ratio

#plotly ratio
ggplotly(p_063A_ratio, tooltip = "text")

#ratio plot with facet wrap
p_063A_r_fctw <- r_fct_wrap(indoor_063A, "063A")
p_063A_r_fctw

#create object of the top 10 analytes by outdoor ratio then plot
#admin outdoor ratio
admin_top_or <- top_n_or(indoor_063A, "Admin", 10)

p_admin_top_or <- or_top_plot(admin_top_or, "orchid",
                                  "Admin top 10 I/O Ratios")
p_admin_top_or

#dining outdoor ratio
dining_top_or <- top_n_or(indoor_063A, "Admin", 10)

p_dining_top_or <- or_top_plot(dining_top_or, "chocolate4",
                                  "Kitchen/Dining top 10 I/O Ratios")
p_dining_top_or

#room_09 outdoor ratio
room_09_top_or <- top_n_or(indoor_063A, "Room 09", 10)

p_room_09_top_or <- or_top_plot(room_09_top_or, "goldenrod2",
                                  "Room 09 top 10 I/O Ratios")
p_room_09_top_or

#all locations outdoor ratio
grid.arrange(p_admin_top_or, p_dining_top_or, p_room_09_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_063A_cat_fctw <- cat_fct_wrap(site_063A, "063A (All Locations)")
p_063A_cat_fctw

#indoor facet wrap by analyte class
p_063Ai_cat_fctw <- cat_fct_wrap(indoor_063A, "063A (Indoor Locations)")
p_063Ai_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "063A")

p_category(p_alcohol, "063A ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "063A")

p_category(p_aldehyde, "063A ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "063A")

p_category(p_straight_chain, "063A ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "063A")

p_category(p_aromatic, "063A ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "063A")

p_category(p_btex, "063A ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "063A")

p_category(p_chlorinated, "063A ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "063A")

p_category(p_ketone, "063A ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "063A")

p_category(p_other, "063A ", "Other")
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

median_063A <- filter_and_summarize(indoor_063A, analytes)

#----
# #correlations
cor(dining$conc., admin$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_063A %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(location5$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_admin <- p.analytes %>%
  filter(ID == "Admin")
sum(tvoc_admin$conc.)
tvoc_dining <- p.analytes %>% 
  filter(ID == "Kitchen/Dining")
sum(tvoc_dining$conc., na.rm = TRUE)
tvoc_lessonprep <- p.analytes %>% 
  filter(ID == "Room 09")
sum(tvoc_lessonprep$conc., na.rm = TRUE)  
tvoc_location4 <- p.analytes %>% 
  filter(ID == "location_4")
sum(tvoc_location4$conc., na.rm = TRUE)
tvoc_location5 <- p.analytes %>% 
  filter(ID == "location_5")
sum(tvoc_location5$conc., na.rm = TRUE)
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
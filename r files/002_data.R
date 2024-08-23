#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "Routt Co. Jail " 
#booking, kitchen, ect. for objects and plots
#Booking, Kitchen, ect. for name in plot titles or axis
#change "[floor] [room type]" for where the canisters were located


#calculate ratios
indoor_002 <- indoor_002 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_002$conc./outdoor_002$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
#location order should be alphabetical

#booking
booking <- site_002 %>% 
  filter(room_name == "Booking")
#kitchen
kitchen <- site_002 %>% 
  filter(room_name == "Kitchen")

#data table
site_002_table <- data_table(sites, "002")

site_dt(site_002_table, "002")

#box plot for all analytes
bp_002 <- box_plot(site_002)
bp_002

#plot all VOCs at site
p_002 <- p_site(site_002, "002")
p_002

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_002, tooltip = "text")

#TVOC for site by room
p_002_sum <- p_conc_room(site_002, "002 Routt Co. Jail")
p_002_sum

#top 10 analyte concentrations for all locations
site_002_top <- top_n_analytes(site_002, 17)

site_002_top <- top_plot(site_002_top, "002", fill = "darkgreen")
site_002_top

#facet wrap by location
p_002_fctw <- fct_wrap(site_002, "002")
p_002_fctw 

#facet wrap by indoor location
p_002i_fctw <- fct_wrap(indoor_002, "002")
p_002i_fctw 

#leave this out for now
#Two plots together not joined
# p_002 +
#   p_002_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

#plots for each room
#outdoor
p_002outdoor <- room_plot(outdoor_002, "002", "midnightblue", "Outdoor, rooftop")
p_002outdoor

#booking
p_booking <- room_plot(booking, "002", "orchid", "Booking, [floor] [room type]")
p_booking

#kitchen
p_kitchen <- room_plot(kitchen, "002", "chocolate4", "Kitchen, [floor] [room type]")
p_kitchen


#all rooms
grid.arrange(p_booking, p_kitchen, p_002outdoor, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#booking
#testing something new for scale y in this plot
booking_top <- top_n_analytes(booking, 10)

p_booking_top <- loc_top_plot(booking_top, "orchid", "Booking top 10")
p_booking_top

#kitchen
kitchen_top <- top_n_analytes(kitchen, 10)

p_kitchen_top <- loc_top_plot(kitchen_top, "chocolate4", "Kitchen top 10")
p_kitchen_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_002, 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
p_outdoor_top

#all locations
grid.arrange(p_booking_top, p_kitchen_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_002_bp <- r_box_plot(indoor_002)
p_002_bp

#indoor to outdoor ratio plot for all analytes
p_002_ratio <- r_p_site(indoor_002, "002")
p_002_ratio

#plotly ratio
ggplotly(p_002_ratio, tooltip = "text")

#ratio plot with facet wrap grpuped by category
p_002_r_fctw <- r_fct_wrap(indoor_002, "002")
p_002_r_fctw

#create object of the top 10 analytes by outdoor ratio then plot
#booking outdoor ratio
booking_top_or <- top_n_or(indoor_002, "Booking", 10)

p_booking_top_or <- or_top_plot(booking_top_or, "orchid",
                                  "Booking top 10 I/O Ratios")
p_booking_top_or

#kitchen outdoor ratio
kitchen_top_or <- top_n_or(indoor_002, "Booking", 10)

p_kitchen_top_or <- or_top_plot(kitchen_top_or, "chocolate4",
                                  "Kitchen top 10 I/O Ratios")
p_kitchen_top_or


#all locations outdoor ratio
grid.arrange(p_booking_top_or, p_kitchen_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_002_cat_fctw <- cat_fct_wrap(site_002, "002 (All Locations)")
p_002_cat_fctw

#indoor facet wrap by analyte class
p_002i_cat_fctw <- cat_fct_wrap(indoor_002, "002 (Indoor Locations)")
p_002i_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "002")

p_category(p_alcohol, "002 ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "002")

p_category(p_aldehyde, "002 ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "002")

p_category(p_straight_chain, "002 ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "002")

p_category(p_aromatic, "002 ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "002")

p_category(p_btex, "002 ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "002")

p_category(p_chlorinated, "002 ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "002")

p_category(p_ketone, "002 ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "002")

p_category(p_other, "002 ", "Other")
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
#I/O for each analyte
median_002 <- filter_and_summarize(indoor_002, analytes)
#I/O for each room


#----
# #correlations
cor(kitchen$conc., booking$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_002 %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(location5$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_booking <- p.analytes %>%
  filter(ID == "Booking")
sum(tvoc_booking$conc.)
tvoc_kitchen <- p.analytes %>% 
  filter(ID == "Kitchen")
sum(tvoc_kitchen$conc., na.rm = TRUE)
tvoc_lessonprep <- p.analytes %>% 
  filter(ID == "location_3")
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


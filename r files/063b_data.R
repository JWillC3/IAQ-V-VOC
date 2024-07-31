#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "High Desert Assisted Living B 2 " 
#admin, dining, ect. for objects and plots
#Admin, Dining/Kitchen, ect. for name in plot titles or axis
#change "[floor] [room type]" for where the canisters were located


#calculate ratios
indoor_063B <- indoor_063B %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_063B$conc./outdoor_063B$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
#location order should be alphabetical

#admin
admin <- site_063B %>% 
  filter(room_name == "Admin")
#dining
dining <- site_063B %>% 
  filter(room_name == "Dining/Kitchen")

#data table
site_063B_table <- sites %>% 
  filter(name == "High Desert B") %>% 
  select(4, 7, 9, 18)

site_dt(site_063B_table, "063B")

#box plot for all analytes
bp_063B <- box_plot(site_063B)
bp_063B

#plot all VOCs at site
p_063B <- p_site(site_063B, "063B")
p_063B

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_063B, tooltip = "text")

#TVOC for site by room
p_063B_sum <- p_conc_room(site_063B, "063B High Desert Assisted Living B 2")
p_063B_sum

#top 10 analyte concentrations for all locations
site_063B_top <- top_n_analytes(site_063B, 45)

site_063B_top <- top_plot(site_063B_top, "063B", fill = "darkgreen")
site_063B_top

#facet wrap by location
p_063B_fctw <- fct_wrap(site_063B, "063B")
p_063B_fctw 

#facet wrap by indoor location
p_063Bi_fctw <- fct_wrap(indoor_063B, "063B")
p_063Bi_fctw 

#leave this out for now
#Two plots together not joined
# p_063B +
#   p_063B_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

#plots for each room
#outdoor
p_063Boutdoor <- room_plot(outdoor_063B, "063B", "midnightblue", "Outdoor, rooftop")
p_063Boutdoor

#admin
p_admin <- room_plot(admin, "063B", "orchid", "Admin, [floor] [room type]")
p_admin

#dining
p_dining <- room_plot(dining, "063B", "chocolate4", "Dining/Kitchen, [floor] [room type]")
p_dining

#all rooms
grid.arrange(p_admin, p_dining, p_063Boutdoor, 
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

p_dining_top <- loc_top_plot(dining_top, "chocolate4", "Dining/Kitchen top 10")
p_dining_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_063B, 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
p_outdoor_top

#all locations
grid.arrange(p_admin_top, p_dining_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_063B_bp <- r_box_plot(indoor_063B)
p_063B_bp

#indoor to outdoor ratio plot for all analytes
p_063B_ratio <- r_p_site(indoor_063B, "063B")
p_063B_ratio

#plotly ratio
ggplotly(p_063B_ratio, tooltip = "text")

#ratio plot with facet wrap
p_063B_r_fctw <- r_fct_wrap(indoor_063B, "063B")
p_063B_r_fctw

#create object of the top 10 analytes by outdoor ratio then plot
#admin outdoor ratio
admin_top_or <- top_n_or(indoor_063B, "Admin", 10)

p_admin_top_or <- or_top_plot(admin_top_or, "orchid",
                                  "Admin top 10 I/O Ratios")
p_admin_top_or

#dining outdoor ratio
dining_top_or <- top_n_or(indoor_063B, "Admin", 10)

p_dining_top_or <- or_top_plot(dining_top_or, "chocolate4",
                                  "Dining/Kitchen top 10 I/O Ratios")
p_dining_top_or

#all locations outdoor ratio
grid.arrange(p_admin_top_or, p_dining_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_063B_cat_fctw <- cat_fct_wrap(site_063B, "063B (All Locations)")
p_063B_cat_fctw

#indoor facet wrap by analyte class
p_063Bi_cat_fctw <- cat_fct_wrap(indoor_063B, "063B (Indoor Locations)")
p_063Bi_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "063B")

p_category(p_alcohol, "063B ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "063B")

p_category(p_aldehyde, "063B ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "063B")

p_category(p_straight_chain, "063B ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "063B")

p_category(p_aromatic, "063B ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "063B")

p_category(p_btex, "063B ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "063B")

p_category(p_chlorinated, "063B ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "063B")

p_category(p_ketone, "063B ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "063B")

p_category(p_other, "063B ", "Other")
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

median_063B <- filter_and_summarize(indoor_063B, analytes)

#----
# #correlations
cor(dining$conc., admin$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_063B %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(location5$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_admin <- p.analytes %>%
  filter(ID == "Admin")
sum(tvoc_admin$conc.)
tvoc_dining <- p.analytes %>% 
  filter(ID == "Dining/Kitchen")
sum(tvoc_dining$conc., na.rm = TRUE)
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
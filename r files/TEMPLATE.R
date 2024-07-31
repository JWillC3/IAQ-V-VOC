#load data
source("source_data.R")

#replace the site number and name first
#change the dates for plots "Oct. 24 - Oct. 31, 2023."
#change the site location "[Site Name] " 
#location1, location2, ect. for objects and plots
#location_1, location_2, ect. for name in plot titles or axis
#change "[floor] [room type]" for where the canisters were located


#calculate ratios
indoor_XXX <- indoor_XXX %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_XXX$conc./outdoor_XXX$conc.))

#UPDATE LOCATIONS FOR THE SITE YOU ARE CURRENTLY WORKING ON, Location 1, 2, etc.!
#location order should be alphabetical

#location1
location1 <- site_XXX %>% 
  filter(room_name == "location_1")
#location2
location2 <- site_XXX %>% 
  filter(room_name == "location_2")
#location3
location3 <- site_XXX %>% 
  filter(room_name == "location_3")
#location4
location4 <- site_XXX %>% 
  filter(room_name == "location_4")
#location5
location5 <- site_XXX %>% 
  filter(room_name == "location_5")

#data table
site_XXX_table <- data_table(sites, "XXX")

site_dt(site_XXX_table, "XXX")

#box plot for all analytes
bp_XXX <- box_plot(site_XXX)
bp_XXX

#plot all VOCs at site
p_XXX <- p_site(site_XXX, "XXX")
p_XXX

#plotly output but y axis not formatted correctly and don't know how to fix
ggplotly(p_XXX, tooltip = "text")

#TVOC for site by room
p_XXX_sum <- p_conc_room(site_XXX, "XXX [Site Name]")
p_XXX_sum

#top 10 analyte concentrations for all locations
site_XXX_top <- top_n_analytes(site_XXX, 45)

site_XXX_top <- top_plot(site_XXX_top, "XXX", fill = "darkgreen")
site_XXX_top

#facet wrap by location
p_XXX_fctw <- fct_wrap(site_XXX, "XXX")
p_XXX_fctw 

#facet wrap by indoor location
p_XXXi_fctw <- fct_wrap(indoor_XXX, "XXX")
p_XXXi_fctw 

#leave this out for now
#Two plots together not joined
# p_XXX +
#   p_XXX_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

#plots for each room
#outdoor
p_XXXooutdoor <- room_plot(outdoor_XXX, "XXX", "midnightblue", "Outdoor, rooftop")
p_XXXoutdoor

#location1
p_location1 <- room_plot(location1, "XXX", "orchid", "location_1, [floor] [room type]")
p_location1

#location2
p_location2 <- room_plot(location2, "XXX", "chocolate4", "location_2, [floor] [room type]")
p_location2

#location3
p_location3 <- room_plot(location3, "XXX", "goldenrod2", "location_3, [floor] [room type]")
p_location3

#location4
p_location4 <- room_plot(location4, "XXX", "#50c878", "location_4, [floor] [room type]")
p_location4

#location5
p_location5 <- room_plot(location5, "XXX", "tomato2", "location_5, [floor] [room type]")
p_location5

#all rooms
grid.arrange(p_location1, p_location2, p_location4, p_location3, p_location5, p_XXXod, 
             ncol = 3,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#create object of the top 5 analytes by conc. for the indoor locations then plot
#location1
#testing something new for scale y in this plot
location1_top <- top_n_analytes(location1, 10)

p_location1_top <- loc_top_plot(location1_top, "orchid", "location_1 top 10")
p_location1_top

#location2
location2_top <- top_n_analytes(location2, 10)

p_location2_top <- loc_top_plot(location2_top, "chocolate4", "location_2 top 10")
p_location2_top

#location3
location3_top <- top_n_analytes(location3, 10)

p_location3_top <- loc_top_plot(location3_top, "goldenrod2", "location_3 top 10")
p_location3_top

#location4
location4_top <- top_n_analytes(location4, 10)

p_location4_top <- loc_top_plot(location4_top, "#50c878", "location_4 top 10")
p_location4_top

#location5
location5_top <- top_n_analytes(location5, 10)

p_location5_top <- loc_top_plot(location5_top, "tomato2", "location_5 top 10")
p_location5_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_XXX, 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
p_outdoor_top

#all locations
grid.arrange(p_location1_top, p_location2_top, p_location3_top,
             p_location4_top, p_location5_top, p_outdoor_top,
             ncol = 3, nrow = 2,
             top = "Top 10 Analytes at Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#indoor, outdoor ratio 

#boxplot of indoor / outdoor ratios for all analytes
p_XXX_bp <- r_box_plot(indoor_XXX)
p_XXX_bp

#indoor to outdoor ratio plot for all analytes
p_XXX_ratio <- r_p_site(indoor_XXX, "XXX")
p_XXX_ratio

#plotly ratio
ggplotly(p_XXX_ratio, tooltip = "text")

#ratio plot with facet wrap
p_XXX_r_fctw <- r_fct_wrap(indoor_XXX, "XXX")
p_XXX_r_fctw

#create object of the top 10 analytes by outdoor ratio then plot
#location1 outdoor ratio
location1_top_or <- top_n_or(indoor_XXX, "location_1", 10)

p_location1_top_or <- or_top_plot(location1_top_or, "orchid",
                                  "location_1 top 10 I/O Ratios")
p_location1_top_or

#location2 outdoor ratio
location2_top_or <- top_n_or(indoor_XXX, "location_1", 10)

p_location2_top_or <- or_top_plot(location2_top_or, "chocolate4",
                                  "location_2 top 10 I/O Ratios")
p_location2_top_or

#location3 outdoor ratio
location3_top_or <- top_n_or(indoor_XXX, "location_3", 10)

p_location3_top_or <- or_top_plot(location3_top_or, "goldenrod2",
                                  "location_3 top 10 I/O Ratios")
p_location3_top_or

#location4 outdoor ratio
location4_top_or <- top_n_or(indoor_XXX, "location_4", 10)

p_location4_top_or <- or_top_plot(location4_top_or, "#50C878",
                                  "location_4 top 10 I/O Ratios")
p_location4_top_or

#location5 outdoor ratio
location5_top_or <- top_n_or(indoor_XXX, "location_5", 10)

p_location5_top_or <- or_top_plot(location5_top_or, "tomato2",
                                  "location_5 top 10 I/O Ratios")
p_location5_top_or

#all locations outdoor ratio
grid.arrange(p_location1_top_or, p_location2_top_or, p_location3_top_or,
             p_location4_top_or, p_location5_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_XXX_cat_fctw <- cat_fct_wrap(site_XXX, "XXX (All Locations)")
p_XXX_cat_fctw

#indoor facet wrap by analyte class
p_XXXi_cat_fctw <- cat_fct_wrap(indoor_XXX, "XXX (Indoor Locations)")
p_XXXi_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "XXX")

p_category(p_alcohol, "XXX ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "XXX")

p_category(p_aldehyde, "XXX ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "XXX")

p_category(p_straight_chain, "XXX ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "XXX")

p_category(p_aromatic, "XXX ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "XXX")

p_category(p_btex, "XXX ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "XXX")

p_category(p_chlorinated, "XXX ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "XXX")

p_category(p_ketone, "XXX ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "XXX")

p_category(p_other, "XXX ", "Other")
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
analytes <- as.data.frame(unique(sites$analyte))
analytes <- rename(analytes, analyte = "unique(sites$analyte)")

#function
filter_and_summarize <- function(df, analytes) {
  # Extract the list of analyte names
  analytes_list <- analytes$analyte
  
  # Initialize an empty list to store results
  results_list <- list()
  
  for (analyte in analytes_list) {
    result <- df %>%
      filter(analyte == !!analyte) %>%
      group_by(room_name, analyte) %>%
      summarize(median_or_ratio = median(od_ratio, na.rm = TRUE), .groups = 'drop')
    
    results_list[[analyte]] <- result
  }
  
  # Combine results into a single data frame
  combined_results <- bind_rows(results_list, .id = "analyte")
  
  return(combined_results)
}

median_XXX <- filter_and_summarize(indoor_XXX, analytes)


#----
# #correlations
cor(location2$conc., location1$conc., method = "spearman")
#repeat the above for all location you want correlation for
#create a new df for correlation. Slice out "outdoor" col. num. [-X]
voc_cor <- site_XXX %>%
  slice(1:2)

#get all TVOC signals for each room, ex. "sum(location5$conc., na.rm = TRUE)"
p.analytes$conc. <- as.numeric(as.character(p.analytes$conc.))

tvoc_location1 <- p.analytes %>%
  filter(ID == "location_1")
sum(tvoc_location1$conc.)
tvoc_location2 <- p.analytes %>% 
  filter(ID == "location_2")
sum(tvoc_location2$conc., na.rm = TRUE)
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
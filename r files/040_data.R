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

#TVOC for site by room
p_040_sum <- p_conc_room(site_040, "040 Teaching Tree (Post)")
p_040_sum

#top 10 analyte concentrations for all locations
site_040_top <- top_n_analytes(site_040, n = 45)

site_040_top <- top_plot(site_040_top, "040", fill = "darkgreen")
site_040_top

#facet wrap by location
p_040_fctw <- fct_wrap(site_040, "040")
p_040_fctw 

#facet wrap by indoor location
p_040i_fctw <- fct_wrap(indoor_040, "040 (indoor locations)")
p_040i_fctw 

#leave this out for now
#Two plots together not joined
# p_040 +
#   p_040_fctw +
#   plot_layout(nrow = 2, heights = c(1, 2))

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

p_bears_top <- loc_top_plot(bears_top, "orchid", "Bears top 10")
p_bears_top

#frogs
frogs_top <- top_n_analytes(frogs, n = 10)

p_frogs_top <- loc_top_plot(frogs_top, "chocolate4", "Frogs top 10")
p_frogs_top

#lesson_prep
lesson_prep_top <- top_n_analytes(lesson_prep, n = 10)

p_lesson_prep_top <- loc_top_plot(lesson_prep_top, "goldenrod2",
                                  "Lesson Prep top 10")
p_lesson_prep_top

#monkeys
monkeys_top <- top_n_analytes(monkeys, n = 10)

p_monkeys_top <- loc_top_plot(monkeys_top, "#50c878", "Monkeys top 10")
p_monkeys_top

#office
office_top <- top_n_analytes(office, n = 10)

p_office_top <- loc_top_plot(office_top, "tomato2", "Office top 10")
p_office_top

#outdoor
outdoor_top <- top_n_analytes(outdoor_040, n = 10)

p_outdoor_top <- loc_top_plot(outdoor_top, "midnightblue", "Outdoor top 10")
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

p_bears_top_or <- or_top_plot(bears_top_or, "orchid",
                              "Bears top 10 I/O Ratios")
p_bears_top_or

#frogs outdoor ratio
frogs_top_or <- top_n_or(indoor_040, "Frogs", 10)

p_frogs_top_or <- or_top_plot(frogs_top_or, "chocolate4",
                              "Frogs top 10 I/O Ratios")
p_frogs_top_or

#lesson_prep outdoor ratio
lesson_prep_top_or <- top_n_or(indoor_040, "Lesson Prep", 10)

p_lesson_prep_top_or <- or_top_plot(lesson_prep_top_or, "goldenrod2",
                                    "Lesson Prep top 10 I/O Ratios")
p_lesson_prep_top_or

#monkeys outdoor ratio
monkeys_top_or <- top_n_or(indoor_040, "Monkeys", 10)

p_monkeys_top_or <- or_top_plot(monkeys_top_or, "#50C878",
                                "Monkeys top 10 I/O Ratios")
p_monkeys_top_or

#office outdoor ratio
office_top_or <- top_n_or(indoor_040, "Office", 10)

p_office_top_or <- or_top_plot(office_top_or, "tomato2",
                               "Office top 10 I/O Ratios")
p_office_top_or

#all locations outdoor ratio
grid.arrange(p_bears_top_or, p_frogs_top_or, p_lesson_prep_top_or,
             p_monkeys_top_or, p_office_top_or,
             ncol = 3, nrow = 2,
             top = "Top 10 I/O Ratios in Each Location",
             left = "Concentration\n(VOC ppbv or methane ppmv)")

#category plots
#facet wrap by analyte class
p_040_cat_fctw <- cat_fct_wrap(site_040, "040 (All Locations)")
p_040_cat_fctw

#indoor facet wrap by analyte class
p_040i_cat_fctw <- cat_fct_wrap(indoor_040, "040 (Indoor Locations)")
p_040i_cat_fctw

#alcohol
p_alcohol <- alcohol %>% 
  filter(site_id == "040")

p_category(p_alcohol, "040 ", "Alcohols")
p_alcohol

#aldehyde
p_aldehyde <- aldehyde %>% 
  filter(site_id == "040")

p_category(p_aldehyde, "040 ", "Aldehydes")
p_aldehyde

#straight chain
p_straight_chain <- straight_chain %>% 
  filter(site_id == "040")

p_category(p_straight_chain, "040 ", "Straight Chains")
p_straight_chain

#aromatic
p_aromatic <- aromatic %>% 
  filter(site_id == "040")

p_category(p_aromatic, "040 ", "Aromatics")
p_aromatic

#btex
p_btex <- btex %>% 
  filter(site_id == "040")

p_category(p_btex, "040 ", "Btex")
p_btex

#chlorinated
p_chlorinated <- chlorinated %>% 
  filter(site_id == "040")

p_category(p_chlorinated, "040 ", "Chlorinated")
p_chlorinated

#ketone
p_ketone <- ketone %>% 
  filter(site_id == "040")

p_category(p_ketone, "040 ", "Ketones")
p_ketone

#others
p_other <- other %>% 
  filter(site_id == "040")

p_category(p_other, "040 ", "Other")
p_other

# #categories plotted together
# grid.arrange(p_alcohol, p_aldehyde, p_straight_chain, p_aromatic,
#              ncol = 2, nrow = 2,
#              top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")
# 
# grid.arrange(p_btex, p_chlorinated, p_ketone, p_other, ncol = 2, nrow = 2,
#              top = "", left = "Concentraion\n (VOC ppbv or methane ppmv")

#----
#SRA meadians
#compute median I/O for each analyte in each indoor location. 
# analytes <- as.data.frame(unique(sites$analyte))
# analytes <- rename(analytes, analyte = "unique(sites$analyte)")

# indoor_040 %>% 
#   filter(analyte == "acetone") %>% 
#   group_by(room_name, analyte) %>% 
#   summarize(median_or_ratio = median(od_ratio))
# 
# acetone_040 <- indoor_040 %>% 
#   filter(analyte == "acetone") %>% 
#   select("room_name", "od_ratio") %>% 
#   mutate(median_or = median(od_ratio))

# #function
# filter_and_summarize <- function(df, analytes) {
#   # Extract the list of analyte names
#   analytes_list <- analytes$analyte
#   
#   # Initialize an empty list to store results
#   results_list <- list()
#   
#   for (analyte in analytes_list) {
#     result <- df %>%
#       filter(analyte == !!analyte) %>%
#       group_by(room_name, analyte) %>%
#       summarize(median_or_ratio = median(od_ratio, na.rm = TRUE), .groups = 'drop')
#     
#     results_list[[analyte]] <- result
#   }
#   
#   # Combine results into a single data frame
#   combined_results <- bind_rows(results_list, .id = "analyte")
#   
#   return(combined_results)
# }

median_040 <- filter_and_summarize(indoor_040, analytes)


#----
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
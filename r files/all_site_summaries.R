source("source_data.R")

#mean conc. for all analytes across ALL indoor locations
indoor %>% 
  select(analyte, conc.) %>% 
  group_by(analyte) %>% 
  summarise(conc._mean = mean(conc.)) %>% 
  print(n = 61)

#mean conc. for all analytes across ALL outdoor locations
outdoor %>% 
  select(analyte, conc.) %>% 
  group_by(analyte) %>% 
  summarise(conc._mean = mean(conc.)) %>% 
  print(n = 61)

#data table
sites_table <- sites %>% 
  select(1,2,4,7,9,18)
datatable(sites_table, colnames = c("Site ID", "Name", "Location", "Analyte",
                              "Concentration", "Category"),
          options = list(pageLenght = 10), rownames = FALSE,
          caption = "All Sites Table, Concentrations: ppb(v) or methane ppb(v)")

#----
#site voc concentration sums
#site 040
p_conc_room(site_040, "040")

#SITE 063 A
p_conc_room(site_063A, "063A")

#SITE 063 B
p_conc_room(site_063B, "063B")

#SITE 066
p_conc_room(site_066, "066")


site_066_top <- top_n_analytes(site_066, 47)
p_site_066_top <- top_plot(site_066_top, "orange", "066")
p_site_066_top

# SITE 079
p_conc_room(site_079, "079")

# SITE 085
p_conc_room(site_085, "085")

# SITE 086
p_conc_room(site_086, "086")

# SITE 099
p_conc_room(site_099, "099")

# SITE 103
p_conc_room(site_103, "103")

#SITE 105
p_conc_room(site_105, "105")

#SITE 106
p_conc_room(site_106, "106")

p_106 <- p_site(site_106, "106")
p_106

site_106_top <- top_n_analytes(site_106, 56)
p_site_106_top <- top_plot(site_106_top, "orange", "106")
p_site_106_top


# SITE 107
p_conc_room(site_107, "107")

#sites 108
p_conc_room(site_108, "108")

#site 094
p_conc_room(site_094, "094")
#grid for all summary plots
grid.arrange(site_094_sum, site_063a_sum, site_063b_sum, site_066_sum, 
             site_079_sum, site_085_sum, site_086_sum, site_099_sum,
             site_103_sum, site_105, site_106, site_107_sum, site_108_sum,
             site_094_sum,
             ncol = 3, nrow = 4,
             bottom = "Rooms Sampled", left = "Sum of VOC Sampled (ppb(v))")


#----
#box plot for all indoor locations
bp_indoor <- ggplot(indoor, aes(x = reorder(analyte, conc.), 
                                y = conc., #fill = analyte
)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC Conectrations: All Indoor Locations in CO\n (n = 18)") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration")
bp_indoor 

#site_040 boxplot
bp_040 <- site_040 %>% 
  ggplot(aes(x = reorder(analyte, conc.), y = conc.)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "Concentration")
bp_040

#apartments


#temp living locations


#classroom locations


#lobby locations


#healthcare room locations


#recreation locations


#kitchen/dinning locations


#staff locations


#outdoor concentrations
#-----------
p_outdoor <- p_locations(outdoor, "Outdoor")
p_outdoor

#top 10 outdoor for all sites
sites_top_out <- top_n_analytes(outdoor, 47)
sites_top_out$analyte <- factor(sites_top_out$analyte,
  levels = c("ethane", "propane", "acetone", "n-butane", "ethene", "isopropanol",
             "acetaldehyde", "i-butane", "ethyne", "acetonitrile"), ordered = TRUE)

ggplot(sites_top_out, aes(x = analyte, y = conc.)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) +
  labs(x = "Analyte", y = "Concentration (ppb)") +
  ggtitle("Top 10 Outdoor Analytes Across All Sites") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))


#indoor concentrations
#-----------
p_indoor <- p_locations(indoor, "Indoor")
p_indoor +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red")

ggplotly(p_indoor, tooltip = "text")

sites_top_in <- top_n_analytes(indoor, 220)
sites_top_in$analyte <- factor(sites_top_in$analyte, levels = c("isopropanol", "ethane",
  "propane", "n-butane", "i-butane", "acetone", "acetaldehyde", "butanol", 
  "ethene", "methane"), ordered = TRUE)

ggplot(sites_top_in, aes(x = analyte, y = conc.)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 1)) +
  labs(x = "Analyte", y = "Concentration (ppb)") +
  ggtitle("Top 10 Analytes Across All Sites") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
  

#----
#SRA medians
#compute median I/O for each analyte in each indoor location.
#calculate ratios 002
indoor_002 <- indoor_002 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_002$conc./outdoor_002$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 040
indoor_040 <- indoor_040 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_040$conc./outdoor_040$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 063A
indoor_063A <- indoor_063A %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_063A$conc./outdoor_063A$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 063B
indoor_063B <- indoor_063B %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_063B$conc./outdoor_063B$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 066
indoor_066 <- indoor_066 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_066$conc./outdoor_066$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 079
indoor_079 <- indoor_079 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_079$conc./outdoor_079$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 085
indoor_085 <- indoor_085 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_085$conc./outdoor_085$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 086
indoor_086 <- indoor_086 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_086$conc./outdoor_086$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 089
indoor_089 <- indoor_089 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_089$conc./outdoor_089$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 094
indoor_094 <- indoor_094 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_094$conc./outdoor_094$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 099
indoor_099 <- indoor_099 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_099$conc./outdoor_099$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 101
indoor_101 <- indoor_101 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_101$conc./outdoor_101$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 103
indoor_103 <- indoor_103 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_103$conc./outdoor_103$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 105
indoor_105 <- indoor_105 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_105$conc./outdoor_105$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 106
indoor_106 <- indoor_106 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_106$conc./outdoor_106$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 107
indoor_107 <- indoor_107 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_107$conc./outdoor_107$conc.)) %>% 
  select(1,4,7,9,20)
#calculate ratios 108
indoor_108 <- indoor_108 %>%
  group_by(room_name, analyte) %>%
  ungroup() %>% 
  mutate(od_ratio = (indoor_108$conc./outdoor_108$conc.)) %>% 
  select(1,4,7,9,20)

ratio_list <- list(indoor_002, indoor_040, indoor_063A, indoor_063B, indoor_066,
                    indoor_079, indoor_085, indoor_086, indoor_089, indoor_094,
                    indoor_099, indoor_101, indoor_103, indoor_105, indoor_105,
                    indoor_106, indoor_107, indoor_108)

ratio_list <- bind_rows(ratio_list)

# ratio_list$ratio_cat <- cut(ratio_list$od_ratio,
#                                  breaks = c(-Inf, 1, 10, Inf),
#                                  labels = c("Less than 1", "1 to 10",
#                                             "Greater than 10"))

ratio_list$ratio_category <- cut(ratio_list$od_ratio,
                                 breaks = c(-Inf, 1, 10, Inf),
                                 labels = c("Less than 1", "1 to 10", "Greater than 10"))

ratio_list_filtered <- ratio_list[!is.na(ratio_list$ratio_category), ]

#plot of all ratios for each site
p_ratio_list <- ratio_list %>% 
  ggplot(aes(x = reorder(analyte, od_ratio), y = od_ratio,
             color = site_id,
             text = paste("Site ID: ", site_id,
                          "<br> Analyte: ", analyte,
                          "<br> I/O :", od_ratio))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC I/O Ratios") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "I/O Ratio") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))

ggplotly(p_ratio_list, tooltip = "text")


#all ratios from all canister locations box plot


ggplot(ratio_list_filtered,
       aes(x = fct_reorder(analyte, od_ratio, .fun = "median", .desc = TRUE),
           y = od_ratio)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "I/O Ratio") +
  ggtitle("I/O Ratios for All Canister Locations Arranged by Median") +
  facet_grid(~ ratio_cat, scales = "free_y")

# donald <- indoor_002 %>% 
#   select(1,4,7,9,20)
# 
# print(donald, n = 100)

#----
#Help from Ben 8/22/24
# List all objects in the environment
all_objects <- ls()

# Filter the objects that match the pattern "indoor_" followed by a number
indoor_objects <- grep("^indoor_\\d+$", all_objects, value = TRUE)

# Combine the dataframes while keeping only the specified columns
indoor_all <- do.call(rbind, lapply(indoor_objects, function(df_name) {
  df <- get(df_name)
  df[, c("site_id", "room_name", "analyte", "od_ratio")]
}))

# Summarize the od_ratio by analyte for each site_id and room_name combination
summary_stats <- as.data.frame(indoor_all) %>%
  mutate(analyte = as.factor(analyte)) %>% 
  group_by(room_name) %>%
  summarize(
    mean_od_ratio = mean(od_ratio, na.rm = TRUE),
    median_od_ratio = median(od_ratio, na.rm = TRUE),
    p5_od_ratio = quantile(od_ratio, 0.05, na.rm = TRUE),
    p95_od_ratio = quantile(od_ratio, 0.95, na.rm = TRUE),
    p25_od_ratio = quantile(od_ratio, 0.25, na.rm = TRUE),
    p75_od_ratio = quantile(od_ratio, 0.75, na.rm = TRUE)
  )

# View the summary statistics
summary_stats

#
median_df <- as.data.frame(median_list) %>%
  group_by(analyte) %>%
  summarize(median_analyte = median(median_or_ratio)) %>%
  ungroup()

# Check the structure of your dataframe
str(indoor_all)

#----
median_002 <- filter_and_summarize(indoor_002, analytes_df) %>%
  mutate(site_id = "002", .before = analyte)
median_040 <- filter_and_summarize(indoor_040, analytes_df) %>% 
  mutate(site_id = "040", .before = analyte)
median_063A <- filter_and_summarize(indoor_063A, analytes_df) %>% 
  mutate(site_id = "063A", .before = analyte)
median_063B <- filter_and_summarize(indoor_063B, analytes_df) %>% 
  mutate(site_id = "063B", .before = analyte)
median_066 <- filter_and_summarize(indoor_066, analytes_df) %>% 
  mutate(site_id = "066", .before = analyte)
median_079 <- filter_and_summarize(indoor_079, analytes_df) %>% 
  mutate(site_id = "079", .before = analyte)
median_085 <- filter_and_summarize(indoor_085, analytes_df) %>% 
  mutate(site_id = "085", .before = analyte)
median_086 <- filter_and_summarize(indoor_086, analytes_df) %>% 
  mutate(site_id = "086", .before = analyte)
median_089 <- filter_and_summarize(indoor_089, analytes_df) %>% 
  mutate(site_id = "089", .before = analyte)
median_094 <- filter_and_summarize(indoor_094, analytes_df) %>% 
  mutate(site_id = "094", .before = analyte)
median_099 <- filter_and_summarize(indoor_099, analytes_df) %>% 
  mutate(site_id = "099", .before = analyte)
median_101 <- filter_and_summarize(indoor_101, analytes_df) %>% 
  mutate(site_id = "101", .before = analyte)
median_103 <- filter_and_summarize(indoor_103, analytes_df) %>% 
  mutate(site_id = "103", .before = analyte)
median_105 <- filter_and_summarize(indoor_105, analytes_df) %>% 
  mutate(site_id = "105", .before = analyte)
median_106 <- filter_and_summarize(indoor_106, analytes_df) %>% 
  mutate(site_id = "106", .before = analyte)
median_107 <- filter_and_summarize(indoor_107, analytes_df) %>% 
  mutate(site_id = "107", .before = analyte)
median_108 <- filter_and_summarize(indoor_108, analytes_df) %>% 
  mutate(site_id = "108", .before = analyte)

median_list <- list(median_002, median_040, median_063A, median_063B, median_066,
                median_079, median_085, median_086, median_089, median_094,
                median_099, median_101, median_103, median_105, median_105,
                median_106, median_107, median_108)

median_list <- bind_rows(median_list)

median_list %>% 
  ggplot(aes(x = reorder(analyte, median_or_ratio), y = median_or_ratio,
             color = site_id,
             text = paste("Analyte: ", analyte,
                          "<br> I/O :", median_or_ratio))) +
  geom_point(shape = 18, size = 3, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ggtitle("VOC I/O Ratios") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "I/O Ratio") +
  scale_color_manual(name = "Site ID",
                     values = c("#48bf8e", "#245a62", "#75b3d8", "#621da6",
                                "#e28de2", "#934270", "#e72fc2", "#5361c7",
                                "#b9cda1", "#096013", "#afe642", "#3aa609",
                                "#2af464", "#683d0d", "#efaa79", "#d6061a",
                                "#d9c937", "#9f04fc"))

#as boxplot
#this is not working like it should, need to figure out how to make the second
#box plot work for I/O medians for all sites...
ggplot(indoor_002,
       aes(x = fct_reorder(analyte, od_ratio, .fun = "median", .desc = TRUE),
           y = od_ratio)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(x = "Ananlyte", y = "I/O Ratio") +
  ggtitle("Site 002 I/O Ratio for All Rooms Arranged by Median")




#-----------
#attempting ratios





#-----------


#-----------
#Jade's analysis
# Plot scatter plots of Average TVOC Values Recorded 
ggplot(site_040, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 040 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_063A, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 063A Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )

ggplot(site_063B, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 063B Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_066, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 066 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_079, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 079 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_085, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 085 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_086, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 086 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_099, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 099 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_103, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 103 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )
ggplot(site_107, aes(x = analyte, y = conc., color = type)) +
  geom_point() +
  guides(size = "none") +
  labs(x = "Analyte", y = "Average TVOC Value Recorded") +
  ggtitle("Site 107 Scatter Plot") +
  theme_minimal() +
  ylim(0, 100) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),  # Center the title horizontally
    plot.subtitle = element_text(face = "italic"),  # Italicize subtitle (if needed)
    plot.caption = element_text(color = "gray20", size = 8)  # Customize caption
  )

# SITE 040 ROOM PAIR CORRELATIONS

# select bears & frogs rooms from site data sheeet 
bears_data <- subset(site_040, room == "Bears")
frogs_data <- subset(site_040, room == "Frogs")

# Select only the columns containing concentration data associated with each analyte 
bears_conc <- select(bears_data, conc., analyte)
frogs_conc <- select(frogs_data, conc., analyte)

# Select columns containing "conc." and ensure they are numeric
bears_numeric <- bears_conc %>%
  select(where(is.numeric))
frogs_numeric <- frogs_conc %>%
  select(where(is.numeric))

# Calculate correlations for each pair of analytes within Bears and Frogs
#Currently not calculating correctly, giving value of 1 for correlation. 
#Should I instead choose one analyte of interest, and gather correlations between rooms from there? 
bears_cor <- cor(bears_numeric, method = "spearman")
frogs_cor <- cor(frogs_numeric, method = "spearman")

print("Correlation matrix for Bears:")
print(bears_cor)
print("Correlation matrix for Frogs:")
print(frogs_cor)

#all possible room pairs from site 040
#bears & lesson prep
#bears & monkeys
#bears & outdoor
#bears & office 
#frogs & lesson prep
#frogs & monkeys
#frogs & office
#frogs & outdoor
#lesson prep & monkeys 
#lesson prep & office 
#lesson prep & outdoor 
#monkeys & office
#monkeys & outdoor
#office & outdoor


# Attempting to pull in qtrak data
path_data <- "/data_qtrak.rds"

# QTrak Data
data_qtrak <- read_rds(paste0(path_data, "/data_qtrak.rds")) %>%
  filter(datetime >= ymd_hms(datetime_start, tz = "US/Mountain")) %>%
  filter(datetime < ymd_hms(datetime_end, tz = "US/Mountain")) %>%
  left_join(inst_log$qtrak, by = "id_inst") %>%
  select(-level, date_installed, date_uninstalled)

voc <- data_qtrak %>%
  select(id_room, datetime, room, val, var) %>%
  mutate(location = gsub("_", " ", str_to_title(room))) %>%
  filter(grepl('totalvoc low24_ppm', var)) %>%
  mutate(voc = val*1000)

baseline <- data_qtrak %>%
  group_by(id_room, room) %>%
  mutate(min_time = min(datetime)) %>%
  #filter(datetime <= min_time + minutes(20)) %>%
  filter(grepl('totalvoc low24_ppm', var)) %>%
  summarise(baseline = median(val)*1000) %>%
  ungroup %>%
  mutate(avg_baseline = mean(baseline)) %>% group_by(id_room) %>%
  mutate(baseline_fix = avg_baseline - baseline)

# Join the baseline data frame with the voc data frame
voc <- voc %>%
  left_join(baseline, by = "id_room")

# Subtract the baseline from the voc values
voc <- voc %>%
  mutate(voc_baseline_subtracted = voc + baseline_fix)
#-----------


#-----------
#Ben's help
# Define the file path for the Excel file
file_path <- "./data/summa_date_time.xlsx"
base_path <- "T:/Projects/iaqv/data_rds"  # Base path for data files

# Read the Excel file into a dataframe
data <- read_excel(file_path)

# Convert to dataframe (although read_excel already returns a tibble, which is a type of dataframe)
df <- as.data.frame(data)

# Extract unique site_ids
site_ids <- unique(df$site_id)

# Function to read the data for each site_id
read_site_data <- function(site_id, base_path) {
  # Construct the paths for each file
  qtrak_path <- file.path(base_path, site_id, "data_qtrak.rds")
  instlog_path <- file.path(base_path, site_id, "data_instlog.rds")
  site_data_path <- file.path(base_path, site_id, "data_site.rds")
  
  # Initialize a list to store data for this site
  site_data <- list()
  
  # Read the data_qtrak.rds file if it exists
  if (file.exists(qtrak_path)) {
    site_data$data_qtrak <- readRDS(qtrak_path)
  } else {
    site_data$data_qtrak <- NULL
  }
  
  # Read the data_instlog.rds file if it exists
  if (file.exists(instlog_path)) {
    site_data$data_instlog <- readRDS(instlog_path)
  } else {
    site_data$data_instlog <- NULL
  }
  
  
  
  return(site_data)
}

# Loop through each site_id and read the data
site_data_list <- lapply(site_ids, read_site_data, base_path = base_path)



# Assign names to the list elements based on site_id
names(site_data_list) <- site_ids

# Display the site_data_list to verify contents
print(site_data_list)

# Initialize an empty vector to store unique 'var' values
unique_vars <- c()

# Loop through each site_id and extract unique 'var' values
for (site_id in names(site_data_list)) {
  site_data <- site_data_list[[site_id]]
  
  # Check if data_qtrak is not NULL
  if (!is.null(site_data$data_qtrak)) {
    # Extract unique 'var' values and combine them with the existing ones
    unique_vars <- unique(c(unique_vars, site_data$data_qtrak$var))
  }
}

# Display the unique 'var' values
print(unique_vars)

# List of VOC-related variables with ppm or equivalent units
voc_ppm_vars <- c(
  "voclow1_ppm", "voclow12_ppm", "totalvoc low24_ppm"
)


# Function to filter data_qtrak by VOC-related variables with ppm or equivalent units
filter_voc_ppm <- function(data_qtrak) {
  # Check if data_qtrak is not NULL
  if (!is.null(data_qtrak)) {
    # Filter the data_qtrak dataframe
    filtered_data <- data_qtrak %>%
      filter(var %in% voc_ppm_vars)
    return(filtered_data)
  } else {
    return(NULL)
  }
}

# Assuming site_data_list is already populated
# Loop through each site_id and filter data_qtrak
filtered_site_data <- lapply(site_data_list, function(site_data) {
  site_data$data_qtrak <- filter_voc_ppm(site_data$data_qtrak)
  return(site_data)
})

# Function to extract and transform data from each site
# Function to extract and transform data from each site
# Function to extract and transform data from each site
extract_data <- function(site_data, site_id) {
  excluded_sites <- c("107", "089")  # List of site_ids to exclude
  if (!(site_id %in% excluded_sites)) {  # Ignore site_ids in the list
    if (!is.null(site_data$data_qtrak) && nrow(site_data$data_qtrak) > 0) {
      data <- site_data$data_qtrak
      data <- data %>%
        mutate(site_id = site_id) %>%  # Include site_id
        select(datetime, site_id, id_inst, var, val)
      return(data)
    }
  }
  return(NULL)
}

# Use map2 to iterate over names and elements of the list and apply the function
all_data <- map2_df(filtered_site_data, names(filtered_site_data), extract_data)

# Function to extract id_inst and room from data_instlog$qtrak
extract_qtrak_data <- function(site_data, site_id) {
  if (!is.null(site_data$data_instlog$qtrak) && nrow(site_data$data_instlog$qtrak) > 0) {
    data <- site_data$data_instlog$qtrak
    data <- data %>%
      mutate(site_id = site_id)
    
    # Check if 'room' column exists, if not, create it with NA
    if (!"room" %in% colnames(data)) {
      data <- data %>% mutate(room = NA)
    }
    
    data <- data %>%
      select(site_id, id_inst, room) %>%
      filter(grepl("^[0-9]+$", id_inst))  # Keep only numeric id_inst
    
    return(data)
  }
  return(NULL)
}

# Use map2_df to iterate over names and elements of the list and apply the function
qtrak_data <- map2_df(site_data_list, names(site_data_list), extract_qtrak_data)

all_data <- all_data %>%
  left_join(qtrak_data, by = c("site_id" = "site_id", "id_inst" = "id_inst"))
# Convert dates and times to POSIXct
df <- df %>%
  mutate(
    start_datetime = as.POSIXct(paste(start_date, format(start_time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
    end_datetime = as.POSIXct(paste(end_date, format(end_time, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S")
  )

# Convert datetime columns to POSIXct if they are not already
all_data$datetime <- as.POSIXct(all_data$datetime, format = "%Y-%m-%d %H:%M:%S")
df$start_datetime <- as.POSIXct(df$start_datetime, format = "%Y-%m-%d %H:%M:%S")
df$end_datetime <- as.POSIXct(df$end_datetime, format = "%Y-%m-%d %H:%M:%S")

# Perform the join and filter based on datetime range
filtered_data <- all_data %>%
  left_join(df, by = c("site_id" = "site_id", "room" = "room")) %>%
  filter(datetime >= start_datetime & datetime <= end_datetime)

# Convert filtered_site_data$`079`$data_qtrak to a dataframe and get the first 5 rows for each id_inst
check <- filtered_data %>%
  group_by(site_id, room) %>%
  slice_head(n = 1)

#for correlation fingers crossed
# Select the required columns and calculate the average val
result <- filtered_data %>%
  select(datetime, site_id, id_inst, room, val) %>%
  group_by(site_id, id_inst, room) %>%
  summarise(sum_val = sum(val, na.rm = TRUE))

#-----------

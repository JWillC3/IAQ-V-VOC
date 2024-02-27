library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)

site.063 <- read.csv("./data/site_063_summa_data.csv")

## Isolate just the room data, convert to numeric
room.data <- site.063[3:8, 5:65]
## Transpose the room data
t.room.data <- data.frame(t(room.data),
                          stringsAsFactors = FALSE)
## Re-name the columns
names(t.room.data) <- as.character(site.063[3:8, 1])

t.room.data <- t.room.data %>% 
   rownames_to_column(var = "analyte") 


## Create a container called ratios
n.rows <- nrow(t.room.data)
n.cols <- ncol(t.room.data)
ratios <- as.data.frame(matrix(rep(0, n.rows*n.cols), nrow=n.rows, ncol=n.cols))
row.names(ratios) <- row.names(t.room.data)
colnames(ratios) <- colnames(t.room.data)

#this will do ratios for all rooms to outdoor
for (i in 1:n.cols) {
  ratios[ , i] <- as.numeric(t.room.data[ , i])/as.numeric(t.room.data$outdoor)
}
# to do the above for other ratios
#x <- as.numeric(t.room.data$room_24) / as.numeric(t.room.data$room_25)
#plot ratios w/ waterfall plot
# cor_df <- cor(t(ratios[ , -4]), method = "pearson")
# corrplot(cor_df)
# do for each room try this: room_24 <- as.numeric(t(t.room.data$room_24))

  
  
  
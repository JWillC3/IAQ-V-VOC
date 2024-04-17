# Libraries
library(tidyverse)
library(faraway)
library(cowplot)
library(reshape2)
library(corrplot)
library(scales)
library(ggthemes)
library(gridExtra)
library(patchwork)
library(gghighlight)
library(ggdark)
library(viridis)
library(DT)
library(plotly)
library(readxl)

# Data path
path <- "data/"


# read site data
sites <- read_csv(paste0(path, "voc_samples_all.csv"))


# function to plot total voc conc by room

p_conc_room <- function(df, site){

ggplot(df, aes(x = room, y = conc.)) +
  geom_bar(stat = "identity", fill = "midnightblue") +
  stat_summary(aes(label = stat(y)), fun = "sum", geom = "text",
               col = "white", vjust = 1.5) +
  ggtitle(paste0("Site ", site,  " VOC Samples by Canister Location")) +
  labs(x = "Room", y = "Sum of VOC Sampled (ppb(v))")+
  theme_bw() +
  theme(axis.text.y = element_blank())
}
  
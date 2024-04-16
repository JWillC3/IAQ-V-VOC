#packages 
library(readxl)
library(corrplot)
library(tidyverse)

#this won't work on other script?
#read.csv from readxl pkg for the correlation to work
summa = read.csv("./data/summa_data.csv",
                 row.names = 1, header = TRUE)
#method = "spearman"
cor <- cor(summa, method = "spearman") 

#so R will be able find the names of columns in the df
attach(summa)

#DON'T RUN THIS ONE!!!
#if chose specific analytes works, but can't do with all?
cor.test(summa[, unlist(lapply(summa, is.numeric))],
         method = "spearman", use = "pairwise.complete.obs") 
cor.test(summa, method = "spearman", exact = FALSE)
cor(summa, method = "spearman")

#the following produced a correlation matrix of all analytes
summa_correlation <- cor(summa[, 1:61], summa[1:61], method = "spearman",
                         use = "pairwise.complete.obs")
summa_correlation

#this one will only have colors based on the value
#make this more readable
corrplot(summa_correlation, method = "color")



# MSDS 6372 Summer 2019: Dr. Jacob Turner
# Project - 2: Logistic Linear Regression
# Team Members - Dhyan Shah, Chaushun Hu, Eric Balke
# Southern Methodist University - Masters of Science in Data Science - Class of 2020
# July 19, 2019
install.packages("caret")
install.packages("ggcorrplot")
install.packages("kernlab")
install.packages("kableExtra")
install.packages("ggpubr")
suppressWarnings(suppressMessages(library(tidyverse))) #required
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(kernlab))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(ggpubr))
#library('dataMaid')


#Session Info
sessionInfo()

#set your working directory for this code to work properly

wd <- "/Users/Dhyan/Desktop/MSDS /Statistic_Modeling/Projects/Project2"
#setwd(wd)

###################################
### RAW DATA IMPORT ###
###################################

BrCan<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=F,sep=",")

#write.csv(data, '/Users/Dhyan/Desktop/MSDS /Statistic_Modeling/Projects/Project2/BrCan.csv', row.names=T)

names(BrCan)<- c('id_number', 'diagnosis', 'radius_mean', 
                'texture_mean', 'perimeter_mean', 'area_mean', 
                'smoothness_mean', 'compactness_mean', 
                'concavity_mean','concave_points_mean', 
                'symmetry_mean', 'fractal_dimension_mean',
                'radius_se', 'texture_se', 'perimeter_se', 
                'area_se', 'smoothness_se', 'compactness_se', 
                'concavity_se', 'concave_points_se', 
                'symmetry_se', 'fractal_dimension_se', 
                'radius_worst', 'texture_worst', 
                'perimeter_worst', 'area_worst', 
                'smoothness_worst', 'compactness_worst', 
                'concavity_worst', 'concave_points_worst', 
                'symmetry_worst', 'fractal_dimension_worst')

#write.csv(data, '/Users/Dhyan/Desktop/MSDS /Statistic_Modeling/Projects/Project2/BrCan.csv', row.names = T)

# Data Summary
summary(BrCan)

# Normalize Data
BrCan.clean <- BrCan[,-c(1)]
normalize <- function(x){
  return (( x - min(x))/(max(x) -min(x)))
}  
BrCanNorm <- as.data.frame(
  lapply(BrCan.clean[,2:31],normalize)
)  
BrCanNorm <- cbind(
  BrCan.clean[,1],
  BrCanNorm
)
names(BrCanNorm)[1] <- "diagnosis"

summary(BrCanNorm)

#Getting a look at the distribution
table(BrCanNorm$diagnosis)

# Let's check the structure of this dataset.
str(BrCanNorm)

##########################
## Lets Perform EDA
##########################

select(BrCanNorm$diagnosis, BrCanNorm$area_mean) %>%
  mutate(BrCanNorm$diagnosis = ifelse(BrCanNorm$diagnosis == "M", "Cancerous", "Not Cancerous")) %>%
  as_data_frame()
BrCanNorm %>% 
  
head()


# Let's encode the response variable into a factor variable of M = 1's (Malignant) and B = 0's (Benign).
BrCanNorm$diagnosis <- ifelse(BrCanNorm$diagnosis == "M", "Cancerous", "Not Cancerous")
BrCanNorm$diagnosis <- factor(BrCanNorm$diagnosis, levels = c("Cancerous", "Not Cancerous"))

P1 <- BrCanNorm %>%
 ggplot(aes(x = BrCanNorm$diagnosis, 
             y = BrCanNorm$area_mean, 
             color = BrCanNorm$diagnosis,
             fill = BrCanNorm$diagnosis)) +
  geom_boxplot(alpha=.25) + 
  stat_compare_means(inherit.aes = TRUE, 
                     label = "p.format", 
                     method = "t.test", 
                     label.x = 1.5,
                     show.legend=FALSE) +
  theme_bw() +
  labs(x = "Diagnosis",
       y = "Area Mean",
       title = "Mean Area of Cell")

P2 <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$area_mean,
                  color = BrCanNorm$diagnosis,
                  fill = BrCanNorm$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Area Mean",
       y = "Density",
       title = "Area Vs Density of Cell")

install.packages("gridExtra")
library("gridExtra")
grid.arrange(P1, P2, ncol=2)


# Let's review Class Imbalnces.
table(BrCanNorm$diagnosis)


#############################
### Make Codebook
############################
#setwd("Codebook")
#makeDataReport(BrCan, replace=FALSE)
#setwd("..")





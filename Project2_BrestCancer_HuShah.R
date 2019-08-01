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
#############################
### Make Codebook
############################
#setwd("Codebook")
#makeDataReport(BrCan, replace=FALSE)
#setwd("..")

##################################################################################################################
# Data Summary
#a) radius (mean of distances from center to points on the perimeter)
#b) texture (standard deviation of gray-scale values)
#c) perimeter
#d) area
#e) smoothness (local variation in radius lengths)
#f) compactness (perimeter^2 / area - 1.0)
#g) concavity (severity of concave portions of the contour)
#h) concave points (number of concave portions of the contour)
#i) symmetry 
#j) fractal dimension ("coastline approximation" - 1)

##################################################################################################################
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
# Let's check the missign values if any.

#############################
#OBJECTIVE 1 - EDA Analysis
## Lets Perform EDA
#############################

# Let's encode the response variable into a factor variable of M = 1's (Malignant) and B = 0's (Benign).
BrCanNorm$diagnosis <- ifelse(BrCanNorm$diagnosis == "M", "Cancerous", "Not Cancerous")
BrCanNorm$diagnosis <- factor(BrCanNorm$diagnosis, levels = c("Cancerous", "Not Cancerous"))
head(BrCanNorm)

### Box Plots : Diagnosis Vs 1. Area 2. Radius 3. Concavity
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
P1 <- P1 + theme(legend.position="top")
###################################################
P2 <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$diagnosis, 
             y = BrCanNorm$radius_mean, 
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
       y = "Radius Mean",
       title = "Mean Radius of Cell")
P2 <- P2 + theme(legend.position="top")
#P2
##################################################
P3 <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$diagnosis, 
             y = BrCanNorm$concavity_mean, 
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
       y = "Concavity Mean",
       title = "Mean Concavity of Cell")

P3 <- P3 + theme(legend.position="top")
#P3
##################################################
install.packages("gridExtra")
library("gridExtra")
grid.arrange(P1, P2, P3, ncol=3) #### ---------------------># plot to use in report
##################################################


### Density Plots : Diagnosis Vs 1. Area 2. Radius 3. Concavity
Pa <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$area_mean,
                  color = BrCanNorm$diagnosis,
                  fill = BrCanNorm$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Area Mean",
       y = "Density",
       title = "Cell Area Density")
Pa <- Pa + theme(legend.position = c(0.7, 0.65))
Pa
##################################################
Pr <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$radius_mean,
             color = BrCanNorm$diagnosis,
             fill = BrCanNorm$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Radius Mean",
       y = "Density",
       title = "Cell Radius Density")
Pr <- Pr + theme(legend.position = c(0.7, 0.65))
Pr
##################################################
Pc <- BrCanNorm %>%
  ggplot(aes(x = BrCanNorm$concavity_mean,
             color = BrCanNorm$diagnosis,
             fill = BrCanNorm$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Concavity Mean",
       y = "Density",
       title = "Cell Concavity Density")
Pc <- Pc + theme(legend.position = c(0.7, 0.65))
Pc
##################################################
install.packages("gridExtra")
library("gridExtra")
grid.arrange(Pa, Pr, Pc, ncol=3) #### ---------------------># plot to use in report
##################################################

##################################
head(BrCanNorm)
dim(BrCanNorm)
str(BrCanNorm)

#################################

#### Histograms ################

pR <- ggplot(BrCanNorm, aes(x=BrCanNorm$radius_mean, color=BrCanNorm$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Radius",
       y = "Count",
       title = "Radius")
pR<-pR + theme(legend.position = c(0.7, 0.65))
##################################################
pA <- ggplot(BrCanNorm, aes(x=BrCanNorm$area_mean, color=BrCanNorm$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Area",
       y = "Count",
       title = "Area")
pA<-pA + theme(legend.position = c(0.7, 0.65))
##################################################
pC <- ggplot(BrCanNorm, aes(x=BrCanNorm$concavity_mean, color=BrCanNorm$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Concavity",
       y = "Count",
       title = "Concavity")
pC<-pC + theme(legend.position = c(0.7, 0.65))
##################################################
#install.packages("gridExtra")
#library("gridExtra")
grid.arrange(pR, pA, pC, ncol=3) #### ---------------------># plot to use in report
##################################################


p1<-ggplot(data=BrCanNorm,aes(x=BrCanNorm[,2])) + geom_histogram(bins = 50) + xlab("Mean Radius")
p1 + geom_bar()
##################################################


pX <- ggplot(data = BrCanNorm,
            mapping = aes(x = BrCanNorm$diagnosis, y = BrCanNorm$area_mean, fill = BrCanNorm$diagnosis, xlab = "Diagnosis", ylab = "Mean Area"))
pX<- pX + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Area", y = "Diagnosis", title = "Mean Cell Area by Diagnosis")
##################################################
pY <- ggplot(data = BrCanNorm,
             mapping = aes(x = BrCanNorm$diagnosis, y = BrCanNorm$radius_mean, fill = BrCanNorm$diagnosis, xlab = "Diagnosis", ylab = "Mean Radius"))
pY<-pY + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Radius", y = "Diagnosis", title = "Mean Cell Radius by Diagnosis")
##################################################
pZ <- ggplot(data = BrCanNorm,
             mapping = aes(x = BrCanNorm$diagnosis, y = BrCanNorm$concavity_mean, fill = BrCanNorm$diagnosis, xlab = "Diagnosis", ylab = "Mean Concavity"))
pZ<-pZ + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Concavity", y = "Diagnosis", title = "Mean Cell Concavity by Diagnosis")
##################################################
#install.packages("gridExtra")
#library("gridExtra")
grid.arrange(pX, pY, pZ, ncol=3) #### ---------------------># plot to use in report
##################################################


# Area, Radius and Concavity
pt<-ggplot(BrCanNorm, aes(x=BrCanNorm$area_mean, y=BrCanNorm$perimeter_mean, color=BrCanNorm$diagnosis, size=BrCanNorm$texture_mean)) + 
  geom_point(alpha=0.6)
pt
####################################
pr<-ggplot(BrCanNorm, aes(x=BrCanNorm$radius_mean, y=BrCanNorm$perimeter_mean, color=BrCanNorm$diagnosis, size=BrCanNorm$area_mean)) + 
  geom_point(alpha=0.6)
pr
####################################
pc<-ggplot(BrCanNorm, aes(x=BrCanNorm$radius_mean, y=BrCanNorm$area_mean, color=BrCanNorm$diagnosis, size=BrCanNorm$concavity_mean)) + 
  geom_point(alpha=0.6)
pc
####################################
grid.arrange(pt, pr, pc, ncol=3) #### ---------------------># plot to use in report
##################################################


#### Scatterplot Matrix ################

# pair-wise scatterplots colored by class
pairs(diagnosis~., data=BrCanNorm, col=BrCanNorm$diagnosis)

install.packages("WVPlots")
library(WVPlots) 

PairPlot(BrCanNorm, 
         colnames(BrCanNorm)[1:10], 
         "Breast Cancer Data by Diagnosis", 
         group_var = "diagnosis")
########################################################

#### Correlation Matrix ################
install.packages("corrplot")
library(corrplot)

BrCanCor <- cor(BrCanNorm[,2:31])
corrplot(BrCanCor, order = "hclust", tl.cex = 0.7) #### ---------------------># plot to use in report
#### Identify Highly Correlated Featuers  ################
HiBrCanCor <- colnames(BrCanNorm)[findCorrelation(BrCanCor, cutoff = 0.9, verbose = TRUE)]
HiBrCanCor
#### Removing Highly Correlated Featuers  ################
LoBrCanCor <- BrCanNorm[, which(!colnames(BrCanNorm) %in% HiBrCanCor)]
ncol(LoBrCanCor)
dim(LoBrCanCor)
########################################################

#### Principal Components Analysis ################
BrCanPCA <- prcomp(BrCanNorm[, 2:31], center=TRUE, scale=TRUE)
p<-plot(BrCanPCA, type="l", main='', col ="red", lty=1)
p+grid(nx = 10, ny = 14, col = "blue")
p+title(main = "Principal Components Analysis - Full", sub = NULL, xlab = "Components")
box(col="blue")                                   #### ---------------------># plot to use in report

summary(BrCanPCA)

#### PCA Proportion of Variance ################

VBrCanPCA <-BrCanPCA$sdev^2
PVBrCanEr <- VBrCanPCA/sum(VBrCanPCA)
PVBrCum <- cumsum(PVBrCanEr)
PVBrEr <- tibble(comp = seq(1:ncol(BrCanNorm %>% select(-diagnosis))), PVBrCanEr, PVBrCum)

ggplot(PVBrEr, aes(x=comp,y=PVBrCum)) +
  geom_point(color="orange", size = 3)+labs(x = "Components",
                                y = "Cumulative Proportion of Variance",
                                title = "Prinipal Variance Components")+
  geom_abline(intercept = 0.95, color= "blue", slope = 0)   #### ---------------------># plot to use in report
########################################################

#### Principal Components Analysis - Correlation ################
LoBrCanPCA <- prcomp(LoBrCanCor, center=TRUE, scale=TRUE)
summary(LoBrCanPCA)

p<-plot(LoBrCanPCA, type="l", main='', col ="red", lty=1)
p+grid(nx = 10, ny = 14, col = "blue")
p+title(main = "Principal Components Analysis - Correlation", sub = NULL, xlab = "Components")
box(col="blue")                                   #### ---------------------># plot to use in report

#### PCA Proportion of Variance ################

VBrCanPCA1 <-LoBrCanPCA$sdev^2
PVBrCanEr1 <- VBrCanPCA1/sum(VBrCanPCA1)
PVBrCum1 <- cumsum(PVBrCanEr1)
PVBrEr1 <- tibble(comp = seq(1:ncol(LoBrCanCor)), PVBrCanEr1, PVBrCum1)

ggplot(PVBrEr1, aes(x=comp,y=PVBrCum1)) +
  geom_point(color="orange", size = 3)+labs(x = "Components",
                                            y = "Cumulative Proportion of Variance",
                                            title = "Prinipal Variance Components - Correlation")+
  geom_abline(intercept = 0.95, color= "blue", slope = 0)   #### ---------------------># plot to use in report
########################################################
############ PCA Full Vs Correlation ##################

PCAfc <- as.data.frame(LoBrCanPCA$x)
ggplot(PCAfc, aes(x=PC1, y=PC2, col=BrCanNorm$diagnosis)) + geom_point(alpha=0.5)  #### ---------------------># plot to use in report

############ Most ifluential variables of PC1 & PC2 ####
autoplot(LoBrCanPCA, data = BrCanNorm,  colour = 'diagnosis',
         loadings = FALSE, loadings.label = TRUE, loadings.colour = "purple",  loadings.label.colour = "black")   #### ---------------------># plot to use in report
############ Seperate Diagnosis classes and Variance ####
BrCanPCS <- cbind(tibble::enframe(BrCanNorm$diagnosis), as_tibble(LoBrCanPCA$x))
GGally::ggpairs(BrCanPCS, columns = 2:4, ggplot2::aes(color = value))  #### ---------------------># plot to use in report

# Let's review Class Imbalnces.
table(BrCanNorm$diagnosis)

#############################
#OBJECTIVE 1 - Model
### Model Building #########
############################

#### Hypothesis Test & Confidence Interval ####

head(BrCanNorm)
BrCanNorm <- BrCanNorm %>% mutate(diagnosis = ifelse(diagnosis == "Cancerous", 1, 0))

#Splitting our data into 70% training and 30% testing data sets
library(caret)
training <- createDataPartition(BrCanNorm$diagnosis, p=0.7, list=FALSE)
BrCantrain <- BrCanNorm[ training, ]
BrCantest <- BrCanNorm[ -training, ]
#head(BrCantrain)

#fitting our model
BrCangLogi <- glm(diagnosis ~ area_mean, data = train, family = "binomial")
summary(BrCangLogi)

BrCangLogi <- glm(diagnosis ~ , data = train, family = "binomial")
summary(BrCangLogi)


fit.logit <- glm(diagnosis~., data=BrCantrain, family = binomial())
summary(fit.logit)











#############################
#OBJECTIVE 2 - Model
### Model Building #########
############################

############ Splitting dataset 70/30 training/test ############
set.seed(1234)
BC <- cbind(diagnosis = BrCanNorm$diagnosis, LoBrCanCor)
train_indx <- createDataPartition(BC$diagnosis, p = 0.7, list = FALSE)

train_set <- BC[train_indx,]
test_set <- BC[-train_indx,]

nrow(train_set)
nrow(test_set)



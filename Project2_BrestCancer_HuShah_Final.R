# MSDS 6372 Summer 2019: Dr. Jacob Turner
# Project - 2: Logistic Linear Regression
# Team Members - Dhyan Shah, Chaushun Hu, Eric Balke
# Southern Methodist University - Masters of Science in Data Science - Class of 2020
# July 19, 2019 rev.4: Aug 16, 2019

# Include Libraries
suppressWarnings(suppressMessages(library(tidyverse))) #required
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggcorrplot))
suppressPackageStartupMessages(library(kernlab))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(gridExtra))

#install.packages("WVPlots")
suppressPackageStartupMessages(library(WVPlots))
suppressPackageStartupMessages(library(glmnet))#required
suppressPackageStartupMessages(library(car)) #required

###################################
### RAW DATA IMPORT ###
###################################

BrCan<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=F,sep=",")

#write.csv(BrCan, '/Users/chux/Desktop/BrCan.csv', row.names=T)
#BrCan<-read.csv(file='/Users/chux/Desktop/BrCan.csv', header=T, sep=',')
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

#Data Summary
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

# Clean Data
BrCan$outcome<-ifelse(BrCan$diagnosis == 'M',1,0)
BrCan$outcome<-as.factor(BrCan$outcome)
#remove id, diagnosis and X columnn
BrCanClean <- BrCan[,c(-1,-2)]
glimpse(BrCanClean)

names(BrCanClean)[31] <- "diagnosis"

summary(BrCanClean)

#Getting a look at the distribution
table(BrCanClean$diagnosis)

# Let's check the structure of this dataset.
str(BrCanClean)

#Getting a look at the distribution
table(BrCanClean$diagnosis)
plot(BrCanClean$diagnosis)
Cancer <- c(357,212)
Lbls <- c("Benign", "Malignant")
pie(Cancer, labels = Lbls, main="Breast Cancer Class Distribution") #### ---------------------># plot to use in repor

#################################################################################################################
#################################      OBJECTIVE 1 - EDA Analysis      ##########################################
#################################################################################################################

## Lets Perform EDA
# Let's encode the response variable into a factor variable of M = 

### Box Plots : Diagnosis Vs 1. Area 2. Radius 3. Concavity
P1 <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$diagnosis, 
             y = BrCanClean$area_mean, 
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
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
P1 <- P1 + ggplot2::theme(legend.position="top")

P2 <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$diagnosis, 
             y = BrCanClean$radius_mean, 
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
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
P2 <- P2 + ggplot2::theme(legend.position="top")

P3 <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$diagnosis, 
             y = BrCanClean$concavity_mean, 
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
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

P3 <- P3 + ggplot2::theme(legend.position="top")

#install.packages("gridExtra")
grid.arrange(P1, P2, P3, ncol=3) #### ---------------------># plot to use in report

### Density Plots : Diagnosis Vs 1. Area 2. Radius 3. Concavity
Pa <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$area_mean,
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Area Mean",
       y = "Density",
       title = "Cell Area Density")
Pa <- Pa + ggplot2::theme(legend.position = c(0.7, 0.65))
#Pa
Pr <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$radius_mean,
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Radius Mean",
       y = "Density",
       title = "Cell Radius Density")
Pr <- Pr + ggplot2::theme(legend.position = c(0.7, 0.65))
#Pr
Pc <- BrCanClean %>%
  ggplot(aes(x = BrCanClean$concavity_mean,
             color = BrCanClean$diagnosis,
             fill = BrCanClean$diagnosis)) +
  geom_density(alpha=.25) +
  theme_bw() +
  labs(x = "Concavity Mean",
       y = "Density",
       title = "Cell Concavity Density")
Pc <- Pc + ggplot2::theme(legend.position = c(0.7, 0.65))
#Pc
grid.arrange(Pa, Pr, Pc, ncol=3)#### ---------------------># plot to use in report

head(BrCanClean)
dim(BrCanClean)
str(BrCanClean)

#### Histograms ################
pR <- ggplot(BrCanClean, aes(x=BrCanClean$radius_mean, color=BrCanClean$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Radius",
       y = "Count",
       title = "Radius")
pR<-pR + ggplot2::theme(legend.position = c(0.7, 0.65))
##################################################
pA <- ggplot(BrCanClean, aes(x=BrCanClean$area_mean, color=BrCanClean$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Area",
       y = "Count",
       title = "Area")
pA<-pA + ggplot2::theme(legend.position = c(0.7, 0.65))
##################################################
pC <- ggplot(BrCanClean, aes(x=BrCanClean$concavity_mean, color=BrCanClean$diagnosis)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  labs(x = "Concavity",
       y = "Count",
       title = "Concavity")
pC<-pC + ggplot2::theme(legend.position = c(0.7, 0.65))

grid.arrange(pR, pA, pC, ncol=3) #### ---------------------># plot to use in report

p1<-ggplot(data=BrCanClean,aes(x=BrCanClean[,2])) + geom_histogram(bins = 50) + xlab("Mean Radius")
p1 + geom_bar()

# Mean Area, Radius and Concavity vs Diagnosis
pX <- ggplot(data = BrCanClean,
             mapping = aes(x = BrCanClean$diagnosis, y = BrCanClean$area_mean, fill = BrCanClean$diagnosis, xlab = "Diagnosis", ylab = "Mean Area"))
pX<- pX + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Area", y = "Diagnosis", title = "Mean Cell Area by Diagnosis")
##################################################
pY <- ggplot(data = BrCanClean,
             mapping = aes(x = BrCanClean$diagnosis, y = BrCanClean$radius_mean, fill = BrCanClean$diagnosis, xlab = "Diagnosis", ylab = "Mean Radius"))
pY<-pY + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Radius", y = "Diagnosis", title = "Mean Cell Radius by Diagnosis")
##################################################
pZ <- ggplot(data = BrCanClean,
             mapping = aes(x = BrCanClean$diagnosis, y = BrCanClean$concavity_mean, fill = BrCanClean$diagnosis, xlab = "Diagnosis", ylab = "Mean Concavity"))
pZ<-pZ + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")+ coord_flip () +  labs(x = "Mean Concavity", y = "Diagnosis", title = "Mean Cell Concavity by Diagnosis")
##################################################
#install.packages("gridExtra")
#library("gridExtra")
grid.arrange(pX, pY, pZ, ncol=3) #### ---------------------># plot to use in report

# Area, Radius and Concavity
pt<-ggplot(BrCanClean, aes(x=BrCanClean$area_mean, y=BrCanClean$perimeter_mean, color=BrCanClean$diagnosis, size=BrCanClean$texture_mean)) + 
  geom_point(alpha=0.6)
#pt
####################################
pr<-ggplot(BrCanClean, aes(x=BrCanClean$radius_mean, y=BrCanClean$perimeter_mean, color=BrCanClean$diagnosis, size=BrCanClean$area_mean)) + 
  geom_point(alpha=0.6)
#pr
####################################
pc<-ggplot(BrCanClean, aes(x=BrCanClean$radius_mean, y=BrCanClean$area_mean, color=BrCanClean$diagnosis, size=BrCanClean$concavity_mean)) + 
  geom_point(alpha=0.6)
#pc
####################################
grid.arrange(pt, pr, pc, ncol=3) #### ---------------------># plot to use in report

#### Scatterplot Matrix ################

plot(BrCanClean[c(1:9,31)], col=BrCanClean$diagnosis)

#### Correlation Matrix ################

#install.packages("corrplot")
library(corrplot)
BrCanClean.numeric<-BrCanClean[sapply(BrCanClean, is.numeric)]
BrCanCor <- cor(BrCanClean.numeric)

corrplot(BrCanCor, order = "hclust", tl.cex = 0.7) #### ---------------------># plot to use in report

#Get all the correlated variables where correlation coefficients >=0.7
highCorr<-findCorrelation(BrCanCor, cutoff=0.7, verbose=T, names=T)
highCorr.col<-colnames(BrCanClean.numeric[highCorr])
highCorr.col

#remove the correlated variables
BrCanData<-BrCanClean[,-which(colnames(BrCanClean) %in% highCorr.col)]
glimpse(BrCanData)

#Compare row 7  and column  8 with corr  0.921 
#  Means:  0.571 vs 0.389 so flagging column 7 
#Compare row 8  and column  6 with corr  0.831 
#  Means:  0.542 vs 0.377 so flagging column 8 
#Compare row 6  and column  28 with corr  0.816 
#  Means:  0.524 vs 0.365 so flagging column 6 
#Compare row 28  and column  27 with corr  0.855 
#  Means:  0.507 vs 0.354 so flagging column 28 
#Compare row 27  and column  26 with corr  0.892 
#  Means:  0.457 vs 0.343 so flagging column 27 
#Compare row 23  and column  21 with corr  0.994 
#  Means:  0.456 vs 0.333 so flagging column 23 
#Compare row 21  and column  3 with corr  0.969 
#  Means:  0.422 vs 0.324 so flagging column 21 
#Compare row 3  and column  24 with corr  0.942 
#  Means:  0.384 vs 0.316 so flagging column 3 
#Compare row 26  and column  30 with corr  0.81 
#  Means:  0.4 vs 0.313 so flagging column 26 
#Compare row 24  and column  1 with corr  0.941 
#  Means:  0.356 vs 0.302 so flagging column 24 
#Compare row 1  and column  4 with corr  0.987 
#  Means:  0.308 vs 0.298 so flagging column 1 
#Compare row 4  and column  13 with corr  0.727 
#  Means:  0.27 vs 0.294 so flagging column 13 
#Compare row 4  and column  11 with corr  0.733 
#  Means:  0.244 vs 0.29 so flagging column 11 
#Compare row 4  and column  14 with corr  0.8 
#  Means:  0.213 vs 0.294 so flagging column 14 
#Compare row 18  and column  16 with corr  0.744 
#  Means:  0.36 vs 0.292 so flagging column 18 
#Compare row 16  and column  17 with corr  0.801 
#  Means:  0.394 vs 0.288 so flagging column 16 
#Compare row 17  and column  20 with corr  0.727 
#  Means:  0.292 vs 0.272 so flagging column 17 
#Compare row 5  and column  25 with corr  0.805 
#  Means:  0.33 vs 0.268 so flagging column 5 
#Compare row 10  and column  30 with corr  0.767 
#  Means:  0.372 vs 0.256 so flagging column 10 
#Compare row 22  and column  2 with corr  0.912 
#  Means:  0.253 vs 0.243 so flagging column 22 

#All correlations <=0.7 are as follows

#Observations: 569
#Variables: 11
#$ texture_mean            <dbl> 10.38, 17.77, 21.25, 20.38, 14.34, 15.70, 19.98, 20.83, 21.82, 24.04, …
#$ area_mean               <dbl> 1001.0, 1326.0, 1203.0, 386.1, 1297.0, 477.1, 1040.0, 577.9, 519.8, 47…
#$ symmetry_mean           <dbl> 0.2419, 0.1812, 0.2069, 0.2597, 0.1809, 0.2087, 0.1794, 0.2196, 0.2350…
#$ texture_se              <dbl> 0.9053, 0.7339, 0.7869, 1.1560, 0.7813, 0.8902, 0.7732, 1.3770, 1.0020…
#$ smoothness_se           <dbl> 0.006399, 0.005225, 0.006150, 0.009110, 0.011490, 0.007510, 0.004314, …
#$ symmetry_se             <dbl> 0.03003, 0.01389, 0.02250, 0.05963, 0.01756, 0.02165, 0.01369, 0.01486…
#$ fractal_dimension_se    <dbl> 0.006193, 0.003532, 0.004571, 0.009208, 0.005115, 0.005082, 0.002179, …
#$ smoothness_worst        <dbl> 0.1622, 0.1238, 0.1444, 0.2098, 0.1374, 0.1791, 0.1442, 0.1654, 0.1703…
#$ symmetry_worst          <dbl> 0.4601, 0.2750, 0.3613, 0.6638, 0.2364, 0.3985, 0.3063, 0.3196, 0.4378…
#$ fractal_dimension_worst <dbl> 0.11890, 0.08902, 0.08758, 0.17300, 0.07678, 0.12440, 0.08368, 0.11510…
#$ diagnosis               <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1…

#### Principal Components Analysis ################

BrCanPCA <- prcomp(BrCanClean[,1:30], center=TRUE, scale=TRUE)
p<-plot(BrCanPCA, type="l", main='', col ="red", lty=1)
p+grid(nx = 10, ny = 14, col = "blue")
p+title(main = "Principal Components Analysis - Full", sub = NULL, xlab = "Components")
box(col="blue")                                             #### ---------------------># plot to use in report

summary(BrCanPCA)

#### PCA Proportion of Variance ################

VBrCanPCA <-BrCanPCA$sdev^2
PVBrCanEr <- VBrCanPCA/sum(VBrCanPCA)
PVBrCum <- cumsum(PVBrCanEr)
PVBrEr <- tibble(comp = seq(1:ncol(BrCanClean %>% select(-diagnosis))), PVBrCanEr, PVBrCum)

ggplot(PVBrEr, aes(x=comp,y=PVBrCum)) +
  geom_point(color="orange", size = 3)+labs(x = "Components",
                                            y = "Cumulative Proportion of Variance",
                                            title = "Prinipal Variance Components")+
  geom_abline(intercept = 0.95, color= "blue", slope = 0)   #### ---------------------># plot to use in report

#### Principal Components Analysis - Correlation ################

LoBrCanPCA <- prcomp(BrCanData[,1:10], center=TRUE, scale=TRUE)
summary(LoBrCanPCA)

p<-plot(LoBrCanPCA, type="l", main='', col ="red", lty=1)
p+grid(nx = 10, ny = 14, col = "blue")
p+title(main = "Principal Components Analysis - Transformed", sub = NULL, xlab = "Components")
box(col="blue")                                              #### ---------------------># plot to use in report

#### PCA Proportion of Variance ################

VBrCanPCA1 <-LoBrCanPCA$sdev^2
PVBrCanEr1 <- VBrCanPCA1/sum(VBrCanPCA1)
PVBrCum1 <- cumsum(PVBrCanEr1)
PVBrEr1 <- tibble(comp = seq(1:ncol(BrCanData %>% select(-diagnosis))), PVBrCanEr1, PVBrCum1)

ggplot(PVBrEr1, aes(x=comp,y=PVBrCum1)) +
  geom_point(color="orange", size = 3)+labs(x = "Components",
                                            y = "Cumulative Proportion of Variance",
                                            title = "Prinipal Variance Components - Correlation")+
  geom_abline(intercept = 0.95, color= "blue", slope = 0)     #### ---------------------># plot to use in report

############ PCA Full Vs Correlation ##################

PCAfc <- as.data.frame(LoBrCanPCA$x)
ggplot(PCAfc, aes(x=PC1, y=PC2, col=BrCanClean$diagnosis)) + geom_point(alpha=0.5) 

############ Most ifluential variables of PC1 & PC2 ####
autoplot(LoBrCanPCA, data = BrCanData,  colour = 'diagnosis',
         loadings = FALSE, loadings.label = TRUE, loadings.colour = "purple",  loadings.label.colour = "black")   #### ---------------------># plot to use in report
############ Seperate Diagnosis classes and Variance ####
BrCanPCS <- cbind(tibble::enframe(BrCanData$diagnosis), as_tibble(LoBrCanPCA$x))
GGally::ggpairs(BrCanPCS, columns = 2:4, ggplot2::aes(color = value))  #### ---------------------># plot to use in report

# Let's review Class Imbalnces.
table(BrCanData$diagnosis)

#################################################################################################################
#################################     OBJECTIVE 1 - Model Building     ##########################################
#################################################################################################################

# Function for plotting confusion matrices
cm_plot <- function(ml, title) {
  confusionMatrix(ml)$table %>%
    round(1) %>%
    fourfoldplot(
      color = c("#CC6666", "#99CC99"),
      main=title, 
      conf.level=0, 
      margin=1
    )
}

library(factoextra)
summary(BrCanPCA)
fviz_eig(BrCanPCA, addlabels = TRUE, ylim = c(0,100), barfill = "steelblue1", line="navy") + 
  theme_classic() +
  labs(x = "Principal Components", y = "% of Explained Variance", title = "BrCanPCA - Principal Components")

### We see that 44.3% of the variance is explained by the first principal component.
### The two first components explains the 0.6324 of the variance. We need 10 principal 
### components to explain more than 0.95% of the variance and 17 to explain more than 0.99

library(factoextra)
summary(LoBrCanPCA)
fviz_eig(LoBrCanPCA, addlabels = TRUE, ylim = c(0,100), barfill = "steelblue1", line="navy") + 
  theme_classic() +
  labs(x = "Principal Components", y = "% of Explained Variance", title = "LoBrCanPCA - Principal Components")

### After removing high correlation term (>=0.7), we get the 10 features.
### hypothesis testing for texture mean

set.seed(1234)

train_indx <- createDataPartition(BrCanData$diagnosis, p = 0.7, list = FALSE)

train_set <- BrCanData[train_indx,]
test_set <- BrCanData[-train_indx,]
testValues <-BrCanData$diagnosis.factor
nrow(train_set)
nrow(test_set)

#### ############################
#### Graph - GLM Model ####
#### ############################
BrCanModel <- glm(diagnosis ~ area_mean, data=train_set, family = "binomial")
summary(BrCanModel)

######  H0:  β1=0 -> Breast Cancer cell mean area = 0
######  Ha:  β1≠0 -> Breast Cancer cell mean area ≠ 0
###### ##########################

plot(diagnosis ~ area_mean, data=train_set, main="Breast Cancer Regression Curve", ylab="Probability of Cancerous Tumor",
     pch=16)
curve(
  exp(BrCanModel$coef[1]+BrCanModel$coef[2]*x)/
    (1+exp(BrCanModel$coef[1]+BrCanModel$coef[2]*x)),
  add=TRUE
) 

##### ###########################################################
#### Check - Goodness of Fit - Hosemer & Lemeshow ####
##### ###########################################################

#install.packages("ResourceSelection")
#install.packages("pander")
library(ResourceSelection)
#library(pander)
hoslem.test(BrCanModel$y, BrCanModel$fitted.values)
#pander()

### After getting rid of highly correlated features, we see that 31.5% of the variance is 
### explained by the first principal component.

#Function to print out the plots for the  linear regression
mlrplots <- function(fit, hidenum = TRUE)
{
  #library(MASS)
  sres <- rstudent(fit)
  res <- resid(fit)
  leverage <- hatvalues(fit)
  
  par(mfrow=c(2,3))
  
  #Plot residuals
  plot(fitted(fit), res, xlab = "Fitted", ylab = "Residuals")
  abline(h=0, col="blue", lty=2)  
  
  #Plot studentized residuals
  plot(fitted(fit), sres, xlab = "Fitted", ylab = "StudResiduals")
  abline(h=-2, col="blue", lty=2)
  abline(h=2, col="blue", lty=2)
  if(!hidenum)
    text(sres~fitted(fit), y=sres, labels=ifelse( abs(sres) >= 2, names(sres),""), col="red")  
  
  #Plot Leverage - examine any observations ~2-3 times greater than the average hat value
  plot(x = leverage, y = sres, xlab = "Leverage", ylab = "StudResiduals")
  abline(h=-2, col="blue", lty=2)
  abline(h=2, col="blue", lty=2)
  abline(v = mean(leverage)*2, col="blue", lty=2) #line is at 2x mean
  
  #QQ Plot
  qqnorm(sres, xlab="Quantile", ylab="Residual", main = NULL) 
  qqline(sres, col = 2, lwd = 2, lty = 2) 
  
  #Cooks D
  cooksd <- cooks.distance(fit)
  sample_size <- length(fit$model[,1])
  plot(cooksd, xlab = "Observation", ylab = "Cooks D", col = c("blue"))
  abline(h = 4/sample_size, col="red")  # add cutoff line
  if(!hidenum)
    text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
  
  #Histogram of residuals with normal curve
  #If the curve looks wrong try using the studentized residuals
  hist(res, freq=FALSE, xlab = "Residuals", main = NULL)
  curve(dnorm(x, mean=mean(res), sd=sd(res)), add=TRUE, col = "blue")
}

logicFit <- glm(diagnosis ~ . , data=train_set, family = binomial(link = "logit") , control = list(maxit = 50))
summary(logicFit)
varImp(logicFit)

confint(logicFit)

# VIF for covariance between texture, area, symmetry, smoothness, fractal_dimension
vif(logicFit) -> logicFit.vif
logicFit.vif

#Anova test
print(anova(logicFit, test="Chisq"))

#Plots for the model
mlrplots(logicFit)

### Apply the algorithm to the training sample

predTrain <- predict(logicFit, train_set, type="response")
predTrain <- ifelse(predTrain>0.5,1,0)
error = mean(predTrain != train_set$diagnosis)
print(paste('Model Accuracy', 1-error))

##"Model Accuracy 0.949874686716792"

### Calculate the ROC Curve and the AUC
#install.packages("ROCR")
library(ROCR)
p <- predict(logicFit, train_set, type="response")
pr <- prediction(p, train_set$diagnosis)
prf = performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

### logistic Regression with PCA dataset
BrCanPCAFull<-LoBrCanPCA$x
BrCanPCAFull<-data.frame(BrCanPCAFull)
BrCanPCAFull$diagnosis<-BrCanData$diagnosis
train_set_pca <- BrCanPCAFull[train_indx,]
test_set_pca <- BrCanPCAFull[-train_indx,]
testValues_pca <-BrCanPCAFull$diagnosis.factor
nrow(train_set_pca)
nrow(test_set_pca)

logicFitPCA <- glm(diagnosis ~ . , data=train_set_pca, family = binomial(link = "logit") , control = list(maxit = 50))
summary(logicFitPCA)

confint(logicFitPCA)

# VIF for covariance between texture, area, symmetry, smoothness, fractal_dimension
vif(logicFitPCA) -> logicFitPCA.vif
logicFitPCA.vif

#Plots for the model
mlrplots(logicFitPCA)

### prediction using Elastic net

library(glmnetUtils)
x <- model.matrix(diagnosis~.,BrCanData)[,-1]
#y <- BrCan$diagnosis

y <- as.factor(as.character(BrCanData$diagnosis))

# Fit models 
#fit.lasso <- glmnet(x, y, family="gaussian", alpha=1) #fit LASSO
logicFitNet <- glmnet(x, y, family = "binomial",  alpha=.5) #fit Elastic Net

cv <- cv.glmnet(x,y, type.measure = "class", alpha = 0.5, family = "binomial")


plot(cv, main="Elastic Net")

### prediction using Elastic net for PCA transformed dataset

#library(glmnetUtils)
x <- model.matrix(diagnosis~.,BrCanPCAFull)[,-1]
#y <- BrCan$diagnosis

y <- as.factor(as.character(BrCanPCAFull$diagnosis))

# Fit models 
#fit.lasso <- glmnet(x, y, family="gaussian", alpha=1) #fit LASSO
logicFitNet <- glmnet(x, y, family = "binomial",  alpha=.5) #fit Elastic Net

cv <- cv.glmnet(x,y, type.measure = "class", alpha = 0.5, family = "binomial")


plot(cv, main="Elastic Net using PCA transformed Data")

#################################################################################################################
#################################     OBJECTIVE 2 - Model Building     ##########################################
#################################################################################################################

############ Setting up 10-fold cross-validation ####
ctrl <- trainControl("cv",number=10)

### ################################
###  Logistic regression        ###
### ################################
set.seed(1234)
logit.ml<-train(diagnosis ~ ., data = train_set_pca, method="glm")
logit.cm <- confusionMatrix(logit.ml)
summary(logit.ml)

cm_plot(logit.ml, "Logistic Regression")                         #### ---------------------># plot to use in report
logit.metrics <- data.frame (
  "Model" = "Logistic Regression",
  "Accuracy" = (logit.cm$table[1,1] + logit.cm$table[2,2])/100,
  "Recall" = logit.cm$table[2,2] / (logit.cm$table[2,2] + logit.cm$table[1,2]),
  "Precision" = logit.cm$table[2,2] / (logit.cm$table[2,1] + logit.cm$table[2,2]),
  "FNR" = (logit.cm$table[1,2] / (logit.cm$table[2,2] + logit.cm$table[1,2])),
  "Fscore" = (2 * logit.cm$table[2,2]) / (2 * logit.cm$table[2,2] + logit.cm$table[1,2] + logit.cm$table[2,1])
)
logit.metrics

### ################################
###   Bagging - Random Forest   ###
### ################################

rfPCA.ml<-train(diagnosis ~ ., data = train_set_pca, method = "rf", trControl=ctrl, importance=FALSE)
rfPCA.cm <- confusionMatrix(rfPCA.ml)
cm_plot(rfPCA.ml, "Random Forest with PCA")  

#### ---------------------># plot to use in report
rfPCA.metrics <- data.frame (
  "Model" = "Random Forest with PCA",
  "Accuracy" = (rfPCA.cm$table[1,1] + rfPCA.cm$table[2,2])/100,
  "Recall" = rfPCA.cm$table[2,2] / (rfPCA.cm$table[2,2] + rfPCA.cm$table[1,2]),
  "Precision" = rfPCA.cm$table[2,2] / (rfPCA.cm$table[2,1] + rfPCA.cm$table[2,2]),
  "FNR" = (rfPCA.cm$table[1,2] / (rfPCA.cm$table[2,2] + rfPCA.cm$table[1,2])),
  "Fscore" = (2 * rfPCA.cm$table[2,2]) / (2 * rfPCA.cm$table[2,2] + rfPCA.cm$table[1,2] + rfPCA.cm$table[2,1])
)
rfPCA.metrics

plot(varImp(rfPCA.ml), top = 10, main = "Random forest with PCA")

### here shows RF without PCA
rf.ml<-train(diagnosis ~ ., data = train_set, method = "rf", trControl=ctrl, importance=FALSE)
rf.cm <- confusionMatrix(rf.ml)
cm_plot(rf.ml, "Random Forest without PCA")                                  #### ---------------------># plot to use in report
rf.metrics <- data.frame (
  "Model" = "Random Forest",
  "Accuracy" = (rf.cm$table[1,1] + rf.cm$table[2,2])/100,
  "Recall" = rf.cm$table[2,2] / (rf.cm$table[2,2] + rf.cm$table[1,2]),
  "Precision" = rf.cm$table[2,2] / (rf.cm$table[2,1] + rf.cm$table[2,2]),
  "FNR" = (rf.cm$table[1,2] / (rf.cm$table[2,2] + rf.cm$table[1,2])),
  "Fscore" = (2 * rf.cm$table[2,2]) / (2 * rf.cm$table[2,2] + rf.cm$table[1,2] + rf.cm$table[2,1])
)
rf.metrics
plot(varImp(rf.ml), top = 10, main = "Random forest without PCA")

library(dplyr)
install.packages("ggraph")
library(ggraph)
library(igraph)
library(randomForest)
tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

### Here shows Random Forest Tree
install.packages("party")
library(party)
model<-randomForest(diagnosis ~., data = train_set, importance = TRUE, ntree=50, mtry=2, do.trace=100)
tree_num =10;
tree_func(model, tree_num)
#plot(x,"simple")


### ################################
###            kNN              ###
### ################################
knn.ml <- train(diagnosis~., data = train_set_pca, method = "knn", trControl =ctrl)
knn.cm <- confusionMatrix(knn.ml)
cm_plot(knn.ml, "KNN Model")                                  #### ---------------------># plot to use in report
knn.metrics <- data.frame (
  "Model" = "KNN",
  "Accuracy" = (knn.cm$table[1,1] + knn.cm$table[2,2])/100,
  "Recall" = knn.cm$table[2,2] / (knn.cm$table[2,2] + knn.cm$table[1,2]),
  "Precision" = knn.cm$table[2,2] / (knn.cm$table[2,1] + knn.cm$table[2,2]),
  "FNR" = (knn.cm$table[1,2] / (knn.cm$table[2,2] + knn.cm$table[1,2])),
  "Fscore" = (2 * knn.cm$table[2,2]) / (2 * knn.cm$table[2,2] + knn.cm$table[1,2] + knn.cm$table[2,1])
)
knn.metrics

### ################################
###     Gradient Boosting       ###
### ################################
library(gbm)
gbm.ml <- train(diagnosis~., data = train_set_pca, method = "gbm", trControl =ctrl)
gbm.cm <- confusionMatrix(gbm.ml)
cm_plot(gbm.ml, "Gradient Model")                                  #### ---------------------># plot to use in report
gbm.metrics <- data.frame (
  "Model" = "Gradient Boosting",
  "Accuracy" = (gbm.cm$table[1,1] + gbm.cm$table[2,2])/100,
  "Recall" = gbm.cm$table[2,2] / (gbm.cm$table[2,2] + gbm.cm$table[1,2]),
  "Precision" = gbm.cm$table[2,2] / (gbm.cm$table[2,1] + gbm.cm$table[2,2]),
  "FNR" = (gbm.cm$table[1,2] / (gbm.cm$table[2,2] + gbm.cm$table[1,2])),
  "Fscore" = (2 * gbm.cm$table[2,2]) / (2 * gbm.cm$table[2,2] + gbm.cm$table[1,2] + gbm.cm$table[2,1])
)
gbm.metrics

### After the model get trained, we check the accuracy for Test part of PCA transformed data
### and call ConfusionMatrix to calculate confusion matrix.

### Prediction using logistic Regression model

logit.pred <-logit.ml %>% predict(test_set_pca)
logit.matrix <- confusionMatrix(logit.pred, test_set_pca$diagnosis)
logit.matrix

### Prediction using Random Forest model
rfPCA.pred <-rfPCA.ml %>% predict(test_set_pca)
rfPCA.matrix <- confusionMatrix(rfPCA.pred, test_set_pca$diagnosis)
rfPCA.matrix

### Prediction using Random - No PCA Forest model

rf.pred <-rf.ml %>% predict(test_set)
rf.matrix <- confusionMatrix(rf.pred, test_set_pca$diagnosis)
rf.matrix


### Prediction using KNN model
knn.pred <-knn.ml %>% predict(test_set_pca)
knn.matrix <- confusionMatrix(knn.pred, test_set_pca$diagnosis)
knn.matrix

### Prediction using GBM model
gbm.pred <-gbm.ml %>% predict(test_set_pca)
gbm.matrix <- confusionMatrix(gbm.pred, test_set_pca$diagnosis)
gbm.matrix


### Comparison of resting results
### The confusionMatrix funcion returns The confusion matrix, Accuracy, 95% CI, No InformationRate and also Sensitivity, Specificity, Pos Pred Value and Neg Pred Value.
#where Accuracy represents the model ability to differentiate the diagnosis(malignant and benign) correctly, Sensitivity means the model ability to found every single individual
#with deasease, if it is100%, that means it can correctly identify all patients with the disease. Specificity is related to the ability of the model to correctly identify those patients without the disease. For example for gbm, it is 92.06. it says 92.06% of patients without the disease as test negative (actually true negatives) but there are 7.94% patient without the disease are incorrectly identified as test positive (false positives); 
# "Pos Pred Value" shows the precent of true positives from the total number of patients with the disease. "Neg Pred Value" shows the precent of true negatives from the total number of patients who didn't have the disease.

### Comparison of resting results
#library(data.table)
library(formattable)
#Logic Regression
logit.1 <- as.data.frame(logit.matrix$overall["Accuracy"])
colnames(logit.1)<-""
logit.2 <- as.data.frame(logit.matrix$byClass[1:4])
colnames(logit.2)<-""
logit<-rbind(logit.1, logit.2)
colnames(logit)<-"Logit PCA"

#RF
rfPCA.1 <- as.data.frame(rfPCA.matrix$overall["Accuracy"])
colnames(rfPCA.1)<-""
rfPCA.2 <- as.data.frame(rfPCA.matrix$byClass[1:4])
colnames(rfPCA.2)<-""
rfPCA<-rbind(rfPCA.1, rfPCA.2)
colnames(rfPCA)<-"Random Forest PCA"
row.names(rfPCA)<-c()

#RF
rf.1 <- as.data.frame(rf.matrix$overall["Accuracy"])
colnames(rf.1)<-""
rf.2 <- as.data.frame(rf.matrix$byClass[1:4])
colnames(rf.2)<-""
rf<-rbind(rf.1, rf.2)
colnames(rf)<-"Random Forest without PCA"
row.names(rf)<-c()

#KNN
knn.1 <- as.data.frame(knn.matrix$overall["Accuracy"])
colnames(knn.1)<-""
knn.2 <- as.data.frame(knn.matrix$byClass[1:4])
colnames(knn.2)<-""
knn<-rbind(knn.1, knn.2)
colnames(knn)<-"KNN PCA"
row.names(knn)<-c()

#GBM
gbm.1 <- as.data.frame(gbm.matrix$overall["Accuracy"])
colnames(gbm.1)<-""
gbm.2 <- as.data.frame(gbm.matrix$byClass[1:4])
colnames(gbm.2)<-""
gbm<-rbind(gbm.1, gbm.2)
colnames(gbm)<-"GBM PCA"
row.names(gbm)<-c()

final<-as.data.frame(t(cbind(logit,rfPCA, rf,knn,gbm)))

formattable(final)


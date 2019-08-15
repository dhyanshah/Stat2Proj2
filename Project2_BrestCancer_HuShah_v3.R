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
plot(BrCanNorm$diagnosis)
Cancer <- c(357,212)
Lbls <- c("Benign", "Malignant")
pie(Cancer, labels = Lbls, main="Breast Cancer Class Distribution") #### ---------------------># plot to use in report


# Let's check the structure of this dataset.
str(BrCanNorm)
# Let's check the missign values if any.

sapply(BrCanNorm, function(x) sum(is.na(x)))

## There are no missing variable
#############################
#OBJECTIVE 1 - EDA Analysis
## Lets Perform EDA
#############################

# Let's encode the response variable into a factor variable of M = 1's (Malignant) and B = 0's (Benign).

BrCanNorm$diagnosis <- ifelse(BrCanNorm$diagnosis == "M", 1, 0)
head(BrCanNorm)
BrCanNorm$diagnosis <- factor(BrCanNorm$diagnosis, levels = c(0,1))
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
#P1 <- P1 + scale_fill_discrete(name="Breast Cancer Cell",
                         #    breaks=c(0,1),
                         #    labels=c("Benign", "Malignant"))
P1

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
P2
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
P3
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
ncol(BrCanNorm)
dim(LoBrCanCor)
LoBrCanCor

#### We now have 21 attributes compare to what we started with which was 31 variables. 
########################################################
colnames(LoBrCanCor) #-----Reduced Non-correlated data
# [1]"perimeter_mean"          "area_mean"               "smoothness_mean"         "concave_points_mean"    
# [5] "symmetry_mean"           "fractal_dimension_mean"  "radius_se"               "area_se"                
# [9] "smoothness_se"           "compactness_se"          "concavity_se"            "concave_points_se"      
# [13] "symmetry_se"             "radius_worst"            "area_worst"              "smoothness_worst"       
# [17] "compactness_worst"       "concavity_worst"         "concave_points_worst"    "symmetry_worst"         
# [21] "fractal_dimension_worst"
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

########################################################

#### Principal Components Analysis - Non-Correlated (reduced) data ################
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


################################################################################################################
#OBJECTIVE 1 - Model
################################################################################################################

############################
### Model Building #########
############################
set.seed(1234)
BC <- cbind(diagnosis = BrCanNorm$diagnosis, LoBrCanCor)
train_indx <- createDataPartition(BC$diagnosis, p = 0.7, list = FALSE)

train_set <- BC[train_indx,]
test_set <- BC[-train_indx,]
testValues <-BC$diagnosis.factor
nrow(train_set)
nrow(test_set)

################################
#### Graph - GLM Model ####
################################

BrCanModel <- glm(diagnosis ~ area_mean, data=train_set, family = "binomial")
summary(BrCanModel)



######  H0:  β1=0 -> Breast Cancer cell mean area = 0
######  Ha:  β1≠0 -> Breast Cancer cell mean area ≠ 0

################################
plot(diagnosis ~ area_mean, data=train_set, main="Breast Cancer Regression Curve", ylab="Probability of Cancerous Tumor",
     pch=16)
curve(
  exp(BrCanModel$coef[1]+BrCanModel$coef[2]*x)/
        (1+exp(BrCanModel$coef[1]+BrCanModel$coef[2]*x)),
      add=TRUE
      )                                                     #### ---------------------># plot to use in report
################################################################
#### Check - Goodness of Fit - Hosemer & Lemeshow ####
################################################################
install.packages("ResourceSelection")
install.packages("pander")
library(ResourceSelection)
library(pander)
hoslem.test(BrCanModel$y, BrCanModel$fitted.values)
pander()

#### BrCanModel$y, BrCanModel$fitted.values
####        Test Statistic    Df        P-value
#### X-squared = 12.114     df = 8      0.1462
#### p-value > 0.05, we fail to reject null hypothesis
####
#### Intercept b0 = -6.7035 estimating β0
#### area_mean b1 = 29.411 estimating β1
#### Reject the null hypothesis, i.e. area_mean does help diagnosing breast cancer!!!
#### e^b1 = e^29.411 = 

P_BrCanPred <- predict(BrCanModel, data=train_set, type='response')
BrCanPred <- ifelse(P_BrCanPred >= .5, 1, 0)

#### Confirmation

BrCanPr <- train_set %>%
  mutate(correct = (diagnosis == BrCanPred)) %>%
  group_by(correct) %>%
  tally()

CanPrAcc <- round(BrCanPr$n[2]/(nrow(train_set)),3)

BrCanPr$n

BrCanPr$n[2] # Prediction that are accurate
# 359
BrCanPr$n[1] # Prediction that are inaccurate
#40

Final_Accuracy <- (CanPrAcc*100)
Final_Accuracy
# 90

############################
### Model Building #########
############################

############ Setting up 5-fold cross-validation ####
ctrl <- trainControl(method = "cv",number = 5, classProbs = TRUE)

############ Function for plotting confusion matrices ####
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

############ Conduct PCA ####
pca_wdbc <- princomp(BrCanNorm[,-c(1)]) # PCA on attributes
pc_wdbc <- pca_wdbc$scores # PCA scores
pc_wdbc_c <- BrCan$diagnosis # WDBC class attribute
full_wdbc <- data.frame(pc_wdbc,pc_wdbc_c) # Combining PC with class attribute

#install.packages("factoextra")
#library(factoextra)
summary(pca_wdbc)
fviz_eig(pca_wdbc, addlabels = TRUE, ylim = c(0,100), barfill = "steelblue1", line="navy") + 
  theme_classic() +
  labs(x = "Principal Components", y = "% of Explained Variance", title = "WDBC - Principal Components")  
                                                                                #### ---------------------># plot to use in report
############### We see that 53.1% of the variance is explained by the first principal component.

############ Conduct PCA - reduced/noncorrelated ####
pca_wdbc_nc <- princomp(LoBrCanCor[,-c(1)]) # PCA on attributes
pc_wdbc_nc <- pca_wdbc$scores # PCA scores
pc_wdbc_nc_c <- BrCan$diagnosis # WDBC class attribute
reduced_wdbc <- data.frame(pc_wdbc_nc,pc_wdbc_nc_c) # Combining PC with class attribute

#install.packages("factoextra")
#library(factoextra)
summary(pca_wdbc_nc)
fviz_eig(pca_wdbc_nc, addlabels = TRUE, ylim = c(0,100), barfill = "steelblue1", line="navy") + 
  theme_classic() +
  labs(x = "Principal Components", y = "% of Explained Variance", title = "WDBC - Principal Components")
                                                                                #### ---------------------># plot to use in report
############### We see that 52.8% of the variance is explained by the first principal component. After removing
############### highly correlated attributes from the data.

####################################################################################
###  Function to print out the plots for the multiple linear regression          ###
####################################################################################
mlrplots <- function(fit, hidenum = TRUE)
{
  #library(MASS)
  sres <- rstudent(fit)
  res <- resid(fit)
  leverage <- hatvalues(fit)
  
  par(mfrow=c(2,3))
  
  #### Plot residuals
  plot(fitted(fit), res, xlab = "Fitted", ylab = "Residuals")
  abline(h=0, col="blue", lty=2)  
  
  #### Plot studentized residuals
  plot(fitted(fit), sres, xlab = "Fitted", ylab = "StudResiduals")
  abline(h=-2, col="blue", lty=2)
  abline(h=2, col="blue", lty=2)
  if(!hidenum)
    text(sres~fitted(fit), y=sres, labels=ifelse( abs(sres) >= 2, names(sres),""), col="red")  
  
  #### Plot Leverage - examine any observations ~2-3 times greater than the average hat value
  plot(x = leverage, y = sres, xlab = "Leverage", ylab = "StudResiduals")
  abline(h=-2, col="blue", lty=2)
  abline(h=2, col="blue", lty=2)
  abline(v = mean(leverage)*2, col="blue", lty=2) #line is at 2x mean
  
  #### QQ Plot
  qqnorm(sres, xlab="Quantile", ylab="Residual", main = NULL) 
  qqline(sres, col = 2, lwd = 2, lty = 2) 
  
  #### Cooks D
  cooksd <- cooks.distance(fit)
  sample_size <- length(fit$model[,1])
  plot(cooksd, xlab = "Observation", ylab = "Cooks D", col = c("blue"))
  abline(h = 4/sample_size, col="red")  # add cutoff line
  if(!hidenum)
    text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
  
  #### Histogram of residuals with normal curve
  #### If the curve looks wrong try using the studentized residuals
  
  hist(res, freq=FALSE, xlab = "Residuals", main = NULL)
  curve(dnorm(x, mean=mean(res), sd=sd(res)), add=TRUE, col = "blue")
}

###################################
###           Regression        ###
###################################

logicFit <- glm(diagnosis ~ . , data=BrCanNorm, family = binomial(link = "logit") , control = list(maxit = 50))
summary(logicFit)

#### VIF for covariance between Radius, Perimeter, Area
install.packages("car")
library(car)
vif(logicFit) -> logicFit.vif
logicFit.vif

#### Plots for the model
mlrplots(logicFit)                                              #### ---------------------># plot to use in report
install.packages("glmnetUtils")
library(glmnetUtils)
x <- model.matrix(diagnosis~.,BrCanNorm)[,-1]
y <- BrCan$diagnosis

y <- as.factor(as.character(BrCan$diagnosis))

# Fit models 
#fit.lasso <- glmnet(x, y, family="gaussian", alpha=1) #fit LASSO
logicFitNet <- glmnet(x, y, family = "binomial",  alpha=.5) #fit Elastic Net

cv <- cv.glmnet(x,y, type.measure = "class", alpha = 0.5, family = "binomial")

plot(cv, main="Elastic Net")                                     #### ---------------------># plot to use in report


################################################################################################################
#### OBJECTIVE 2 - Model
################################################################################################################

###################################
###  Logistic regression        ###
###################################
install.packages("e1071")
library("e1071")
logit.ml <- train(pc_wdbc_nc_c~., reduced_wdbc, method = "glm", family = "binomial", trControl =ctrl)
logit.cm <- confusionMatrix(logit.ml)
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

###################################
###   Bagging - Random Forest   ###
###################################
rf.ml <- train(pc_wdbc_nc_c~., reduced_wdbc, method = "rf", trControl =ctrl)
rf.cm <- confusionMatrix(rf.ml)
cm_plot(rf.ml, "Random Forest")                                  #### ---------------------># plot to use in report
rf.metrics <- data.frame (
  "Model" = "Random Forest",
  "Accuracy" = (rf.cm$table[1,1] + rf.cm$table[2,2])/100,
  "Recall" = rf.cm$table[2,2] / (rf.cm$table[2,2] + rf.cm$table[1,2]),
  "Precision" = rf.cm$table[2,2] / (rf.cm$table[2,1] + rf.cm$table[2,2]),
  "FNR" = (rf.cm$table[1,2] / (rf.cm$table[2,2] + rf.cm$table[1,2])),
  "Fscore" = (2 * rf.cm$table[2,2]) / (2 * rf.cm$table[2,2] + rf.cm$table[1,2] + rf.cm$table[2,1])
)
rf.metrics


plot(varImp(rf.ml), top = 10, main = "Random forest") ##### NEED TO ASSIGN COLUMN NAME BACK FOR VISUALIZATION ####

###################################
###            kNN              ###
###################################
knn.ml <- train(pc_wdbc_nc_c~., reduced_wdbc, method = "knn", trControl =ctrl)
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

###################################
###     Gradient Boosting       ###
###################################
gbm.ml <- train(pc_wdbc_nc_c~., reduced_wdbc, method = "gbm", trControl =ctrl)
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

###################################
### Model Results Comparision   ###
###################################
Compare_Results <- resamples(list(GLM = logit.ml, RF = rf.ml, KNN = knn.ml, GBM = gbm.ml))
summary(Compare_Results)

# SUMMARY: KNN performs slightly better followed by GLM, GBM and RF by their accuracy, however pretty close to be the same.
# --------------------------------------------------------------------
# Call:
#  summary.resamples(object = Compare_Results)
# Models: GLM, RF, KNN, GBM 
# Number of resamples: 5 
# --------------------------------------------------------------------
# Accuracy 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# GLM 0.9298246 0.9380531 0.9380531 0.9454882 0.9478261 0.9736842    0
# RF  0.9210526 0.9217391 0.9557522 0.9473815 0.9649123 0.9734513    0
# KNN 0.9391304 0.9557522 0.9736842 0.9684502 0.9736842 1.0000000    0
# GBM 0.9298246 0.9298246 0.9646018 0.9508140 0.9646018 0.9652174    0
# --------------------------------------------------------------------
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# GLM 0.8461538 0.8680127 0.8705189 0.8836034 0.8896000 0.9437315    0
# RF  0.8288407 0.8296247 0.9066270 0.8863643 0.9238477 0.9428812    0
# KNN 0.8668761 0.9038625 0.9431705 0.9315281 0.9437315 1.0000000    0
# GBM 0.8476954 0.8492562 0.9242119 0.8943551 0.9242119 0.9264000    0


#######################################
### Pair-wise comparision by Pval   ###
#######################################
Results_Diff <- diff(Compare_Results) 
summary(Results_Diff)

# Call:
# summary.diff.resamples(object = Results_Diff)
# p-value adjustment: bonferroni 
# Upper diagonal: estimates of the difference
# Lower diagonal: p-value for H0: difference = 0
# -----------------------------------------------
# Accuracy 
#     GLM RF        KNN       GBM      
# GLM     -0.001893 -0.022962 -0.005326
# RF  1             -0.021069 -0.003432
# KNN 1   1                    0.017636
# GBM 1   1         1                  
# -----------------------------------------------
# Kappa 
#     GLM RF        KNN       GBM      
# GLM     -0.002761 -0.047925 -0.010752
# RF  1             -0.045164 -0.007991
# KNN 1   1                    0.037173
# GBM 1   1         1  

# SUMMARY: There is no difference in distribution as per lower diagonal of table showing pvalues. While the 
# upper diagonal shows the estimated difference between model distributions howerver, GLM appears to be the most
# accurate model among all models.

#######################################
###   Prediction Accuracy of GLM    ###
#######################################
GLM_pred <- predict(logit.ml, newdata = reduced_wdbc)
confusionMatrix(GLM_pred, testValues) ##reduced_wdbc )



#######################################
###          McNemar's Test         ###
#######################################









################################################################################################################
######################################## Code End ##############################################################
################################################################################################################





################################################################################################################
#### Backup/Misc. Code (DELETE in Final Submission)
################################################################################################################
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



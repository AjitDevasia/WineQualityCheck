#Installing packages
install.packages("ROSE")

#Loading Libraries


library(tidyverse)
library(dplyr)
library(DataExplorer)
library(data.table)
library(ggplot2)
library(vcd)
library(rpart)
library(randomForest)
library(ROSE)

#Loading Datasets
redwine <- read.csv("winequality-red.csv",header = TRUE, sep = ';')
str(redwine)
whitewine <-read.csv("winequality-white.csv", header = TRUE, sep = ';')
str(whitewine)

#Data Profiling
object.size(whitewine)/10^6
#0.45 MB

#Check for NAs across the Datasets
whitewine %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))
#NO NAs present in the Data---------------------------------------

#Discrete Variables check
prop.table(table(whitewine$quality))
ggplot(data=whitewine, mapping = aes(x=(whitewine$quality),fill =as.factor(whitewine$quality)))+
  geom_bar()
# Quality Index 6 and 5 dominate the dataset 74% contribution

#Outlier Detection & Treatment
#Outlier Detection
  boxplot(whitewine$fixed.acidity, main="Fixed Acidity")
  boxplot(whitewine$volatile.acidity, main="Volatile Acidity")
  boxplot(whitewine$citric.acid, main="Citric Acid")
  boxplot(whitewine$residual.sugar, main="Residual Sugar")
  boxplot(whitewine$chlorides, main="Chlorides")
  boxplot(whitewine$free.sulfur.dioxide, main="Free Sulphur Dioxide")
  boxplot(whitewine$total.sulfur.dioxide, main="Total Sulph. Dioxide")
  boxplot(whitewine$density, main="Density") # Not required as insignificant outliers
  boxplot(whitewine$pH, main="PH")
  boxplot(whitewine$sulphates, main="Sulphates")
  boxplot(whitewine$alcohol, main="Alochol") # Not required as insignificant outliers
  fun <- function(x){
    quantiles <- quantile( x, c(.05, .95 ) )
    x[ x < quantiles[1] ] <- quantiles[1]
    x[ x > quantiles[2] ] <- quantiles[2]
    x
  }
  whitewine$volatile.acidity<-fun(whitewine$volatile.acidity)
  whitewine$citric.acid<-fun(whitewine$citric.acid)
  whitewine$residual.sugar <-fun(whitewine$residual.sugar)
  whitewine$chlorides <-fun(whitewine$chlorides)
  whitewine$free.sulfur.dioxide<-fun(whitewine$free.sulfur.dioxide)
  whitewine$total.sulfur.dioxide<-fun(whitewine$total.sulfur.dioxide)
  whitewine$pH<-fun(whitewine$pH)
  whitewine$sulphates<-fun(whitewine$sulphates)
  
  
  # AFter capping outliers
  boxplot(whitewine$fixed.acidity, main="Fixed Acidity")
  boxplot(whitewine$volatile.acidity, main="Volatile Acidity")
  boxplot(whitewine$citric.acid, main="Citric Acid")
  boxplot(whitewine$residual.sugar, main="Residual Sugar")
  boxplot(whitewine$chlorides, main="Chlorides")
  boxplot(whitewine$free.sulfur.dioxide, main="Free Sulphur Dioxide")
  boxplot(whitewine$total.sulfur.dioxide, main="Total Sulph. Dioxide")
  boxplot(whitewine$density, main="Density") # Not required as insignificant outliers
  boxplot(whitewine$pH, main="PH")
  boxplot(whitewine$sulphates, main="Sulphates")
  boxplot(whitewine$alcohol, main="Alochol")

  #Transforming Data
  whitewine$quality<- as.numeric(whitewine$quality)
  str(whitewine$quality)  
  
  #Applying multiple regression
  ww_model <- lm(quality~.,data=whitewine)
  str(whitewine)
  ww_model
  summary(ww_model)
  
  #Applying Classification
  whitewine$qualityLabel <- ifelse(whitewine$quality>=6,'Good','Bad')
  prop.table(table(whitewine$qualityLabel))
  whitewine$qualityLabel <- as.factor(whitewine$qualityLabel)
  ww_randomforest <- randomForest(qualityLabel~.,data=whitewine)
  ww_randomforest  
  #Testing on RedWine
  redwine$qualityLabel <- ifelse(redwine$quality>=6,'Good','Bad')
  prop.table(table(redwine$qualityLabel))
  redwine$qualityLabel <- as.factor(redwine$qualityLabel)
  rw_randomforest <- randomForest(quality~.,data=redwine)
  rw_randomforest
  
  
    
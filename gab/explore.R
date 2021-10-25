# ==================================================================
# Purpose:      Rscript for demo and Exercise in ADA1 Vol 1 Chap 3.
# Author:       Neumann Chew
# DOC:          20-08-2018
# Updated:      25-08-2018
# Topics:       Data Exploration on Big Dataset
# RPackages:    data.table
# Data Source:  NYC Flights Jan-Oct 2014 at https://github.com/arunsrinivasan/flights/wiki/NYC-Flights-2014-data
# Ref:          data.table Vignette at https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
#               All data.table references at https://github.com/Rdatatable/data.table/wiki/Getting-started
#===================================================================

library(data.table)
library(ggplot2)
# install.packages('Rcpp')
library(Rcpp)
library(car)

# Set a working directory to store all the related datasets and files.
# setwd("D:/Dropbox/Schools/NBS/REP RE6013/3 Data Exploration and Statistics")

# Import using data.table fread function
system.time(wind.dt <- fread("data/train_data.csv")) # datatable object

sum(is.na(wind.dt)) # 20579
summary(wind.dt)
colnames(wind.dt)
# get rid of brackets in columns
remove_brackets <- function(x) {  
  gsub("\\(.*?\\)", "", x)
  }
colnames(wind.dt) <- sapply(colnames(wind.dt), remove_brackets)
colnames(wind.dt)

# Datetime column is in object format. It should be converted into datetime format

###################
# Datetime Utils
###################
get_year <- function(x) {  
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$year + 1900 # based on 1990
}
get_month <- function(x){
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$mon + 1 # 0 index
}
get_mday <- function(x){
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$mday
}
get_wday <- function(x){
  # The wday component of a POSIXlt object is the numeric weekday (0-6 starting on Sunday).
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$wday
}
get_hour <- function(x){
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$hour
}
get_min <- function(x){
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$min
}
get_sec <- function(x){
  as.POSIXlt(x,format="%Y-%m-%dT%H:%M")$sec
}

class(wind.dt$datetime)
#as.POSIXlt(wind.dt$datetime[1],format="%Y-%m-%dT%H:%M")
#as.POSIXlt(wind.dt$datetime[1],format="%Y-%m-%dT%H:%M")$wday

wind.dt$year <- unlist(lapply(wind.dt$datetime, get_year))
wind.dt$month <- unlist(lapply(wind.dt$datetime, get_month))
wind.dt$mday <- unlist(lapply(wind.dt$datetime, get_mday))
wind.dt$wday <- unlist(lapply(wind.dt$datetime, get_wday))
wind.dt$hour <- unlist(lapply(wind.dt$datetime, get_hour))
wind.dt$min <- unlist(lapply(wind.dt$datetime, get_min))
wind.dt$sec <- unlist(lapply(wind.dt$datetime, get_sec))

# convert necessary features to factor 
wind.dt$year = as.factor(wind.dt$year)
wind.dt$month = as.factor(wind.dt$month)
wind.dt$mday = as.factor(wind.dt$mday)
wind.dt$wday = as.factor(wind.dt$wday)

# drop column tracking_id and datatime
wind.dt[,tracking_id:=NULL]
wind.dt[,datetime:=NULL]
wind.dt[,sec:=NULL]
wind.dt[,min:=NULL]
colnames(wind.dt)
ncol(wind.dt)

summary(wind.dt)


#sample split into train and test set
library(caTools)
set.seed(2021)
train <- sample.split(Y=wind.dt$windmill_generated_power, SplitRatio=0.7)
trainset<- subset(wind.dt, train==T)
testset<- subset(wind.dt, train==F)
paste("number of rows of trainset: ",nrow(trainset))
paste("proportion of trainset: ", nrow(trainset)/nrow(wind.dt))
paste("number of rows of testset: ",nrow(testset))
paste("proportion of testset: ", nrow(testset)/nrow(wind.dt))

###################
# NA Analysis and Handling
###################
# install.packages("naniar")
library(naniar)
# install.packages("VIM")
library(VIM)
# Data imputation with MICE stochastic regression imputation
library(mice)

trainset[trainset == ""] <- NA # account for "" as NA
testset[testset == ""] <- NA

data_imputation = function(data)
{
  # Imputing Numeric missing data using MICE stochastic regression imputation
  imp = mice(data[,c(1:14,17,19:20)], method = "norm.nob", m = 5, maxit = 5)
  data1 = complete(imp,2)
  
  # Filling missing values in categorical variables using KNN imputer
  imp2 = kNN(data[,c(15,16)])
  data2 = imp2[,c(1,2)]
  
  # Concatenating all the imputed features
  data1 = cbind(data[,c(18,21:25)],data1, data2)
  
  return(data1)
}

trainset.imputation <- data_imputation(trainset)
testset.imputation <- data_imputation(testset)

sum(is.na(trainset)) 
sum(is.na(trainset.imputation))
sum(is.na(testset))
sum(is.na(testset.imputation))

trainset.imputation$cloud_level = as.factor(trainset.imputation$cloud_level)
trainset$cloud_level = as.factor(trainset$cloud_level)
testset.imputation$cloud_level = as.factor(testset.imputation$cloud_level)
testset$cloud_level = as.factor(testset$cloud_level)

trainset.imputation$cloud_level <- droplevels(trainset.imputation$cloud_level) # removes unused "" level
trainset$cloud_level <- droplevels(trainset$cloud_level) # removes unused "" level
testset.imputation$cloud_level <- droplevels(testset.imputation$cloud_level) # removes unused "" level
testset$cloud_level <- droplevels(testset$cloud_level) # removes unused "" level

###############################################################################################
# Linear Regression
###############################################################################################
# Develop model on trainset.imputation, including selected time data
m0 <- lm(windmill_generated_power ~ . - year, data = trainset.imputation)
summary(m0)
# residuals(m1) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.m0.train.imputation <- sqrt(mean(residuals(m0)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(m0)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.m0.test.imputation <- predict(m0, newdata = testset.imputation)
testset.imputation.error <- testset.imputation$windmill_generated_power - predict.m0.test.imputation

# Testset Errors
RMSE.m0.test.imputation <- sqrt(mean(testset.imputation.error^2))
summary(abs(testset.imputation.error))

vif(m0)
###############################################################################################
# Develop model on trainset.imputation, excluding time data
m1 <- lm(windmill_generated_power ~ . - year - mday - wday - month, data = trainset.imputation)
summary(m1)
# residuals(m1) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.m1.train.imputation <- sqrt(mean(residuals(m1)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(m1)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.m1.test.imputation <- predict(m1, newdata = testset.imputation)
testset.imputation.error <- testset.imputation$windmill_generated_power - predict.m1.test.imputation

# Testset Errors
RMSE.m1.test.imputation <- sqrt(mean(testset.imputation.error^2))
summary(abs(testset.imputation.error))

vif(m1)

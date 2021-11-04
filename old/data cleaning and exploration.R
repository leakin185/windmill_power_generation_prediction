# ==================================================================
# Purpose:      Rscript for data cleaning and exploration
# Author:       RE6013 Team 4 
# DOC:          02-10-2021
# Updated:      
# Topics:       data cleaning and exploration
# RPackages:    data.table, ggplot2 
# Data Source:  
# Ref:          https://www.kaggle.com/synergystud/a-fine-windy-day-hackerearth-ml-challenge
#===================================================================


# import libraries
library(data.table)
library(ggplot2)

setwd('/Users/leawangyi/Documents/2021/DOCUMENTS/REP_Y4S1/RE6013 BUS ANA & APP MACHINE LEARNING/RE6013')
# setwd("D:/Dropbox/Schools/NBS/REP RE6013/3 Data Exploration and Statistics")

# Import using data.table fread function
system.time(wind.dt <- fread("data/train_data.csv")) # datatable object

sum(is.na(wind.dt)) # 20579
summary(wind.dt)
colnames(wind.dt)

# get rid of brackets and units in columns
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
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$year + 1900 # based on 1990
}
get_month <- function(x){
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$mon + 1 # 0 index
}
get_mday <- function(x){
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$mday
}
get_wday <- function(x){
  # The wday component of a POSIXlt object is the numeric weekday (0-6 starting on Sunday).
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$wday
}
get_hour <- function(x){
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$hour
}
get_min <- function(x){
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$min
}
get_sec <- function(x){
  as.POSIXlt(x,format="%Y-%m-%d %H:%M:%S")$sec
}

class(wind.dt$datetime)
# as.POSIXlt(wind.dt$datetime[1],format="%Y-%m-%d %H:%M:%S")
# as.POSIXlt(wind.dt$datetime[1],format=""%Y-%m-%d %H:%M:%S")$wday

wind.dt$year <- unlist(lapply(wind.dt$datetime, get_year))
wind.dt$month <- unlist(lapply(wind.dt$datetime, get_month))
wind.dt$mday <- unlist(lapply(wind.dt$datetime, get_mday))
wind.dt$wday <- unlist(lapply(wind.dt$datetime, get_wday))
wind.dt$hour <- unlist(lapply(wind.dt$datetime, get_hour))
wind.dt$min <- unlist(lapply(wind.dt$datetime, get_min))
wind.dt$sec <- unlist(lapply(wind.dt$datetime, get_sec))

###################
# NA Analysis and Handling
###################
# install.packages("naniar")
library(naniar)
# install.packages("VIM")
library(VIM)

gg_miss_upset(wind.dt) # interesting overview of NA

# Missing Values Pattern
aggr_plot <- aggr(wind.dt, col=c('black','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=0.4, gap=10, ylab=c("Histogram of missing data","Pattern"))

# Analysis of NA values
# missing columns: wind_speed  atmospheric_temperature  shaft_temperature  
# blades_angle  gearbox_temperature  engine_temperature  motor_torque  
# generator_temperature  atmospheric_pressure  area_temperature  windmill_body_temperature  wind_direction  
# resistance  rotor_torque  blade_length  windmill_height
# most number of NA values come from wind-direction and blade_length
# most instances of NA values are unrelated to each other
# most related pair of NA values belong to blade_length and wind_direction

# analysis of NA
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

###################
# Preparation of Data for Analysis
###################
# drop column tracking_id and datatime
wind.dt[,tracking_id:=NULL]
wind.dt[,datetime:=NULL]

# 1. simplistic way to handle NA

wind.dt[wind.dt == ""] <- NA # account for "" as NA

# na_handler <- function(x) {
  # if (is.numeric(x)) {
    ### Numerical Imputation using Mean Values
    # x[is.na(x)] <- mean(x, na.rm = TRUE)
    # x
  # } else {
    ### Categorical Imputation using Mode
   #  x[is.na(x)] <- names(which.max(table(x)))
    # x
 #  }
# }

# sum(is.na(wind.dt)) # 22614
# wind.dt <- wind.dt[, lapply(.SD, na_handler)]
# sum(is.na(data1)) # 0

# OR 2. Data imputation with MICE stochastic regression imputation
as.data.frame(colnames(wind.dt))
library(mice)
data_imputation = function(data)
{
  # Imputing Numeric missing data using MICE stochastic regression imputation
  imp = mice(wind.dt[,c(3:16,19,21:22)], method = "norm.nob", m = 5, maxit = 5)
  data1 = complete(imp,2)
  densityplot(imp) 
  print("Blue is observed v/s Magenta is imputed")
  
  # Filling missing values in categorical variables using KNN imputer
  imp2 = kNN(wind.dt[,c(17,18)])
  data2 = imp2[,c(1,2)]
  
  # Concatenating all the imputed features
  data1 = cbind(wind.dt[,c(1:2,20,23:29)],data2,data1)
  
  return(data1)
}

data1 <- data_imputation(wind.dt)

sum(is.na(wind.dt)) # 22614
apply(is.na(data1), 2, which)
sum(is.na(data1)) # 0

# Converting character variables to factors, after NAs are removed
wind.dt$turbine_status = as.factor(wind.dt$turbine_status)
wind.dt$turbine_status <- droplevels(wind.dt$turbine_status)
wind.dt$cloud_level = as.factor(wind.dt$cloud_level) # removes unused "" level
wind.dt$cloud_level <- droplevels(wind.dt$cloud_level) # removes unused "" level
wind.dt$year = as.factor(wind.dt$year)
wind.dt$month = as.factor(wind.dt$month)
wind.dt$mday = as.factor(wind.dt$mday)
wind.dt$wday = as.factor(wind.dt$wday)


levels(wind.dt$turbine_status)
levels(wind.dt$cloud_level)

# drop the cateogorical variables
wind.dt_continuous = copy(wind.dt)
wind.dt_continuous[,turbine_status:=NULL]
wind.dt_continuous[,cloud_level:=NULL]
wind.dt_continuous

library(e1071)
# skewness of each continuous variable
skewness <- sapply(wind.dt_continuous, skewness)
summary(skewness)
skewness
# in general, the data has negative skewness which indicates that the mean of the data values is less than the median, and the data distribution is left-skewed.
# most negatively skewed in distribution is blade_length, -9.5087
# most positively skewed in distribution is gearbox_temperature, 0.8868

# from reference, removal of outliers affects ML model performance;
# no outlier removal for now 


boxplot(wind.dt$blade_length) 
# anomaly is a negative number
# many blade_length values are negative, replace with mean of blade length; refine if needed
wind.dt[blade_length < 0]$blade_length <- mean(wind.dt$blade_length, na.rm = TRUE)
wind.dt[blade_length < 0]$blade_length
boxplot(wind.dt$blade_length)

# gearbox_temperature
boxplot(wind.dt_continuous$gearbox_temperature)

# individual variable data analysis: interesting findings
# 1. wind_speed, atmospheric_pressure, resistance: negative values exist, which may represent direction
# 2. windmill_height, negative values exist, might represent lower than sea level? 

# Cleaned wind.dt
wind.dt

# cleaned numeric data table 
wind.numeric = copy(wind.dt)
wind.numeric[,turbine_status:=NULL]
wind.numeric[,cloud_level:=NULL]
wind.numeric

###############################################################################################
# General Analysis
###############################################################################################

library(visdat)
# install.packages("Hmisc")
library(Hmisc)
library(lattice)
options(warn=-1)

# 1. Time Series analysis w.r.t Target Variable: Power Generated
wind.dt %>%
  ggplot(aes(x = month, y = windmill_generated_power)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Monthwise Generated Power",
       y = "Power Genereraed(Kw/h)",
       x = "mday") + theme_bw(base_size = 8) + facet_wrap(~mday)

wind.dt %>%
  ggplot(aes(x = month,y = windmill_generated_power)) + 
  geom_point() + 
  facet_wrap(~year) + 
  theme_dark()
paste("We observe in the first month(Jan), max power is generated")

# 2. Analysing all the Numeric Variables
library(funModeling) 
plot_num(wind.numeric)

# 3. Analysing Categorical Variables 
# majority is Low for cloud level
# majority = BB for turbine status
# month october is anomaly low in percentage (0.98%)
freq(wind.dt[,c(15,16,22,23,24,25,26,27)])



# add correlation charts 

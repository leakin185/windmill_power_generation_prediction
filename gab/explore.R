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
# lawsuit.dt$Dept <- factor(lawsuit.dt$Dept)

###################
# NA Analysis and Handling
###################
# install.packages("naniar")
library(naniar)

gg_miss_upset(wind.dt) # interesting overview of NA

# TODO further analysis of NA
# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

###################
# Preparation of Data for Analysis
###################
# drop column tracking_id and datatime
wind.dt[,tracking_id:=NULL]
wind.dt[,datetime:=NULL]

# simplistic way to handle NA, TODO refine if have time
na_handler <- function(x) {
  if (is.numeric(x)) {
    ### Numerical Imputation using Mean Values
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    ### Categorical Imputation using Mode
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

sum(is.na(wind.dt)) # 20579
wind.dt <- wind.dt[, lapply(.SD, na_handler)]
sum(is.na(wind.dt)) # 0

# Cleaned wind.dt
wind.dt

###############################################################################################
# General Analysis
###############################################################################################
# Start with environmental data analysis
# 
# #Our transformation function
# scaleFUN <- function(x) sprintf("%.2f", x)
# 
# ggplot(data = wind.dt) + labs(title="Windmill power against wind_speed and Experience\n without Department 6")+ 
#   geom_point(mapping = aes(x = wind_speed, y = windmill_generated_power, color=atmospheric_temperature), position = "jitter")
# 
# d# dept breakdown
# ggplot(data = lawsuit.dt) + labs(title="Breakdown of Gender across Departments")+ 
#   geom_bar(mapping = aes(x = Dept, fill = Gender))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_bar(mapping = aes(x = Clin, colour = Clin))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_bar(mapping = aes(x = Cert, colour = Cert))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_bar(mapping = aes(x = Exper, colour = Gender))
# 
# ###############################################################################################
# # Deeper Relations to salary increment
# ###############################################################################################
# lawsuit.dt.exclude6 = lawsuit.dt
# lawsuit.dt.exclude6 <- lawsuit.dt.exclude6[Dept!=6]
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Gender, y = sal_incr), position = "jitter")
# ## from the above two results, shows a similar breakdown when comparing sal_incr
# 
# ## with Rank as gradient, can show that there are other correlations with higher increment, other than gender
# ggplot(data = lawsuit.dt) + labs(title="Salary Increment against Gender and Rank", color="Rank") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Rank), position = "jitter")
# 
# ggplot(data = lawsuit.dt.exclude6) + labs(title="Salary Increment against Gender and Rank\n without Department 6", color="Rank") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Rank), position = "jitter")
# 
# ggplot(data = lawsuit.dt) + labs(title="Salary Increment against Gender and Experience", color="Experience") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Exper), position = "jitter")
# 
# ggplot(data = lawsuit.dt.exclude6) + labs(title="Salary Increment against Gender and Experience\n without Department 6", color="Experience") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Exper), position = "jitter")
# 
# ggplot(data = lawsuit.dt) + labs(title="Salary Increment against Gender and Publication Rate", color="Publication Rate") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Prate), position = "jitter")
# 
# ggplot(data = lawsuit.dt.exclude6) + labs(title="Salary Increment against Gender and Publication Rate\n without Department 6", color="Publication Rate") + ylab("Salary Increment")+ 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Prate), position = "jitter")
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Cert), position = "jitter")
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Clin), position = "jitter")
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Gender, y = sal_incr, color=Dept), position = "jitter")
# 
# ### Box Plot (Category variables)
# ggplot(data = lawsuit.dt) +
#   geom_boxplot(mapping = aes(x = Gender, y = sal_incr, color=Rank))
# 
# ggplot(data = lawsuit.dt) +
#   geom_boxplot(mapping = aes(x = Gender, y = sal_incr, color=Cert))
# 
# ggplot(data = lawsuit.dt) +
#   geom_boxplot(mapping = aes(x = Gender, y = sal_incr, color=Clin))
# 
# ggplot(data = lawsuit.dt) +labs(title="Salary Increment against Gender and Department", color="Department") + ylab("Salary Increment")+ 
#   geom_boxplot(mapping = aes(x = Gender, y = sal_incr, color=Dept))
# 
# ###############################################################################################
# # Extras
# ###############################################################################################
# # continuous variable
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Exper, y = sal_incr, color=Gender))
# 
# # continuous variable
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Prate, y = sal_incr, color=Gender))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Rank, y = sal_incr, color=Gender))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Clin, y = sal_incr, color=Gender))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Cert, y = sal_incr, color=Gender))
# 
# ggplot(data = lawsuit.dt) + 
#   geom_point(mapping = aes(x = Dept, y = sal_incr, color=Gender))
# 
# ### Correlation plot
# library(ggcorrplot)
# summary(lawsuit.dt)
# str(lawsuit.dt)
# 
# # drop id
# lawsuit.corr.dt = lawsuit.dt
# lawsuit.corr.dt[,ID:=NULL]
# str(lawsuit.corr.dt)
# 
# cols <- c("Dept","Gender", "Clin", "Cert", "Prate", "Exper", "Rank", "Sal94", "Sal95", "sal_incr")
# lawsuit.corr.dt <- lawsuit.dt[, lapply(.SD, as.numeric)]
# str(lawsuit.corr.dt)
# 
# corr <- round(cor(lawsuit.corr.dt), 1)
# ggcorrplot(corr, hc.order = TRUE, 
#            type = "lower", 
#            lab = TRUE, 
#            lab_size = 3, 
#            method="circle", 
#            colors = c("tomato2", "white", "springgreen3"), 
#            ggtheme=theme_bw)

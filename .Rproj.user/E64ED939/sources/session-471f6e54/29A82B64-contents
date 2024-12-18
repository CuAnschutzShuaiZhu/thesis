######################################
### Title: 01_data_cleaning
### Date: 06/20/2024
### Author: Shuai Zhu
### Description: thesis 
######################################
# Bayesian model. 
### load library
library(dplyr)
library(openxlsx)
library(mclust)
library(tidyr)

### setting working directory
#working_directory <-  'C:\\Users\\zhushu\\OneDrive\\Graduate File\\Course\\Thesis'
#setwd(working_directory)

### read data

df_liia <- read.xlsx('./DataRaw/LIIA Plasma and CSF Study.xlsx')%>%as_tibble()
df_transposed <- read.csv('./DataRaw/transposed_data_240619.csv')%>%as_tibble()

### clean
df_liia$visit <- factor(substr(df_liia$`Sample.ID`, 11, 11), labels = c("baseline", "followup")) 


LIIA_baseline <- filter(df_liia, visit=="baseline")
data.fit <- LIIA_baseline%>%
  dplyr::select(c('CSF.AB42/40.Ratio', 'Plasma.AB42/40.Ratio'))%>%drop_na()





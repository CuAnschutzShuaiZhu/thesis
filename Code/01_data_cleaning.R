######################################
### Title: 01_data_cleaning
### Date: 06/20/2024
### Author: Shuai Zhu
### Description: thesis 
######################################
# Bayesian model. 
### load library
library(tidyverse)
library(openxlsx)
library(mclust)
library(tidyr)
library(table1)
library(pROC)
library(cutpointr)
library(caret)
library(gridExtra)
library(MASS)
library(parallel)
#library(bayesrules)
### setting working directory
if(Sys.info()[1] =='Windows'){
  working_directory <-  'C:\\Users\\zhushu\\OneDrive\\Graduate File\\Course\\Thesis'
}else if(Sys.info()[1]=='Linux'){
  working_directory <-  '/home/shuai/thesis'
}

setwd(working_directory)

### read data

df_liia <- read.xlsx('./DataRaw/LIIA Plasma and CSF Study.xlsx')%>%as_tibble()
df_transposed <- read.csv('./DataRaw/transposed_data_240619.csv')%>%as_tibble()

### clean
df_liia$visit <- factor(substr(df_liia$`Sample.ID`, 11, 11), labels = c("baseline", "followup")) 


LIIA_baseline <- df_liia%>%filter(visit=="baseline")%>%
  mutate(across(c(`Plasma.P-Tau181.(pg/ml)`, `Plasma.AB40.(pg/ml)`), as.numeric))

data.fit <- LIIA_baseline%>%
  dplyr::select(c('CSF.AB42/40.Ratio', 'Plasma.AB42/40.Ratio'))%>%drop_na()
colnames(data.fit ) <- c("csf", "plasma")
data.fit%>%saveRDS('DataProcessed/datafit.RDS')


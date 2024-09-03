######################################
### Title: 02_bayesian_sampling
### Date: 06/20/2024
### Author: Shuai Zhu
### Description: thesis 
######################################

library(bayesmix)
library(bmixture)
### setting working directory
working_directory <-  'C:\\Users\\zhu\\OneDrive\\Graduate File\\Course\\Thesis'
setwd(working_directory)
### load data
source('./Code/01_data_cleaning.R')


csf_40_42_ratio <- LIIA_baseline$`CSF.AB42/40.Ratio`
model_csf <- BMMmodel(csf_40_42_ratio, k = 2, initialValues = list(S0 = 0),
                  priors = list(kind = "independence"))
control_csf <- JAGScontrol(variables = c("mu", "tau", "eta", "S"),
                       burn.in = 3000, n.iter = 2000, seed = 10)
z_csf <- JAGSrun(csf_40_42_ratio, model = model_csf, control = control_csf)
BMMposteriori(z_csf,plot = T)
x <- seq(0,0.125,length.out=1000)
component_1 <- dnorm(x,0.06159 , sqrt(0.0002861))
component_2 <- dnorm(x, 0.09804 , sqrt(5.852e-05))
mixture_model <- 0.3221 *component_1+0.6779 *component_2
plot(x,mixture_model,lwd = 1,type = 'l')

LIIA_baseline%>%ggplot(aes(x=`CSF.AB42/40.Ratio`)) +
  geom_histogram( binwidth=0.005, fill="#69b3a2",)
  
### example

data("fish", package = "bayesmix")
fish%>%as_tibble(.)%>%ggplot(aes(x=fish)) +
  geom_histogram( binwidth=0.3, fill="#69b3a2") 
model <- BMMmodel(fish, k = 4, initialValues = list(S0 = 2),
                  priors = list(kind = "independence",
                                parameter = "priorsFish", hierarchical = "tau"))
control <- JAGScontrol(variables = c("mu", "tau", "eta", "S"),
                         burn.in = 3000, n.iter = 2000, seed = 10)
z <- JAGSrun(fish, model = model, control = control)





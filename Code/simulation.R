#source('Code/mnv.R')
source('Code/01_data_cleaning.R')
source('Code/jags.R')


library(MASS)
library(parallel)
## crate functions

generate_data <- function(sample_size, prob){
  N1 <- sample_size*prob
  N2 <- sample_size-N1
  mu1 <- c(0.1, 0.1)
  mu2 <- c(0.05, 0.08)
  vcov1 <- matrix(c(9e-05, 3e-05, 3e-05, 9e-05), nrow = 2, byrow = 2)
  sample1 <- mvrnorm(N1, mu = mu1, Sigma = vcov1 )
  sample2 <- mvrnorm(N2, mu = mu2, Sigma = vcov1 )
  sample.mvn <- rbind(sample1,sample2)%>%data.frame()
  colnames(sample.mvn) <- c('csf', 'plasma')
  #plot(sample.mvn)
  return(sample.mvn)
}
make_res_table <- function(model_freq,samples_summary){
  freq_mean_vec <- c(model_freq$parameters$mean[,1], model_freq$parameters$mean[,2])
  baye_mean_vec <- samples_summary[1]$statistics[c(136:139),1]
  lambda_vec <- c(model_freq$parameters$pro[1], 1-samples_summary[1]$statistics[135,1])
  freq_sigma_vec <- model_freq$parameters$variance$sigma[c(1,2,4)]
  baye_sigma_vec <- samples_summary[1]$statistics[c(1,2,4),1]
  
  df_res <- cbind(freq_mean_vec, c(baye_mean_vec[1], baye_mean_vec[3], baye_mean_vec[2], baye_mean_vec[4]))
  df_res <- rbind(df_res,lambda_vec, cbind(freq_sigma_vec, baye_sigma_vec))
  colnames(df_res) <- c('Frequentist', 'Bayesian')
  rownames(df_res) <- c("csf1 mean", "plasma1 mean", "csf2 mean", "plasma2 mean", 'lambda', 'Sigma csf', 'Sigma csf plasma', 'Sigma plasma')
  
  df_res%>%round(.,digits = 7)
}

## simulation setup
set.seed(1234)
n_sim <- 1000

cl <- makeCluster( detectCores())

run_simulation <- function(i, sample_size){
  model_list <- list()
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data,G = 2,verbose = F)
  model_bays <- bayesian_estimate(data)
  make_res_table
}
clusterEvalQ(cl,{
  library(MASS)
  library(tidyverse)
  library(mclust)
  library(rjags)
  NULL
})
clusterExport(cl, "generate_data")
clusterExport(cl, "bayesian_estimate")
clusterExport(cl, "model_string")
#system.time(lapply(1:10, run_simulation, 200))

system.time(model_list <- parLapply(cl,1:2, run_simulation,200))



make_res_table()

ggplot(sample.mvn, aes(x = csf)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Variable1", x = "Variable1", y = "Frequency") +
  theme_minimal()










library(MASS)
library(plyr)
library(dplyr)
library(parallel)

## crate functions

generate_data <- function(sample_size, prob){
  N1 <- sample_size*prob ## high csf 
  N2 <- sample_size-N1 ## lower csf
  mu1 <- c(0.1, 0.1)
  mu2 <- c(0.05, 0.08)
  vcov1 <- matrix(c(0.000108 , 0.000037, 0.000037 , 0.000103), nrow = 2, byrow = 2)
  sample1 <- mvrnorm(N1, mu = mu1, Sigma = vcov1 )
  sample2 <- mvrnorm(N2, mu = mu2, Sigma = vcov1 )
  sample.mvn <- rbind(sample1,sample2)%>%data.frame()
  colnames(sample.mvn) <- c('csf', 'plasma')
  sample.mvn$true_class <- c(rep('Positive', N1),rep('Negative', N2) )
  #plot(sample.mvn)
  return(sample.mvn)
}

make_res_table <- function(model_freq, samples_summary){
  freq_mean_vec <- c(model_freq$parameters$mean[,1], model_freq$parameters$mean[,2])
  baye_mean_vec <- tail(samples_summary[1]$statistics[,1], 4)
  lambda_vec <- c(model_freq$parameters$pro[1], 1-tail(samples_summary[1]$statistics[,1], 5)[1])
  freq_sigma_vec <- model_freq$parameters$variance$sigma[c(1,2,4)]
  baye_sigma_vec <- samples_summary[1]$statistics[c(1,2,4),1]
  
  df_res <- cbind(freq_mean_vec, c(baye_mean_vec[1], baye_mean_vec[3], baye_mean_vec[2], baye_mean_vec[4]))
  df_res <- rbind(df_res,lambda_vec, cbind(freq_sigma_vec, baye_sigma_vec))
  colnames(df_res) <- c('Frequentist', 'Bayesian')
  rownames(df_res) <- c("csf1 mean", "plasma1 mean", "csf2 mean", "plasma2 mean", 'lambda', 'Sigma csf', 'Sigma csf plasma', 'Sigma plasma')
  
  df_res%>%round(.,digits = 7)
}

get_class <- function(bays_df){
  z_df <- bays_df[grepl("^Z", rownames(bays_df)), ]
  return(z_df)
  }

get_theta <- function(bays_df){

  theta_df <- bays_df[-c(which(grepl("^Z", rownames(bays_model_res))==T)),]
  return(theta_df)
}
run_simulation <- function(i, sample_size){
  set.seed(i+sample_size)
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data,G = 2,verbose = F)
  model_bays <- bayesian_estimate(data)
  model_list <- list(freq = model_freq,
                     bays = model_bays,
                     res_table = make_res_table(model_freq, summary(model_bays)),
                     data = data)
  model_list
}

run_simulation_train_test <- function(i, sample_size, datapartition){
  set.seed(i+sample_size)
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data[,1:2],G = 2,verbose = F)
  train_index <- createDataPartition(1:sample_size, p = datapartition, list = T)
  data[train_index$Resample1,'istrain'] <- 1
  data[-train_index$Resample1,'istrain'] <- 0
  train <- data[train_index$Resample1,]
  test <- data[-train_index$Resample1,]
  model_bays <- bayesian_estimate(train)
  bays_df <- summary(model_bays)[[1]]%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  data[train_index$Resample1,'bayes_class'] <- classification
  model_list <- list(freq = model_freq,
                     bays = summary(model_bays),
                     res_table = make_res_table(model_freq, summary(model_bays)),
                     data = data)
  model_list
}

parallel_sim <- function(sample_size, n_sim, datapartition){
  cl <- makeCluster(6)
  clusterEvalQ(cl,{
    library(MASS)
    library(dplyr)
    library(tidyr)
    library(mclust)
    library(rjags)
    library(caret)
    NULL
  })
  clusterExport(cl, "generate_data")
  clusterExport(cl, "bayesian_estimate")
  clusterExport(cl, "model_string")
  clusterExport(cl, "make_res_table")
  clusterExport(cl, "get_class")
  model_list <- parLapply(cl,1:n_sim, run_simulation_train_test, sample_size,datapartition)
  stopCluster(cl)
  saveRDS(model_list, file = paste0('DataProcessed/samplesize',sample_size,'partition',datapartition,'traintest.RDS'))
}

## example of simulation

start <- Sys.time()
cat('start runing', as.character(start))
parallel_sim(200, 1000, 0.1)
parallel_sim(200, 1000, 0.3)
parallel_sim(200, 1000, 0.5)
parallel_sim(200, 1000, 0.8)
end <- Sys.time()
cat('end runing', as.character(end))
difftime(end, start, units = 'mins')


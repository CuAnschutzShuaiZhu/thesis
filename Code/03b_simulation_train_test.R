
source('Code/03a_simulation.R')

run_simulation_train_test <- function(i, sample_size){
  set.seed(i+sample_size)
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data[,1:2],G = 2,verbose = F)
  train_index <- createDataPartition(1:sample_size, p = 0.5, list = T)
  data[train_index$Resample1,'istrain'] <- 1
  data[-train_index$Resample1,'istrain'] <- 0
  train <- data[train_index$Resample1,]
  test <- data[-train_index$Resample1,]
  model_bays <- bayesian_estimate(train)
  bays_df <- summary(model_bays)[[1]]%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  data[train_index$Resample1,'bayes_class'] <- classification
  model_list <- list(freq = model_freq,
                     bays = model_bays,
                     res_table = make_res_table(model_freq, summary(model_bays)),
                     data = data)
  model_list
}

parallel_sim <- function(sample_size, n_sim){
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
  model_list <- parLapply(cl,1:n_sim, run_simulation_train_test, 50)
  stopCluster(cl)
  saveRDS(model_list, file = paste0('DataProcessed/samplesize',sample_size,'traintest.RDS'))
}
start <- Sys.time()
parallel_sim(200, 6)
end <- Sys.time()
difftime(end, start, units = 'mins')


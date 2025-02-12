
suppressPackageStartupMessages(source('Code/01_data_cleaning.R'))
source('Code/02a_jags.R')
source('Code/02a_jags_1d.R')
source('Code/02b_mnv.R')
source('Code/04_data analysis.R')

## example of simulation
start <- Sys.time()
cat('start runing', as.character(start),'\n')
parallel_sim(200, bayesian_estimate_uni, 10, 0.3)
parallel_sim(200, bayesian_estimate_uni, 1000, 0.5)
parallel_sim(200, bayesian_estimate_uni, 1000, 0.8)
end <- Sys.time()
cat('\nend runing', as.character(end),'\n')
difftime(end, start, units = 'mins')%>%print()

## simulation version 2

samplesize200partition0.3traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.3traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.5traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.5traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.8traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.8traintest.RDS"), get_evaluation_metric2))
cbind(train_size = c('0.3', '0.5', '0.8'), 
      rbind(samplesize200partition0.3traintest%>%col_summary(),
            samplesize200partition0.5traintest%>%col_summary(),
            samplesize200partition0.8traintest%>%col_summary()))%>%write.xlsx('DataProcessed/samplesize200partition.xlsx')



### real data

real_data_train_test <- function(f,data.fit.in, datapartition ){
  real_bays <- f(data.fit.in)
  data.fit.in$true_class <- ifelse(get_class(summary(real_bays)[[1]])[,"Mean"]%>%round(), 'Positive', 'Negative')
  train_index <- createDataPartition(1:130, p = datapartition, list = T)
  data.fit.in[train_index$Resample1,'istrain'] <- 1
  data.fit.in[-train_index$Resample1,'istrain'] <- 0
  train <- data.fit.in[train_index$Resample1,]
  test <- data.fit.in[-train_index$Resample1,]
  model_bays <- f(train)
  bays_df <- summary(model_bays)[[1]]%>%as.data.frame()
  classification <- ifelse(get_class(bays_df)[,'Mean']%>%round(), 'Positive', 'Negative')
  data.fit.in[train_index$Resample1,'bayes_class'] <- classification
  sample_real <- list(data = data.fit.in)
  get_evaluation_metric2(sample_real)
}
##### 2d
set.seed(124)
real_res <- rbind(real_data_train_test(bayesian_estimate, data.fit, 0.15),
                  real_data_train_test(bayesian_estimate, data.fit, 0.3),
                  real_data_train_test(bayesian_estimate, data.fit, 0.5),
                  real_data_train_test(bayesian_estimate, data.fit, 0.9))
real_res$train_partitation <- c(0.15,0.3, 0.5, 0.9)
real_res%>%dplyr::select(c("train_partitation","optimal_cutpoint", "Accuracy", "Sensitivity", "Specificity", 'sum_sen_spec'))%>%round(.,digits = 3)%>%
  saveRDS('./DataProcessed/real data train test partitation.RDS')


get_df_withclass <- function(index,sample_str){
  if(sample_str=='sample100'){
    sample <- sample100[[index]]
  }else if (sample_str=='sample200'){
    sample <- sample200[[index]]
  }else if (sample_str=='sample500'){
    sample <- sample500[[index]]
  }
  bays_df <- sammary(sample$bays)%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  classification <- ifelse(classification ==1, 'Negative', 'Positive')
  data <- sample$data%>%as_tibble()
  data$class <- classification
  return(data)
}

get_evaluation_metric <- function(index,sample_str){
  data <- get_df_withclass(index,sample_str)
  fit_cutpoint_Plasma <- suppressMessages(cutpointr(data = data, x =plasma, class = class))
  data%>%mutate(cutpoint_class = ifelse(plasma>fit_cutpoint_Plasma$optimal_cutpoint,'Positive','Negative'))
  class <- factor(data$class)
  class <- relevel(class, ref = 'Positive')
  real_class <- factor(c(rep('Positive',nrow(data)*0.7), rep('Negative',nrow(data)*0.3)))
  real_class <- relevel(real_class, ref = 'Positive')
  plasma_class <- factor(ifelse(data$plasma>fit_cutpoint_Plasma$optimal_cutpoint, 'Positive', 'Negative'))
  plasma_class <- relevel(plasma_class, ref = 'Positive')
  class_evaluation <- confusionMatrix(class,real_class)
  tibble(optimal_cutpoint = class_evaluation$optimal_cutpoint, Accuracy = class_evaluation$overall[1], 
         Sensitivity =class_evaluation$byClass[1], Specificity =class_evaluation$byClass[2])
  
}

col_summary <- function(data){
  apply(data, 2, function(x) {
    sprintf("%.4f (%.4f)", mean(x), sd(x))
  })
}

sample50 <- readRDS('DataProcessed/samplesize50.RDS')
sample70 <- readRDS('DataProcessed/samplesize75.RDS')
sample100 <- readRDS('DataProcessed/samplesize100.RDS')
sample200 <- readRDS('DataProcessed/samplesize200.RDS')
sample500 <- readRDS('DataProcessed/samplesize500.RDS')

## 100 sample size
fit_cutpoint_Plasma_real <- suppressMessages(cutpointr(data = data.fit2, x =`plasma`, class = class))[,c(2,5:7)]%>%
  as.data.frame()%>%round(.,digits = 4)
df_metric_50 <- do.call(rbind, lapply(1:100, get_evaluation_metric, sample_str ='sample50'))
df_metric_70 <- do.call(rbind, lapply(1:100, get_evaluation_metric, sample_str ='sample70'))
df_metric_100 <- do.call(rbind, lapply(1:100, get_evaluation_metric, sample_str ='sample100'))
df_metric_200 <- do.call(rbind, lapply(1:200, get_evaluation_metric, sample_str ='sample200'))
df_metric_500 <- do.call(rbind, lapply(1:500, get_evaluation_metric, sample_str ='sample500'))
df_metric_all <- rbind(df_metric_100%>%col_summary(), df_metric_200%>%col_summary(),df_metric_500%>%col_summary())
#df_metric_all%>%mutate(sum_sens_spec = sensitivity +specificity)
rownames(df_metric_all) <- c( 'simulated data with sample size 100', 'simulated data with sample size 200','simulated data with sample size 500')
#colnames(df_metric_all)[2] <-c('Accuracy') 
df_metric_all%>%saveRDS('DataProcessed/df class metric.RDS')






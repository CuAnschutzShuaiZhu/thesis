get_df_withclass <- function(index,sample_str){
  if(sample_str=='sample100'){
    sample <- sample100[[index]]
  }else if (sample_str=='sample200'){
    sample <- sample200[[index]]
  }else if (sample_str=='sample500'){
    sample <- sample500[[index]]
  }
  bays_df <- sample$bays%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  classification <- ifelse(classification ==1, 'Negative', 'Positive')
  data <- sample$data%>%as_tibble()
  data$class <- classification
  return(data)
}
get_evaluation_metric <- function(index,sample_str){
  get_df_withclass(index,sample_str)
  fit_cutpoint_Plasma <- suppressMessages(cutpointr(data = data, x =plasma, class = class))
  summary(fit_cutpoint_Plasma)$cutpointr[[1]][,c(2,4:8,11)]
}





sample100 <- readRDS('DataProcessed/samplesize100.RDS')
sample200 <- readRDS('DataProcessed/samplesize200.RDS')
sample500 <- readRDS('DataProcessed/samplesize500.RDS')

## 100 sample size
fit_cutpoint_Plasma_real <- suppressMessages(cutpointr(data = data.fit2, x =`Plasma.AB42/40.Ratio`, class = class))[,c(2,4:8,11)]%>%as.data.frame()
df_metric_100 <- do.call(rbind, lapply(1:100, get_evaluation_metric, sample_str ='sample100'))
df_metric_200 <- do.call(rbind, lapply(1:200, get_evaluation_metric, sample_str ='sample200'))
df_metric_500 <- do.call(rbind, lapply(1:500, get_evaluation_metric, sample_str ='sample500'))
df_metric_all <- rbind(fit_cutpoint_Plasma_real, df_metric_100%>%colMeans(), df_metric_200%>%colMeans(),df_metric_500%>%colMeans())
rownames(df_metric_all) <- c('real data with sample size 130', 'simulated data with sample size 100',
                             'simulated data with sample size 200','simulated data with sample size 500')
df_metric_all%>%saveRDS('DataProcessed/df class metric.RDS')

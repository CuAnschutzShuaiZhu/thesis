
get_evaluation_metric <- function(index,sample_str){
  if(sample_str=='sample100'){
    sample <- sample100[[index]]
  }else if (sample_str=='sample200'){
    sample <- sample200[[index]]
  }else if (sample_str=='sample500'){
    sample <- sample500[[index]]
  }
  bays_df <- sample$bays%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  classification <- ifelse(classification ==1, 'Nagative', 'Positive')
  data <- sample$data%>%as_tibble()
  data$class <- classification
  fit_cutpoint_Plasma <-suppressMessages(cutpointr(data = data, x =plasma, class = class))
  summary(fit_cutpoint_Plasma)$cutpointr[[1]]
}


sample100 <- readRDS('DataProcessed/samplesize100.RDS')
sample200 <- readRDS('DataProcessed/samplesize200.RDS')
sample500 <- readRDS('DataProcessed/samplesize500.RDS')


bays_df_100 <- sample100[[1]]$bays%>%as.data.frame()
bays_df_200 <- sample200[[1]]$bays%>%as.data.frame()
bays_df_500 <- sample500[[1]]$bays%>%as.data.frame()

a <- lapply(1:500, get_evaluation_metric, sample_str ='sample500')

sample500[[1]]$data
sample500[[3]]$data



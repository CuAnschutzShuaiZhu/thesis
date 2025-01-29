get_evaluation_metric <- function(index, sample){
  data <- sample[[index]]
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

get_evaluation_metric2 <- function(sample){
  train <- sample$data[sample$data$istrain==1,]
  test <- sample$data[sample$data$istrain==0,]
  fit_cutpoint_Plasma <- suppressMessages(cutpointr(data = train, x =plasma, class = bayes_class))
  test$cutpoint_class <- factor(ifelse(test$plasma>fit_cutpoint_Plasma$optimal_cutpoint, 'Positive', 'Negative'))
  test$true_class <- factor(test$true_class)
  class_evaluation <- confusionMatrix(test$cutpoint_class, test$true_class, positive = 'Positive')
  tibble(optimal_cutpoint = fit_cutpoint_Plasma$optimal_cutpoint, Accuracy = class_evaluation$overall[1], 
         Sensitivity =class_evaluation$byClass[1], Specificity =class_evaluation$byClass[2])
}

col_summary <- function(data){
  apply(data, 2, function(x) {
    sprintf("%.4f (%.4f)", mean(x), sd(x))
  })
}



## simulation version 2
samplesize200partition0.3traintest<- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.3traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.5traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.5traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.8traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.8traintest.RDS"), get_evaluation_metric2))
cbind(train_size = c('0.3', '0.5', '0.8'), 
      rbind(samplesize200partition0.3traintest%>%col_summary(),
            samplesize200partition0.5traintest%>%col_summary(),
            samplesize200partition0.8traintest%>%col_summary()))%>%write.xlsx('DataProcessed/samplesize200partition.xlsx')




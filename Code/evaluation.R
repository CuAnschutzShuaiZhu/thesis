library(pROC)

model_list100 <- readRDS('DataProcessed/samplesize100.RDS')
model_list100[[1]]$freq$classification

par(mfrow = c(2,2))
hist(model_list100[[1]]$data$csf)
hist(model_list100[[1]]$data$plasma)
plot(model_list100[[1]]$data)

data <- model_list100[[1]]$data%>%as_tibble()
threshold1 <- 0.08
threshold2 <- 0.09
true_class <-  c(rep(1,70), rep(2,30))
pred_class <-  cut(data$plasma,breaks = c(-Inf, threshold1, threshold2, Inf),
                   labels = c(2, 0, 1))

roc_obj <- roc(as.numeric(true_class == 1), as.numeric(pred_class == 1))
roc_obj <- roc(true_class, data$plasma)



plot(roc_obj)






## frequentest

## specify data


model_freq <- Mclust(data.fit,G = 2)
classification <- model_freq$classification
summary(model_freq)

model_freq$parameters$pro
model_freq$parameters$mean
model_freq$parameters$variance$sigma


#plot(model_freq, what = "density", type = "persp")
plot_freq <- function(model_freq, data){
  par(mfrow = c(2,2))
  plot(densityMclust(data), what = "BIC")
  plot(model_freq, what = "classification")
  plot(model_freq, what = "uncertainty")
}
plot_freq(model_freq, data.fit)

data.fit2 <- data.fit
data.fit2$class <- classification
data.fit2 <- data.fit2%>%mutate(class = ifelse(class==1, 'Positive', 'Negative'))
colnames(data.fit2 ) <- c("csf", "plasma", 'class')

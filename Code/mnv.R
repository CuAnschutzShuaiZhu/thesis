working_directory <- "C:\\Users\\zhushu\\OneDrive\\Graduate File\\Course\\Thesis"
## frequentest

## specify data
model_freq <- Mclust(data.fit,G = 2)
classification <- model_freq$classification
#plot(model_freq, what = "density", type = "persp")
par(mfrow = c(2,2))
plot(densityMclust(data.fit), what = "BIC")
plot(model_freq, what = "classification")
plot(model_freq, what = "uncertainty")

summary(model_freq)
model_freq$parameters$pro
model_freq$parameters$mean
model_freq$parameters$variance$sigma



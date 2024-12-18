
rm(list = ls())

sample100 <- readRDS('DataProcessed/samplesize100.RDS')

bays_df <- sample100[[1]]$bays%>%as.data.frame()

bays_z_df <- bays_df[grepl("^Z", rownames(bays_df)), ]
bays_z_df <- round(bays_z_df, digits = 5)

bays_z_df[,'Mean']

plot_freq(sample100[[1]]$freq, sample100[[1]]$data)


sample100[[1]]$freq$uncertainty%>%round(.,digits = 5)

sample100[[1]]$freq$classification%>%round(.,digits = 5)


df_class <- cbind(bayes_class= bays_z_df[,'Mean'], ture_class = c(rep(0, 70), rep(1,30)))%>%as.data.frame()%>%
  mutate(bayes_class = ifelse(bayes_class> 0.6, 1,0), 
         bayes_class = factor(bayes_class),
         ture_class = factor(ture_class, levels = c(0,1), labels = c('CSF Amyloid positive', 'CSF Amyloid negative')))
head(df_class)

table1( ~ bayes_class | ture_class, data=df_class)


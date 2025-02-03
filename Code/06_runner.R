
## example of simulation
start <- Sys.time()
cat('start runing', as.character(start))
parallel_sim(200, 1000, 0.3)
parallel_sim(200, 1000, 0.5)
parallel_sim(200, 1000, 0.8)
end <- Sys.time()
cat('\nend runing', as.character(end))
difftime(end, start, units = 'mins')%>%print()

## simulation version 2
#samplesize200partition0.1traintest <- do.call(rbind, lapply(readRDS("DataProcessed/samplesize200partition0.1traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.3traintest <- do.call(rbind, lapply(readRDS("DataProcessed/train test/samplesize200partition0.3traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.5traintest <- do.call(rbind, lapply(readRDS("DataProcessed/train test/samplesize200partition0.5traintest.RDS"), get_evaluation_metric2))
samplesize200partition0.8traintest <- do.call(rbind, lapply(readRDS("DataProcessed/train test/samplesize200partition0.8traintest.RDS"), get_evaluation_metric2))
cbind(train_size = c('0.3', '0.5', '0.8'), 
      rbind(samplesize200partition0.3traintest%>%col_summary(),
            samplesize200partition0.5traintest%>%col_summary(),
            samplesize200partition0.8traintest%>%col_summary()))%>%write.xlsx('DataProcessed/samplesize200partition.xlsx')




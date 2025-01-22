### histogram of real data

plot_csf <- data.fit%>%ggplot(aes(x = csf))+geom_histogram()
plot_plasma <- data.fit%>%ggplot(aes(x = plasma))+geom_histogram()
g <- arrangeGrob(plot_csf, plot_plasma, ncol=2)
ggsave('Figures/histogram of csf and plasma.png',g, width = 20, height = 10, units = 'cm')

### scatter plot of csf and plasma
png('Figures/scatter plot with different sample sizes.png', res = 100, width = 20, height = 20, units = 'cm')
par(mfrow = c(2,2))
plot(data.fit, main = 'True data with sample size 130')
plot(sample100[[1]]$data, main = 'simulated data with sample size 100')
plot(sample200[[1]]$data, main = 'simulated data with sample size 200')
plot(sample500[[1]]$data, main = 'simulated data with sample size 500')
dev.off()
### scatter plot

### scatter plot of csf and plasma with classificaton

rbind(
cbind(data.fit2, size = rep('real data with sample size 130',130)),
cbind(get_df_withclass(1, 'sample100'), size = rep('simulated data with sample size 100',100)),
cbind(get_df_withclass(1, 'sample200'), size = rep('simulated data with sample size 200',200)),
cbind(get_df_withclass(1, 'sample500'), size = rep('simulated data with sample size 500',500))
)%>%ggplot()+
  geom_point(aes(x = csf, y = plasma, colour = class))+
  geom_hline(data = rownames_to_column(df_metric_all, var = 'size'), aes(yintercept = optimal_cutpoint), color = "red", linetype = "dashed") +
  facet_wrap(~size)
ggsave('Figures/scatter plot of csf and plasma with classificaton.png',dpi = 300, width = 30, height = 30, units = 'cm')

## trace plot of bayes parameter
png('Figures/traceplot.png', res = 100, width = 30, height = 30, units = 'cm')
par(mfrow = c(12, 12), mar = c(1, 1, 1, 1))
theta_vec <- bayes[[1]]%>%colnames()
for(i in 1:length(theta_vec)){
  plot(bayes[[1]][,theta_vec[i]]%>%as.vector(), type = 'l', main = theta_vec[i], ylab = '',xaxt = "n",)
}
dev.off()





# JAGS model code for two-component bivariate normal mixture

model_string <- "
model {
  # Likelihood
  for (i in 1:N) {
    Z[i] ~ dbern(lambda)  # Latent variable for component assignment
    
    # Shared covariance matrix (Sigma_inv) for both components
    Y[i,1:2] ~ dmnorm(mu[Z[i] + 1,], Sigma_inv[,])  # Precision matrix used here

  }
  
  # Priors
  # Mixture proportion (lambda)
  lambda ~ dbeta(1, 1)  # Flat prior for mixture proportion

  # Shared precision matrix (inverse of covariance matrix)
  Sigma_inv[1:2,1:2] ~ dwish(R[,], nu)  # Wishart prior for the precision matrix
  Sigma[1:2,1:2] <- inverse(Sigma_inv[,])  # Covariance matrix is the inverse of precision
  
  # Priors for the means (each component has different covariance for the prior)

  mu[1, 1:2] ~ dmnorm(mu_prior[1,], cov_mu_prior[,])
  mu[2, 1:2] ~ dmnorm(mu_prior[2,], cov_mu_prior[,])
}
"
# Prepare the data for JAGS
bayesian_estimate <- function(data){
  data <- data[,1:2]
  data_jags <- list(
    Y = data,  # Nx2 data matrix
    N = nrow(data),  # Number of observations
    mu_prior = matrix(c(0.05, 0.095, 0.1, 0.095), nrow = 2, byrow = 2),  # Means of the prior for mu (2x2)
    cov_mu_prior = diag(2)*10^-5,  # Different covariance for the priors of mu
    # cov_mu_prior_2 = diag(2)*10^8, 
    R = diag(2)*10^-4,  # Scale matrix for the Wishart prior for Sigma_inv
    nu = 2  # Degrees of freedom for the Wishart prior
  )
  set.seed(123)
  # Initialize JAGS model
  model <- jags.model(textConnection(model_string), data = data_jags, n.chains = 4, quiet = T)
  
  # Burn-in period
  update(model, 200, progress.bar = 'none')
  # Draw samples from posterior
  
  invisible(capture.output(
    samples <- coda.samples(model, variable.names = c("mu", "Sigma", "lambda", 'Z'), quiet = T,n.iter= 2000)
                          ))
  
  # Check summary of posterior distributions

  return((samples))
}


## crate functions

generate_data <- function(sample_size, prob){
  N1 <- sample_size*prob ## high csf 
  N2 <- sample_size-N1 ## lower csf
  mu1 <- c(0.1, 0.1)
  mu2 <- c(0.05, 0.08)
  vcov1 <- matrix(c(0.000108 , 0.000037, 0.000037 , 0.000103), nrow = 2, byrow = 2)
  sample1 <- mvrnorm(N1, mu = mu1, Sigma = vcov1 )
  sample2 <- mvrnorm(N2, mu = mu2, Sigma = vcov1 )
  sample.mvn <- rbind(sample1,sample2)%>%data.frame()
  colnames(sample.mvn) <- c('csf', 'plasma')
  sample.mvn$true_class <- c(rep('Positive', N1),rep('Negative', N2) )
  #plot(sample.mvn)
  return(sample.mvn)
}

make_res_table <- function(model_freq, samples_summary){
  freq_mean_vec <- c(model_freq$parameters$mean[,1], model_freq$parameters$mean[,2])
  baye_mean_vec <- tail(samples_summary[1]$statistics[,1], 4)
  lambda_vec <- c(model_freq$parameters$pro[1], 1-tail(samples_summary[1]$statistics[,1], 5)[1])
  freq_sigma_vec <- model_freq$parameters$variance$sigma[c(1,2,4)]
  baye_sigma_vec <- samples_summary[1]$statistics[c(1,2,4),1]
  
  df_res <- cbind(freq_mean_vec, c(baye_mean_vec[1], baye_mean_vec[3], baye_mean_vec[2], baye_mean_vec[4]))
  df_res <- rbind(df_res,lambda_vec, cbind(freq_sigma_vec, baye_sigma_vec))
  colnames(df_res) <- c('Frequentist', 'Bayesian')
  rownames(df_res) <- c("csf1 mean", "plasma1 mean", "csf2 mean", "plasma2 mean", 'lambda', 'Sigma csf', 'Sigma csf plasma', 'Sigma plasma')
  
  df_res%>%round(.,digits = 7)
}

get_class <- function(bays_df){
  z_df <- bays_df[grepl("^Z", rownames(bays_df)), ]
  return(z_df)
}
get_element <- function(sample){
  #z <- sample$bays[[1]][grepl("^Z", rownames(sample$bays[[1]])), ][,'Mean']
  lambda <- tail(sample$bays[[1]][,'Mean'],5)[1]
  mean <- matrix(tail(sample$bays[[1]][,'Mean'],4), byrow = F, nrow = 2)%>%round(.,digits = 5)
  sigma <- matrix(head(sample$bays[[1]][,'Mean'],4), byrow = F, nrow = 2)%>%round(.,digits = 5)
  list(lambda = lambda, mean = mean,cov = sigma)
}

run_simulation <- function(i, sample_size){
  set.seed(i+sample_size)
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data,G = 2,verbose = F)
  model_bays <- bayesian_estimate(data)
  model_list <- list(freq = model_freq,
                     bays = model_bays,
                     res_table = make_res_table(model_freq, summary(model_bays)),
                     data = data)
  model_list
}

run_simulation_train_test <- function(i, sample_size, datapartition){
  set.seed(i+sample_size)
  data <- generate_data(sample_size, 0.7)
  model_freq <- Mclust(data[,1:2],G = 2,verbose = F)
  train_index <- createDataPartition(1:sample_size, p = datapartition, list = T)
  data[train_index$Resample1,'istrain'] <- 1
  data[-train_index$Resample1,'istrain'] <- 0
  train <- data[train_index$Resample1,]
  test <- data[-train_index$Resample1,]
  model_bays <- bayesian_estimate(train)
  bays_df <- summary(model_bays)[[1]]%>%as.data.frame()
  classification <- get_class(bays_df)[,'Mean']%>%round()
  data[train_index$Resample1,'bayes_class'] <- classification
  model_list <- list(freq = model_freq,
                     bays = summary(model_bays),
                     res_table = make_res_table(model_freq, summary(model_bays)),
                     data = data)
  model_list
}

parallel_sim <- function(sample_size, n_sim, datapartition){
  cl <- makeCluster(6)
  clusterEvalQ(cl,{
    library(MASS)
    library(dplyr)
    library(tidyr)
    library(mclust)
    library(rjags)
    library(caret)
    NULL
  })
  clusterExport(cl, "generate_data")
  clusterExport(cl, "bayesian_estimate")
  clusterExport(cl, "model_string")
  clusterExport(cl, "make_res_table")
  clusterExport(cl, "get_class")
  model_list <- parLapply(cl,1:n_sim, run_simulation_train_test, sample_size,datapartition)
  stopCluster(cl)
  saveRDS(model_list, file = paste0('DataProcessed/samplesize',sample_size,'partition',datapartition,'traintest.RDS'))
}




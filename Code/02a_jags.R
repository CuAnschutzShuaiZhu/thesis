## Bayesian
library(rjags)
library(coda)
# JAGS model code for two-component bivariate normal mixture

model_string <- "
model {
  # Likelihood
  for (i in 1:N) {
    Z[i] ~ dbern(lambda)  # Latent variable for component assignment
    
    # Shared covariance matrix (Sigma_inv) for both components
    Y[i,1:2] ~ dmnorm(mu[Z[i] + 1,], Sigma_inv[,])  # Precision matrix used here
    
  # Posterior predictive distribution for Y
    # Z_pred[i] ~ dbern(lambda)
    # Y_pred[i, 1:2] ~ dmnorm(mu[Z_pred[i] + 1,], Sigma_inv[,])  # Using same parameters
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
    nu = 10  # Degrees of freedom for the Wishart prior
  )
  set.seed(123)
  # Initialize JAGS model
  model <- jags.model(textConnection(model_string), data = data_jags, n.chains = 4, quiet = T)
  
  # Burn-in period
  update(model, 200, progress.bar = 'none')
  04
  # Draw samples from posterior
  
  invisible(capture.output(
    samples <- coda.samples(model, variable.names = c("mu", "Sigma", "lambda", 'Z'), quiet = T,n.iter= 2000)
                          ))
  
  # Check summary of posterior distributions

  return((samples))
}

# sample <- run_simulation_train_test(5,200,0.8)
# sample$bays[[1]][,'Mean']
# matrix(tail(sum,4), byrow = F, nrow = 2)
# get_evaluation_metric2(sample)


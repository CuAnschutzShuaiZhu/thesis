
# JAGS model code for two-component univariate normal mixture

model_string_uni <- "
model {
  # Likelihood
  for (i in 1:N) {
    Z[i] ~ dbern(lambda)  # Latent variable for component assignment
    
    # Shared covariance matrix (Sigma_inv) for both components
    Y[i] ~ dnorm(mu[Z[i] + 1], Sigma_inv)  # Precision matrix used here

  }
  
  # Priors
  # Mixture proportion (lambda)
  lambda ~ dbeta(2, 1)  # Flat prior for mixture proportion

  # Shared precision matrix (inverse of covariance matrix)
  Sigma_inv ~ dgamma(sigma_prior[1], sigma_prior[2])  # Wishart prior for the precision matrix
  Sigma <- inverse(Sigma_inv)  # Covariance matrix is the inverse of precision
  
  # Priors for the means (each component has different covariance for the prior)

  mu[1] ~ dnorm(mu_prior[1], cov_mu_prior)
  mu[2] ~ dnorm(mu_prior[2], cov_mu_prior)

}
"
# Prepare the data for JAGS
bayesian_estimate_uni <- function(data){
  data_1d <- data[,1]%>%unlist()%>%as.vector()
  data_jags <- list(
    Y = data_1d,  # N vector
    N = nrow(data),  # Number of observations
    mu_prior = c(0.06, 0.1),  # Means of the prior for mu 
    cov_mu_prior = 10^-5,  # Different covariance for the priors of mu
    sigma_prior = c(10^-4, 10^-4) 
  )
  # Initialize JAGS model
  model <- jags.model(textConnection(model_string_uni), data = data_jags, n.chains = 4, quiet = T)
  
  # Burn-in period
  update(model, 1000, progress.bar = 'none')
  # Draw samples from posterior
  
  invisible(capture.output(
    samples <- coda.samples(model, variable.names = c("mu", "Sigma", "lambda",'Z'), quiet = T,n.iter= 5000)
  ))
  
  # Check summary of posterior distributions
  
  return((samples))
}




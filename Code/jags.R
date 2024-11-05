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
  }
  
  # Priors
  # Mixture proportion (lambda)
  lambda ~ dbeta(1, 1)  # Flat prior for mixture proportion

  # Priors for the means (each component has different covariance for the prior)

  mu[1, 1:2] ~ dmnorm(mu_prior[1,], cov_mu_prior_1[,])
  mu[2, 1:2] ~ dmnorm(mu_prior[2,], cov_mu_prior_2[,])


  # Shared precision matrix (inverse of covariance matrix)
  Sigma_inv[1:2,1:2] ~ dwish(R[,], nu)  # Wishart prior for the precision matrix
  Sigma[1:2,1:2] <- inverse(Sigma_inv[,])  # Covariance matrix is the inverse of precision
}
"

# Prepare the data for JAGS
data_jags <- list(
  Y = sample.mvn,  # Nx2 data matrix
  N = nrow(data.fit),  # Number of observations
  mu_prior = matrix(c(0.1, 0.1, 0.05, 0.08), nrow = 2, ncol = 2, byrow = T),  # Means of the prior for mu (2x2)
  cov_mu_prior_1 = diag(2)*10^6,  # Different covariance for the priors of mu
  cov_mu_prior_2 = diag(2)*10^6, 
  R = diag(2)*10^-3,  # Scale matrix for the Wishart prior for Sigma_inv
  nu = 3  # Degrees of freedom for the Wishart prior
)
set.seed(123)
# Initialize JAGS model
model <- jags.model(textConnection(model_string), data = data_jags, n.chains = 4)

# Burn-in period
update(model, 1000)

# Draw samples from posterior
samples <- coda.samples(model, variable.names = c("mu", "Sigma", "Z", "lambda"), 
                        n.iter= 5000)

# Check summary of posterior distributions
samples_summary <- summary(samples)
samples_summary[1]$statistics[c(1:4,135:139),]
samples_summary[2]$quantiles[c(1:4,135:139),]%>%round(.,3)

## plot
#par(mfrow = c(2,2))
#traceplot(samples[, "mu"])








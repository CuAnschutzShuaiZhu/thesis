######################################
### Title: 02_bayesian_sampling
### Date: 06/20/2024
### Author: Shuai Zhu
### Description: thesis 
######################################

library(bayesmix)
library(bmixture)
### setting working directory
working_directory <-  'C:\\Users\\zhushu\\OneDrive\\Graduate File\\Course\\Thesis'
setwd(working_directory)
### load data
source('./Code/01_data_cleaning.R')


csf_40_42_ratio <- LIIA_baseline$`CSF.AB42/40.Ratio`
model_csf <- BMMmodel(csf_40_42_ratio, k = 2, initialValues = list(S0 = 2),
                  priors = list(kind = "independence"))
control_csf <- JAGScontrol(variables = c("mu", "tau", "eta", "S"),
                       burn.in = 3000, n.iter = 2000, seed = 10)
fit_baye_csf <- JAGSrun(csf_40_42_ratio, model = model_csf, control = control_csf)
fit_baye_csf_results <- z_csf$results%>%as_tibble()
csf_classification <- as.data.frame(z_csf$results)[2000,][1:131]%>%unlist()%>%as.vector()
cutpointr(data = LIIA_baseline, x = CSF_ABratio, class=csf_classification)



BMMposteriori(z_csf,plot = T)
x <- seq(0,0.125,length.out=1000)
component_1 <- dnorm(x,0.06159 , sqrt(0.0002861))
component_2 <- dnorm(x, 0.09804 , sqrt(5.852e-05))
mixture_model <- 0.3221 *component_1+0.6779 *component_2
plot(x,mixture_model,lwd = 1,type = 'l')
df_model = tibble(x = x,y = mixture_model)
LIIA_baseline%>%ggplot(aes(x=`CSF.AB42/40.Ratio`)) +
  geom_histogram( binwidth=0.005, fill="#69b3a2")+
  geom_line(data=df_model, aes(x=x, y=y))
ggsave(paste(working_directory,'Code/images/desirable career of dentistry.png',sep = '/'),dpi = 800, width = 12, height = 6, units = "in")
### example

mu <- c(-2.75, 2.75);
sigma <- c(1, 1);
lambda <- 0.4
set.seed(689934)
N <- 1000
z <- rbinom(N, 1, lambda) + 1;
y <- rnorm(N, mu[z], sigma[z]);
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



##############################
# Required Libraries
library(MASS)

# Function to initialize parameters
initialize_params <- function(X, K) {
  N <- length(X)
  mu <- runif(K, min(X), max(X))
  sigma <- rep(sd(X), K)
  pi <- rep(1/K, K)
  z <- sample(1:K, N, replace = TRUE)
  
  list(mu = mu, sigma = sigma, pi = pi, z = z)
}

# Gibbs Sampler
gibbs_sampler <- function(X, K, n_iter = 1000) {
  N <- length(X)
  params <- initialize_params(X, K)
  
  for (iter in 1:n_iter) {
    # Step 1: Sample z_i
    z_prob <- sapply(1:K, function(k) {
      params$pi[k] * dnorm(X, mean = params$mu[k], sd = params$sigma[k])
    })
    params$z <- apply(z_prob, 1, function(prob) sample(1:K, 1, prob = prob/sum(prob)))
    
    # Step 2: Update mu_k, sigma_k, pi_k
    for (k in 1:K) {
      X_k <- X[params$z == k]
      N_k <- length(X_k)
      
      # Update mu_k
      params$mu[k] <- rnorm(1, mean = mean(X_k), sd = params$sigma[k] / sqrt(N_k))
      
      # Update sigma_k^2
      params$sigma[k] <- sqrt(1 / rgamma(1, shape = N_k/2, rate = sum((X_k - params$mu[k])^2)/2))
      
      # Update pi_k
      params$pi <- rdirichlet(1, table(params$z) + 1)
    }
  }
  
  return(params)
}

# Simulated Data
set.seed(123)
X <- csf_40_42_ratio

# Run Gibbs Sampler
K <- 2
result <- gibbs_sampler(X, K, n_iter = 10000)

# Final Parameters
result$mu
result$sigma
result$pi



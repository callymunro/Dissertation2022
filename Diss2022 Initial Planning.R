# Dissertation 2022: Initial Simple Simulation.


# Initial calculation of one stage sample size.
Sample_Size <- function(alpha, beta, delta, sigma, attr) {
  n <- (qnorm(1 - alpha) - qnorm(beta))^2 * (2 * sigma^2) / delta ^ 2
  n_adj <- n * 1/ (1 - attr) 
  return(c(n, n_adj))
}

# Initial calculation of one stage sample size - BINOMIAL
Sample_Size_Bin <- function(alpha, beta, delta, theta) {
  n <- (qnorm(1 - alpha) - qnorm(beta))^2 * theta * (1 - theta) / delta ^ 2
  n_adj <- n * 1/ (1 - attr) 
  return(c(n, n_adj))
}

# Simulation for simple endpoint.
# Need a null hypothesis:
  # H0: delta = 0
  # H1: delta \ne 0
    #two-sided test.
  # H0: delta = 0
  # H1: delta > 0
    #one-sided test.


# In terms of a function (need to generate under the null hypothesis):
TypeI_Error <- function(alpha, beta, delta, sigma, mu, n) {
  # Generate data from the normal distribution.
  x <- rnorm(n)
  
  # Calculate the one sample t-test value for each data.
  ttest <- (x - mu) / (sigma / sqrt(n))
  
  # Critical value of the normal distribution.
  critval <- qt(alpha, n - 1)
  
  # TypeI error: Rejecting the null when it is true.
  TypeI <- sum((ttest <= qnorm(alpha)) / n)
  
  # Different approach - not sure how this works.
  TypeI_alt <- sum((t.test(x, mu = mu)))
  
  return(TypeI)
}


# Stop/Go criteria for null hypothesis (normally distributed / continuous endpoint):
Go <- function(alpha, mu_0, mu, sigma, n) {
  
  # Generate observations from the normal distribution.
  x <- rnorm(n = n, mean = mu, sd = sigma)
  
  # # Calculate SD from the data:
  # sigma_hat <- sd(x)
  
  # Perform a t-test (or z-test if sigma is known) on the mean of the obs.
  ttest <- (mean(x) - mu_0) / (sigma / sqrt(n))
  
  # Critical value of the t-distribution.
  critval <- qt(alpha, n - 1)
  
  # TypeI error: rejected hypothesis under null.
  TypeI  <- ttest > critval
  
  return(TypeI)
}

# TypeI error:
Go(alpha, mu_0, mu = mu_0, sigma, n)

# 1 - TypeII error:
Go(alpha, mu_0, mu = mu, sigma, n)


# For binomially distibuted data:

Go_Binom <- function(alpha, sigma, mu, n) {
  
  # Generate observations from the binomial distribution.
  x <- rbinom(n = n, size = 1, prob= mu)
  
  # Estimated variance
  sigma_hat <- mu * (1 - mu) # I think?
  
  # Test statistic.
  ttest <- (mean(x) - 0) / (sigma_hat / sqrt(n))
  
  # Critical value - testing from chi-squared?
  # Need to finish here and look into what test is appropriate.
}


# Two-arm trial:

Go2arm <- function(alpha, mu_0, mu, sigma, n) {
  
  # Generate observations from the normal distribution.
  x1 <- rnorm(n = n, mean = mu_0, sd = sigma)
  x2 <- rnorm(n = n, mean = mu, sd = sigma)
  
  # # Calculate SD from the data:
  # sigma_hat <- sd(x)
  
  # Perform a t-test (or z-test if sigma is known) on the mean of the obs.
  ttest <- (abs(mean(x2) - mean(x1)) - mu_0) / (sigma / sqrt(n))
  
  # Critical value of the t-distribution.
  critval <- qt(alpha, n - 1)
  
  # TypeI error: rejected hypothesis under null.
  TypeI  <- ttest > critval
  
  return(TypeI)
}

# x1 control arm (mean = 0)
# Saying second arm is better than the first for "Go".
# Either stick with the control or co ahead with the recommended treatment.

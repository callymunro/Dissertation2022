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
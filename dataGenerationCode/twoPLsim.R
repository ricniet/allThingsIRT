
# Data generation code for the 2PL model (or 1PL/Rasch if twoPL == FALSE)

## Function parameters:

### J = number of items
### N = number of individuals
### sd.theta = SD of (normal) theta distribution
### twoPL = if F, sets all J alphas to 1.0

## Output:

### trueValues = list of true (generating) values (beta, alpha, theta)
### data = (N x J) matrix of observed item responses

twoPLsim <- function(J = 20, N = 500, sd.theta = 1, twoPL = T, seed = 11071987) {
  
  set.seed(seed)
  
  beta <- rnorm(J)
  theta <- rnorm(N, sd.theta)
  alpha <- if (twoPL==T) {
    rlnorm(J, sdlog=.4)} else {
      rep(1, J)
    }
  
  logits <- array(NA,c(N,J))
  
  for (i in 1:N) {
    for (j in 1:J) {
      logits[i,j] <- alpha[j] * (theta[i] - beta[j])
    }
  }
  
  responseProb <- 1 / ( 1 + exp(-logits))
  
  Y <- array(NA,c(N,J))
  
  for (i in 1:N) {
    for (j in 1:J) {
      Y[i,j] <- rbinom(n = 1, size = 1, prob = responseProb[i,j])
    }
  }
  
  output        <- list()
  output$trueValues <- list(beta = beta,
                            alpha = alpha,
                            theta = theta)
  output$data   <- Y
  output
}



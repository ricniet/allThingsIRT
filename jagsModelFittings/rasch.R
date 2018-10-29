library(R2jags)

source('./dataGenerationCode/twoPLsim.R')

# declare model 

jagsModel <- function() {
  for (i in 1:N) {
    for (j in 1:J) {
      Y[i,j] ~ dbern(prob[i,j])
      logit(prob[i,j]) <- theta[i] - beta[j]
    }
  }
  
  for (i in 1:N) {
    theta[i] ~ dnorm(muTheta, precTheta)
  }
  
  for (j in 1:J) {
    beta[j] ~ dnorm(muBeta, precBeta)
  }
  
  # transformations
  
  precTheta <- pow(sdTheta, -2)
  precBeta <- pow(sdBeta, -2)
}

# declare data and save for R2jags::jags.parallel()
data <- twoPLsim(twoPL = F)
Y <- data$data
J <- ncol(Y)
N <- nrow(Y)

muTheta <- 0
sdTheta <- 1

muBeta <- 0
sdBeta <- 5

jagsDat <- list("Y","J","N","muTheta","sdTheta",
                "muBeta","sdBeta")

# initial values

jagsInits <- function() {
  list(
    theta = rnorm(N,0,2),
    beta = rnorm(J,0,2)
  )
}

# parameters to save

jagsParams <- c("theta","beta")

# MCMC specs

jagsObj <- jags.parallel(
  model.file = jagsModel,
  data = jagsDat,
  inits = jagsInits,
  parameters.to.save = jagsParams,
  n.iter = 3000,
  n.burn = 1500,
  n.chains = 3
)

jagsSummary <- data.frame(round(jagsObj$BUGSoutput$summary,4))

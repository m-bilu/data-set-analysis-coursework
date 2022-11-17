set.seed(20880910)

satis <- read.csv('C:/Users/mbila/Downloads/satisfaction.csv')

sim <- function(beta2, beta3) {
  
  allps <- rep(0, 1000)
  
  for(i in 1:1000) {
  
    sigma <- 10
    beta0 <- 150
    beta1 <- -1
    
    y <- satis['Satisfaction']
    age <- satis['Age']
    severity <- satis['Severity']
    stress <- satis['Stress']
    
    ## Here we are defning our true model, and simulating 46 yi observations
    M <- lm(data = satis, # look for variables with these names in "satis"
            formula = Satisfaction ~ Age + Severity + Stress)
    simy <- simulate(M, 1)
    
    ## We fit the vector yhat using the betahat values from summary(M)
    ## yhat = X * betahat, where betahat is a vector of coefficients
    XM <- cbind(1, age, severity, stress) # design matrix
    yhatM <- as.matrix(XM) %*% as.vector(coef(M))
  
    ## Fitted for reduced model
    K <- lm(data = satis, # look for variables with these names in "satis"
            formula = Satisfaction ~ Age)
    
    ## We fit the vector yhat using the betahat values from summary(M)
    ## yhat = X * betahat, where betahat is a vector of coefficients
    XK <- cbind(1, age) # design matrix
    yhatK <- as.matrix(XK) %*% as.vector(coef(K))
    
    # Saving B1hat and a1hat
    agecoefs <- c(coef(M)[2], coef(K)[2])
    
    agesd <- c(coef(summary(M))['Age', "Std. Error"], 
               coef(summary(K))['Age', "Std. Error"])
    
    ## Our H0 is beta2 = beta3
    ## == 2P(T > |Tobs| )
    V <- vcov(M)
    betahat2 <- as.vector(coef(M))[3]
    betahat3 <- as.vector(coef(M))[4]
    sigmahat2 <- sum((y-yhatM)^2)/(length(t(y))-2)
    Tobs <- (betahat3 - betahat2 - 0)/sqrt(sigmahat2*(V[3,3] + V[4,4] - 2*V[3,4]))
    p <- 2*pt(q = abs(Tobs),               # |Tobs|
            df = length(t(y))-2,                    # degrees of freedom
            lower.tail = FALSE)
    
    allps[i] <- p
    
  }
  
  return(allps)
  
  
  
  
}
sim(1, 2)

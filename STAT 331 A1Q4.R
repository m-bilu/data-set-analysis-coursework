set.seed(20880910)

#### A1Q4a

x <- c(1, 10, 8, 1, 3, 4, 5, 5, 6, 7)

simulation <- function(b0, b1, sig) {
  
  alpha <- 0.05
  n <- length(x)
  pvals <- c()
  
  for (i in 1:2000) {
    
    y <- rnorm(10, b0 + b1*x, sig)
    M <- lm(y ~ x)
    b0hat <- M$coefficients[[1]]
    b1hat <- M$coefficients[[2]]
    yhat <- b0hat + b1hat*x
    Sxx <- sum((x - mean(x))^2)
    
    Tobs <- b1hat/(sig/sqrt(Sxx))
    
    p <- 2*pt(abs(Tobs), df = n - 2, lower.tail = FALSE)
    #p <- summary(M)$coefficients[,4][2]
    pvals <- c(pvals, p)
    
  }
  
  return(pvals)
  
}

#### A1Q4b
sim1 <- simulation(1, 0.25, 1)

hist(sim1, main = 'Simulation of 2000 p values')

length(sim1[sim1 < 0.05]) / length(sim1)

## -> This proportion is 0.4335. Hence, 
##    43.4% of the samples reject the hypothesis


#### A1Q4c
sim2 <- simulation(1, 0, 1)

hist(sim2, main = 'Simulation of 2000 p values')

length(sim2[sim2 < 0.05]) / length(sim2)

## -> This proportion is 0.0185. Hence, 
##    1.9% of the samples reject the hypothesis

#### A1Q4d
sim3 <- simulation(1, 0.25, 0.5)

hist(sim3, main = 'Simulation of 2000 p values with Beta1 = 0.25, sig = 0.5')

length(sim3[sim3 < 0.05]) / length(sim3)

## -> This proportion is 0.9755. Hence, 
##    97.6% of the samples reject the hypothesis

sim4 <- simulation(1, 0, 0.5)

hist(sim4, main = 'Simulation of 2000 p values with Beta1 = 0, sig = 0.5')

length(sim4[sim4 < 0.05]) / length(sim4)

## -> This proportion is 0.024. Hence, 
##    2.4% of the samples reject the hypothesis

## -> As the variance of the simulated yi's decreased, a significantly larger
##    number of samples rejected the hypothesis when b1 != 0. This is because
##    a larger variance allows for a higher probability that simulated data
##    goes farther from the mean, and a lower variance allows for a higher 
##    probability that simulated data remains closer to the mean. 

## A1Q4e

## -> Increasing the sample size would increase the proportions of reject 
##    samples with b1 != 0. THis is because by the formula for variance, 

############################


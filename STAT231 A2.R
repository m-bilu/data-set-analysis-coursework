install.packages('tidyverse')

filepath <- "C:/Users/mbila/Downloads/gas.csv"

gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
)

gas
gasS <- dplyr::filter(gas,car=="S")
dplyr::glimpse(gasS)

n <- length(gasS$gas)

mu <- (1/n)*(sum(gasS$gas))
sig <- ((1/(n-1))*(sum((gasS$gas - mu)^2)))^(1/2)
mu
sig
sig^2

## Q2f
set.seed(54534)
n <- nrow(gasS)
B <- 1e04 # Number of simulations to do, an arbitrary "large" number
mn <- 47 # Known
ss <- 11 # Known
# Generate a sample of sample means
samps <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  samps[b] <- mean(thesample)
}


## i
set.seed(53498)

par(mfrow=c(1,1))

hist(samps, prob = TRUE)
x <- seq(from = 0, to = 100, by=0.1)
lines(x, dnorm(x, mean = mn, sd = ss/sqrt(n)), type = "l")

## ii

# Generate a sample of sample variances
vsamps <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  vsamps[b] <- var(thesample)
}

par(mfrow=c(1,1))

hist(vsamps, prob = TRUE)
x <- seq(from = 0, to = 350, by=0.1)
lines(x, ((n-1)/ss^2)*dchisq(((n-1)/(ss^2))*x, n-1))

## iii
set.seed(75294)
tsamps <- numeric(B)
for (b in 1:B) {
  thesample <- rnorm(n,mn,ss)
  
  rmean <- mean(thesample)
  rsd <- sqrt(var(thesample))
  
  tsamps[b] <- (rmean - mn)/(rsd/sqrt(n))
}

par(mfrow=c(1,1))

hist(tsamps, prob = TRUE)
x <- seq(from = -100, to = 100, by=0.1)
lines(x, dt(x, n-1))

qnorm(0.95, 0, 1)

## Q3
mean(gasS$gas) - qnorm(0.975, 0, 1)*(11/sqrt(n))
mean(gasS$gas) + qnorm(0.975, 0, 1)*(11/sqrt(n))

(qt(0.975, n-1)*sd(gasS$gas)/1.5)^2

mean(gasS$gas) - qt(0.975, n-1)*sqrt(var(gasS$gas)/n)
mean(gasS$gas) + qt(0.975, n-1)*sqrt(var(gasS$gas)/n)

(n-1)*(var(gasS$gas))/qchisq(0.975, n-1)
(n-1)*var(gasS$gas)/qchisq(0.025, n-1)

sqrt((n-1)*(var(gasS$gas))/qchisq(0.975, n-1))
sqrt((n-1)*var(gasS$gas)/qchisq(0.025, n-1))

#Q3e

x <- seq(40, 60, by = 0.1)

L <- function(muu) {
  exp(-20*(mu - muu)^2/242) 
}

R <- function(muu) {
  exp((10/121)*(2*mu*(muu - mean(gasS$gas)) + (mean(gasS$gas))^2 - muu^2))
}

plot(x, A(x), type = "l")
horiz <- function(x){
    qnorm(0.975, 0, 1) + 0*x
}
lines(x, horiz(x))

## Q3 f

A <- function(muu) {
  -2*log(R(muu), base = exp(1))
}

m2 <- 0.15/L

uniroot(function(x) (R(x) - 0.15/L(mu)), c(40, 50))
uniroot(function(x) (R(x) - 0.15/L(mu)), c(50, 60))

## Q3 g

set.seed(435)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
zval <- qnorm(.975)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - zval * ss/sqrt(n) # Treating ss as known, so it appears in the interval
  intupper[b] <- mean(samp) + zval * ss/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
covr_mean <- mean(covr)
covr_mean

# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}  


## ii
set.seed(435)
n <- nrow(gasS)
mn <- 47 # Known
ss <- 11 # Known
B <- 1e04
intlower <- intupper <- covr <- numeric(B)
zval <- qnorm(.975)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - zval * sd(samp)/sqrt(n) # Treating ss as unknown
  intupper[b] <- mean(samp) + zval * sd(samp)/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
covr_mean <- mean(covr)
covr_mean

# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
} 



## iii AGAIN
set.seed(435)
n <- nrow(gasS)
intlower <- intupper <- covr <- numeric(B)
zval <- qnorm(.975)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - zval * sd(samp)/sqrt(n) # Treating ss as unknown
  intupper[b] <- mean(samp) + zval * sd(samp)/sqrt(n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mn & mn <= intupper[b]
}
covr_mean <- mean(covr)
covr_mean

# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mn)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
} 



## iii
intlower <- intupper <- covr <- numeric(B)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- sd(samp)*sqrt((n-1)/qchisq((1+0.95)/2, n-1)) # Treating ss as known, so it appears in the interval
  intupper[b] <- sd(samp)*sqrt((n-1)/qchisq((1-0.95)/2, n-1))
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mu & mu <= intupper[b]
}
covr_mean <- mean(covr)
covr_mean

# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mu)
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}  


## Q 3 J I

mean(gasS$gas) - qt(0.975, n-1)*sd(gasS$gas)*sqrt(1+1/n)
mean(gasS$gas) + qt(0.975, n-1)*sd(gasS$gas)*sqrt(1+1/n)

## II
set.seed(4723698)
intlower <- intupper <- covr <- numeric(B)
for (b in 1:B) {
  samp <- rnorm(n,mn,ss)
  intlower[b] <- mean(samp) - qt(0.975, n-1)*sd(samp)*sqrt(1+1/n) # Treating ss as known, so it appears in the interval
  intupper[b] <- mean(samp) + qt(0.975, n-1)*sd(samp)*sqrt(1+1/n)
  # Regardless of whether we pretend mu is known or not, calculate the coverage
  # using the true value of mu.
  covr[b] <- intlower[b] <= mean(gasS$gas) & mean(gasS$gas) <= intupper[b]
}
covr_mean <- mean(covr)
covr_mean

# Plot the first 100 simulated intervals
numplot <- 100
plot(c(intlower[1],intupper[1]),c(1,1),
     xlim = c(min(intlower),max(intupper)),
     ylim = c(1,numplot),
     type='l',lty='dashed')
abline(v = mean(gasS$gas))
for (i in 1:numplot) {
  if (covr[i] == 1) {
    lines(c(intlower[i],intupper[i]),c(i,i))
  } else {
    lines(c(intlower[i],intupper[i]),c(i,i),col='red')
  }
}  



install.packages('tidyverse')

filepath <- "C:/Users/mbila/Downloads/gas.csv"

gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
)

gas
gasS <- dplyr::filter(gas,car=="S")
gasH <- dplyr::filter(gas,car=="H")
gasS
sum(gasS$gas)/20
mean(gasS$gas)

# Generate some data
set.seed(424242)
x <- rnorm(10)
# Create a function to compute the normal log-likelihood for these at a vector of
# values of mu
loglik <- function(mu) {
  ll <- numeric(length(mu))
  for (i in 1:length(ll)) ll[i] <- sum(dnorm(x,mean=mu[i],sd=1,log=TRUE))
  ll
}
# Construct the values that you want to plot it at. I'll plot 1000 values, to
# get a nice, smooth curve.
xx <- seq(-3,3,length.out=1000)
# Compute the values to be plotted
vv <- loglik(xx)
# Plot it
plot(xx,vv,type='l') # I didn't print this in the assignment.
# Can put a vertical line at any point you like:
abline(v = 0)


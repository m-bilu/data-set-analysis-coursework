
## Y is population
Y <- c(80,60,75,70,75,80,60,68,85,85,90,90,80,90,95,110,100,
          105,110,115)

## Q2a
Ybar <- mean(Y)
Yvar <- var(Y)

## Q2b i
set.seed(20880910)
y <- sample(Y, 5)
ybar <- mean(y)
yvar <- var(y)

## Q2b iv
sampleAvs <- rep(0, 1000)
sampleVars <- rep(0, 1000)

for (i in 1:1000) {
  sample <- sample(Y, 5)
  sampleAvs[i] <- mean(sample)
  sampleVars[i] <- var(sample)
}


hist(sampleAvs)

sum(sampleAvs)/1000
sum(sampleVars)/1000

## Q2c i, ii

Z <- Y >= 100
z <- y >= 100
mean(z)
var(z)







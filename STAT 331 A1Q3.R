### A1Q3a

berries <- read.csv("C:/Users/mbila/Downloads/berries.csv")

y <- berries$chewiness
x <- berries$sugar
n <- length(x)

plot(x, y, 
     xlab = 'Sugar Content', ylab = 'Chewiness', 
     main = 'Sugar content vs chewiness of berries')

### A1Q3b
M <- lm(y ~ x)
summary(M)

b0hat <- M$coefficients[[1]]
b1hat <- M$coefficients[[2]]
b0hat
b1hat

abline(M)

## Interpretations:

##  b0hat is the expected chewiness of a fruit from the database when
##  there is no sugar content.

##  b1hat is the linear change in expected chewiness with respect to change
##  in sugar content.


### A1Q3c

### Hypothesis Testing:
# H0: beta1 = 0
# test statistic: T = beta1.hat/s1
#   where s1 = sigmahat / sqrt(Sxx)
# T | H0 ~ t_(n-2)
# pval = P( |T| > |Tobs|  |  H0 )
tstat <- coef(summary(M))[, "t value"][[2]]
pval <- coef(summary(M))[, "Pr(>|t|)"][[2]]
tstat
pval


# Conclusion:
#   There is significant evidence against the hypothesis H0: B1 = 0,
#   There is evidence against the statement, "there is no statistically 
#   significant linear relationship in between sugar levels and chewiness"


### A1Q3d

# 95% CI for E[yi | xi = 220 g/L] = B0 + B1(220) = mu(220):
x0 <- 220
mu0hat <- b0hat + b1hat * x0
sighat <- sqrt(sum((M$residuals)^2)/(n-2))
qval <- qt(0.025, df = n - 2, lower.tail = FALSE)
mu0sig <- sighat * (1/n + (x0 - mean(x))^2/sum((x - mean(x))^2))^(1/2)
CI <- mu0hat + c(-1, 1) * qval * mu0sig
CI


### A1Q3e

xnew <- 110

## Prediction Estimate
ynewhat <- b0hat + b1hat*xnew


## Prediction Interval

qvalnew <- qt((1-0.95)/2, df = n-2, lower.tail=FALSE) # level = 1-alpha quantile
munew <- b0hat + b1hat * xnew # mu.star = yhat.star: plugin estimate
signew <- sqrt(sum((M$residuals)^2)/(n-2)) 
musignew <- signew *
  sqrt(1 + 1/n + (xnew - mean(x))^2/sum((x - mean(x))^2)) # standard error
PI <- cbind(L = munew - qvalnew * musignew,
        U = munew + qvalnew * musignew)
PI

#### Conclusion:
``
ynewhat


install.packages('tidyverse')

filepath <- "C:/Users/mbila/Downloads/gas.csv"

gas <- readr::read_csv(
  filepath,
  col_types = readr::cols("c","n","n",readr::col_date(format = "%m/%d/%Y"))
)



dplyr::glimpse(gas)

n <- nrow(gas)

## 1c redo

mod <- lm(gas ~ km, data = gas)

# a conf int
ahat - qt((1+0.95)/2, n-2)*se*sqrt(1/n + (mean(gas$km))^2/sxx)
ahat + qt((1+0.95)/2, n-2)*se*sqrt(1/n + (mean(gas$km))^2/sxx)

# b conf int
bhat - qt((1+0.95)/2, n-2)*se/sqrt(sxx)
bhat + qt((1+0.95)/2, n-2)*se/sqrt(sxx)

# errors
se*sqrt(1/n + (mean(gas$km))^2/sxx)
se/sqrt(sxx)

## 1e

sxx <- sum((gas$km - mean(gas$km))^2)
syy <- sum((gas$gas - mean(gas$gas))^2)
sxy <- sum((gas$km - mean(gas$km))*(gas$gas - mean(gas$gas)))

se <- sqrt(((1/(n-2)) * (syy - bhat*sxy)))

bhat <- sxy / sxx
ahat <- mean(gas$gas) - bhat*mean(gas$km)

teststat <- abs(ahat)/(se*sqrt((1/n) + (mean(gas$km))^2/sxx))

pval <- 2*(1-pt(teststat, n-2))

## 1f

conf_func_left <- function(p, x) {
  
  ahat + bhat*(x) - qt(1-p/2, n-2)*se*sqrt(1/n + ((x - mean(gas$km))^2)/sxx)
  
}

conf_func_right <- function(p, x) {
  
  ahat + bhat*(x) + qt(1-p/2, n-2)*se*sqrt(1/n + ((x - mean(gas$km)))^2/sxx)

}

pred_func_left <- function(p, x) {
  
  ahat + bhat*(x) - qt(1-p/2, n-2)*se*sqrt(1 + 1/n + ((x - mean(gas$km))^2)/sxx)
  
}

pred_func_right <- function(p, x) {
  
  ahat + bhat*(x) + qt(1-p/2, n-2)*se*sqrt(1 + 1/n + ((x - mean(gas$km)))^2/sxx)
  
}

## 1g

xx <- seq(200,800,length.out=1e03)
datforpred <- data.frame(km = xx)
mypredictions_confidence <- predict(mod,datforpred,interval="prediction")
myconfidence <- predict(mod,datforpred,interval="confidence")
plot(gas$km,gas$gas,pch=20)
lines(xx,mypredictions_confidence[ ,'fit'])
lines(xx,mypredictions_confidence[ ,'lwr'])
lines(xx,mypredictions_confidence[ ,'upr'])
lines(xx,mypredictions_confidence[ ,'lwr'])
lines(xx,myconfidence[ ,'lwr'])
lines(xx,myconfidence[ ,'upr'])

## 1h I

muhat <- ahat + bhat*(gas$km)
resid <- gas$gas - muhat
stdresid <- resid/se

muhat
resid
stdresid

## 1h II

plot(gas$km, stdresid)
lines(gas$km, 0*stdresid)

## 1h III

plot(muhat, stdresid)
lines(muhat, 0*stdresid)

## 1h IV

qqnorm(stdresid)
qqline(stdresid)

## 1h V

## Here we are making a frequency table for all gas recordings. 
##   The expected frequency of gas consumed = b is n * P(Yi = b).
## n = 40
## Yi ~ G(mu(xi), sigma)
## Hence, when calculating expected probability, we use
##   MLE mu(x) = ahat + bhat*(mean(xi))

muhat_mle <- ahat + bhat * mean(gas$km)

  expFreq <- function(b) {
    
    if (b < 19) { ## CDF, n * P(Yi < 19) where Yi ~ G(mu(xi), sigma)
      
    n * pnorm(b, muhat_mle, shat)
    
  } else if (19 <= b && b <= 41) { ## Interval Prob, n * (p(b) - p(19))
    
    n * (pnorm(b, muhat_mle, shat) - pnorm(19, muhat_mle, shat))
    
  } else if (42 <= b && b <= 44) {
    
    n * (pnorm(b, muhat_mle, shat) - pnorm(42, muhat_mle, shat))
    
  } else if (b >= 65) {
    
    n * (1 - pnorm(65, muhat_mle, shat))
    
  } else if (45 <= b && b <= 49) {
    
    n * (pnorm(b, muhat_mle, shat) - pnorm(45, muhat_mle, shat))
    
  } else if (50 <= b && b <= 54) {
    
    n * (pnorm(b, muhat_mle, shat) - pnorm(50, muhat_mle, shat))
    
  } else if (55 <= b && b <= 64) {
    
    n * (pnorm(b, muhat_mle, shat) - pnorm(55, muhat_mle, shat))
    
  }
  
  
  
}


  cutpoints <- c(19, 42, 45, 50, 55, 65)
  intervals <- paste0(paste0("[",cutpoints,",",sep=""),
                      c(cutpoints[2:length(cutpoints)],"+\U221E"),")",sep="")
  intervals <- c(paste0("(-\U221E,",cutpoints[1],")"),intervals)
  observed <- c(0,table(cut(gas$gas,cutpoints)),0)
  
  mn <- mean(gas$gas)
  n <- nrow(gas)
  ss <- sqrt((n-1)/n)*sd(gas$gas)
  expected <- nrow(gas) * ( pnorm(c(cutpoints,Inf),mn,ss) - pnorm(c(-Inf,cutpoints),mn,ss))
  tab_notfull <- data.frame(
    Interval = c(intervals,"Total"),
    Observed = c(observed,nrow(gas)),
    Expected = c(expected[1:2],rep(0,length(intervals)-2),nrow(gas))
  )
  tab_full <- tab_notfull
  tab_full$Observed <- c(observed,nrow(gas))
  tab_full$Expected <- c(expected,nrow(gas))
  knitr::kable(tab_full,digits = 2)
  

## Q2b
  
S <- gas[gas$car == 'S',]
S <- S[,2:4]

H <- gas[gas$car == 'H',]
H <- H[,2:4]

## Conf interval for sigma_s

d <- qchisq(0.975, length(S$gas) - 1)
c <- qchisq(0.025, length(S$gas) - 1)

sqrt((length(S$gas) - 1)*(var(S$gas))/d) # left
sqrt((length(S$gas) - 1)*(var(S$gas))/c) # right

## Conf interval for sigma_h

d <- qchisq(0.975, length(H$gas) - 1)
c <- qchisq(0.025, length(H$gas) - 1)

sqrt((length(H$gas) - 1)*(var(H$gas))/d) # left
sqrt((length(H$gas) - 1)*(var(H$gas))/c) # right

## Q2c

n1 <- nrow(S)
n2 <- nrow(H)

yhatS = mean(S$gas)
yhatH = mean(H$gas)
sp2 <- ((n1 - 1)*var(S$gas) + (n2 - 1)*var(H$gas)) / (n1 + n2 - 2)

yhatS - yhatH - qt(0.975, n1 + n2 - 2)*(sqrt(sp2))*sqrt(1/nrow(S) + 1/nrow(H))

yhatS - yhatH + qt(0.975, n1 + n2 - 2)*(sqrt(sp2))*sqrt(1/nrow(S) + 1/nrow(H))

## Q2d

yhatS - yhatH - qnorm(0.975)*sqrt(var(S$gas)/n1 + var(H$gas)/n2)

yhatS - yhatH + qnorm(0.975)*sqrt(var(S$gas)/n1 + var(H$gas)/n2)

## Q2e

2*(1-pt(abs(yhatS - yhatH)/(sqrt(sp2*(1/n1 + 1/n2))), n1 + n2 - 2))
  
    

energyapp <- read.csv("C:/Users/mbila/Downloads/energyapp.csv")

## Q3 b

covariateCleaner <- function(vstar) {
  
  # This will initially hold all energyapp data with all columns of csv file
  # At each iteration of while loop, will loose columns (multicollinear 
  # covariates) until best version of energyapp dataset is held in Mcur
  # Will be returned at the end.
  curdata <- energyapp[, 2:ncol(energyapp)]
  n = nrow(energyapp)
  
  # Loop will continue until no more covariates (columns in Mcur) with 
  # VIF > vstar
  while(1) {
    
    VIF <- rep(0, ncol(curdata)) # uncalculated, initialized
    
    myrange <- 1:(ncol(curdata))
    
    for (j in myrange) {
      
      # xj would be the jth covariate of the current model
      #   -> the j+1th column of curdata dataframe, since 
      #   first column is outcome
      xj <- curdata[, j]
      xjname <- colnames(curdata)[j]
      
      # For covariate xj, VIFj = 1/(1-Rj^2)
      #   Rj^2 = SS(Reg)/SS(Total) from the model 
      #   -> xj = X-j * alpha + epsilon
      Msubj <- lm(as.formula(paste(xjname, '~', '.')), data = curdata)
      
      Rj2 <- summary(Msubj)$r.squared
      vifj <- 1/(1-Rj2)
      
      VIF[j] <- vifj
      
    }
    
    ## By now, we have calculated the VIF of all covariates in the current model
    ## All values stored in VIF vector
    ##    Now we find largest value s.t VIFj > vstar,
    ##    The associated jth covariate gets removed from curdata, and loop 
    ##    runs again UNTIL VIF vector is has no j s.t VIFj > vstar
    
    badvifs <- VIF[VIF > vstar]
    # Loop will terminate at this if statement:
    # If no more covariates with VIF > vstar, break
    if (length(badvifs) == 0) {
      break
    }
    
    badvariate <- which(badvifs == max(badvifs))
    curdata <- curdata[,-badvariate]
    
  }
  
 return(curdata)
}

## Q3 c
reduced <- covariateCleaner(10)
colnames(reduced)

## Q3 d
Mfull <- lm(appEuse ~ ., data=energyapp)
reduced <- cbind(energyapp$appEuse, reduced)
Mred <- lm(energyapp$appEuse ~ ., data=reduced)

R2 <- cbind(summary(Mfull)$r.squared, summary(Mred)$r.squared)
aR2 <- cbind(summary(Mfull)$adj.r.squared, summary(Mred)$adj.r.squared)
AIC <- cbind(AIC(Mfull), AIC(Mred))
BIC <- cbind(BIC(Mfull), BIC(Mred))
p <- cbind(pf(summary(Mfull)$fstatistic[1],
              summary(Mfull)$fstatistic[2],
              summary(Mfull)$fstatistic[3],
              lower.tail=FALSE), pf(summary(Mred)$fstatistic[1],
                                    summary(Mred)$fstatistic[2],
                                    summary(Mred)$fstatistic[3],
                                    lower.tail=FALSE))

sol <- rbind(R2, aR2, AIC, BIC, p)
rownames(sol) <- c("R2", "adj. R2", "AIC", "BIC", "p-values")
colnames(sol) <- c("Full model", "Reduced Model")
sol

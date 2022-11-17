aquifer <- read.csv("C:/Users/mbila/Downloads/aquifer.csv") 

## First, we fit porosity to each single covariate in aquifer.csv
Mtype <- lm(aquifer$porosity ~ aquifer$type)
Mdensity <- lm(aquifer$porosity ~ aquifer$density)
Mresidue <- lm(aquifer$porosity ~ aquifer$residue)
Mglength <- lm(aquifer$porosity ~ aquifer$glength)

btype1hat <- Mtype$coefficients[[1]]
btype2hat <- Mtype$coefficients[[2]]
btype3hat <- Mtype$coefficients[[3]]
btype4hat <- Mtype$coefficients[[4]]
bdensityhat <- Mdensity$coefficients[[2]]
bresiduehat <- Mresidue$coefficients[[2]]
bglengthhat <- Mglength$coefficients[[2]]

## Q2b: We have our hypothesis H0: (btype2, btype3, btype4) = (0, 0, 0),
# using an F-statistic (3, 31-4)

df <- Mtype$df

# F-statistic using subset of MLE of full model (betastar)
bstarhat <- coef(Mtype)[2:4] # subset of MLE
# vcov(Mfull) = sigma.hat^2 * (X'X)^{-1}
bstar.ve <- vcov(Mtype)[2:4,2:4] # vcov => sigma.hat^2 * V
# Fobs = (betastar.hat' V^{-1} betastar.hat / q) / sigma.hat^2
# note: betastar.ve already includes the denominator
Fobs <- t(bstarhat) %*% solve(bstar.ve, bstarhat) / length(bstarhat)

# p-value: large values of Fobs are more evidence against H0
pf(Fobs, df1 = length(bstarhat), df2 = df, lower.tail = FALSE)

## Q2c: We shall now fit multiple regression model:
Mfull <- lm(aquifer$porosity ~ aquifer$type + 
              aquifer$density + aquifer$residue + aquifer$glength)
Mfull$coefficients

## Q2d: Another test of no association:
# H0: (b_bellefont, b_nittany, b_stonehedge) = (0, 0, 0)

df <- Mfull$df

# F-statistic using subset of MLE of full model (betastar)
bstarhat <- coef(Mfull)[2:4] # subset of MLE
# vcov(Mfull) = sigma.hat^2 * (X'X)^{-1}
bstar.ve <- vcov(Mfull)[2:4,2:4] # vcov => sigma.hat^2 * V
# Fobs = (betastar.hat' V^{-1} betastar.hat / q) / sigma.hat^2
# note: betastar.ve already includes the denominator
Fobs <- t(bstarhat) %*% solve(bstar.ve, bstarhat) / length(bstarhat)

# p-value: large values of Fobs are more evidence against H0
pf(Fobs, df1 = length(bstarhat), df2 = df, lower.tail = FALSE)

## Q2e: We write a model including interaction between residue and glength
Minter1 <- lm(aquifer$porosity ~ aquifer$type + 
                aquifer$density + aquifer$residue + 
                aquifer$glength + aquifer$residue:aquifer$glength)
coef(Minter1)

# Technically, testing for no interaction would test if the coefficient 
# of the residue_i * glength_i term is 0. Intuitively, this is testing if
# the effect of residue on porosity is different for different values of glength,
# keeping all other covariates constant. If there is interaction, then the 
# difference in E(y) as residue increments by 1 (keeping everything else constant)
# would change depending on the constant value of glength. 
###
# H0: b7 == 0 against HA: b7 != 0, using a T-statistic
# p = 2*P(T > |Tobs|), where Tobs = (b7hat - 0)/sqrt(var(b7hat))
b7hat <- coef(summary(Minter1))[[8,1]]
seb7hat <- coef(summary(Minter1))[[8, 2]]
Tobs <- b7hat/seb7hat
p <- pt(abs(Tobs), df = Minter1$df, lower.tail = FALSE)

#########
## Q2f: We write a model including interaction between residue and type
Minter2 <- lm(aquifer$porosity ~ aquifer$type + 
                aquifer$density + aquifer$residue + 
                aquifer$glength + aquifer$residue:aquifer$type)
coef(Minter2)

# Technically, testing for no interaction would test if the coefficients 
# for the terms residue_i*I(Bellefonte)_i, residue_i*I(Nittany)_i and residue_i*I(Stonehedge)
# are all 0. Intuitively, this is testing if the effect of residue on porosity 
# is different for different values of glength, keeping all other covariates constant. 
# If there is interaction, then the difference in E(y) as residue increments by 1
# (keeping everything else constant) would change depending which indicator variable
# is non-zero ie. depending on which of the 4 limestones are in use. 
###
# H0: (b7, b8, b9) == (0, 0, 0), using an F-statistic
bstarhat <- coef(Minter2)[8:10]
Vstarinv <- solve(vcov(Minter2))[8:10, 8:10] # includes denom
Fobs <- ((t(bstarhat)) %*% Vstarinv %*% bstarhat)/3
p <- pf(Fobs, df1 = 3, df2 = Minter2$df, lower.tail = FALSE)


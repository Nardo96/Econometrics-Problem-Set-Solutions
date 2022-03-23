library(readstata13)
library(MASS)


kt <- read.dta13("kt_data.dta")

year10 <- subset(kt, year == 10)

firstreg <- lm(data = year10, wage ~ educ)
summary(firstreg)

secondreg <- lm(data = year10, wage ~ I(exper^2))
summary(secondreg)

thirdreg <- lm(data = year10, wage ~ educ + I(exper^2) + abil + Moth_ed + Fath_ed)
summary(thirdreg)
resid <-residuals(thirdreg)

year10 <- as.matrix(year10)

## calculate inverse matrix (X'x)^-1
X <- t(year10)
XX <- X %*% year10
inverse <- solve(XX)

minusierror <- as.numeric(vector())

## Calculate leverage score hii for each observation and calculate leave-one-out error
## through the formula e-i = ei(1-hii)^-1

for (i in 1:nrow(year10)){
  x_it <- t(as.numeric(year10[i, ])) 
  x_i <- as.numeric(year10[i, ])
  hii <- x_it %*% inverse %*% x_i
  minusierror <- append(minusierror, resid[i]*(1-hii)^(-1))
}

plot(resid - minusierror)

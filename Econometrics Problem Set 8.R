library(readstata13)
library(sandwich)
library(lmtest)

kt <- read.dta13("kt_data.dta")
year10 <- subset(kt, year == 10)

regression <- lm(data = year10, wage ~ educ + abil + I(exper^2) + Fath_ed)

residuals <- resid(regression)
summary(residuals)
plot(residuals)

## Non-robust Errors
summary(regression)

## Robust HC1 Errors
coeftest(regression, vcov. = vcovHC(regression, type = "HC1"))

## Robust HC3 Errors
coeftest(regression, vcov. = vcovHC(regression, type = "HC3"))



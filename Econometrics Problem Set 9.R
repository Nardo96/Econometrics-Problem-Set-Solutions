library(readstata13)
library(sandwich)
library(lmtest)
library(plm)


kt <- read.dta13("kt_data.dta")
year10 <- subset(kt, year == 10)

regression <- lm(data = year10, wage ~ educ + abil + I(exper^2) + Fath_ed)

summary(regression)

residuals <- resid(regression)
plot(residuals)
plot(regression)

bptest(regression)

## Non-robust Errors
summary(regression)

## Robust HC1 Errors
coeftest(regression, vcov. = vcovHC(regression, type = "HC1"))

## Robust HC3 Errors
coeftest(regression, vcov. = vcovHC(regression, type = "HC3"))


clustered <- plm(data = year10, wage ~ educ + abil + I(exper^2) + Fath_ed, model = "pooling", index = c('loc'))

# clustered SE method by Richard Bluhm
# compute State like df-adjustment
G <- length(unique(year10$loc))
N <- length(year10$loc)
dfa <- (G/(G - 1)) * (N - 1)/clustered$df.residual

#display with VCE and df-adjustment
loc_c_vcov <- dfa * vcovHC(clustered, type = "HC0", cluster = "group")
coeftest(clustered, vcov. = loc_c_vcov)





library(readstata13)
library(fastDummies)

## create dataset
cpsdata <- read.dta13("cps09mar.dta") 

## compute hourly wages
cpsdata$hourly_wage <- cpsdata$earnings / cpsdata$hours / cpsdata$week 

## convert  earnings to log earnings
logwage <- cpsdata
logwage$earnings <- log(cpsdata$earnings) 
logwage$hourly_wage <- log(cpsdata$hourly_wage)

## regress hourly wage on female

femalelm <- lm(hourly_wage ~ female, data = logwage)
summary(femalelm)

## create gender subsets and calculate mean conditional on marital status
fsubset <- subset(logwage, female == 1)
msubset <- subset(logwage, female == 0)

flogwagemarital <- by(fsubset$hourly_wage, fsubset$marital, mean )
mlogwagemarital <- by(msubset$hourly_wage, msubset$marital, mean )

## create one dataframe with 14 values for expected wage, corresponding to the 14 combinations of
## gender and marital status
cmean <- cbind(flogwagemarital, mlogwagemarital)
conditionalmean <- data.frame(y = c(cmean[, "flogwagemarital"], cmean[, "mlogwagemarital"]))

conditionalmean <- cbind(conditionalmean, female = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0))
conditionalmean <- cbind(conditionalmean, marital1 = c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0))
conditionalmean <- cbind(conditionalmean, marital2 = c(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0))
conditionalmean <- cbind(conditionalmean, marital3 = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0))
conditionalmean <- cbind(conditionalmean, marital4 = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
conditionalmean <- cbind(conditionalmean, marital5 = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0))
conditionalmean <- cbind(conditionalmean, marital6 = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0))
conditionalmean <- cbind(conditionalmean, marital7 = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1))

conditionalmean

## regress expected wage on dummy variables female & marital(1-7) along with interaction terms

conditionalmeanlm <- lm(y ~ female + marital1 + female*marital1 + marital2 + female*marital2 +
                          marital3 + female*marital3 + marital4 + female*marital4 + marital5 + 
                          female*marital5 + marital6 + female*marital6 + marital7 + female*marital7,
                        data= conditionalmean)
conditionalmeanlm

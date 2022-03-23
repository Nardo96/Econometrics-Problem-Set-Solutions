library(readstata13)
library(plyr)

## create dataset
cpsdata <- read.dta13("cps09mar.dta") 

## compute hourly wages
cpsdata$hourly_wage <- cpsdata$earnings / cpsdata$hours / cpsdata$week 
head(cpsdata)

## convert annual earnings to log annual earnings
logwage <- cpsdata
logwage$earnings <- log(cpsdata$earnings) 

## compute mean of log wages conditional on region
logwage_region <- by(data = logwage$earnings, logwage$region, mean)
logwage_region

## compute mean of log wages conditional on education
logwage_educ <- by(logwage$earnings, logwage$education, mean) 
logwage_educ

## create dataframe from logwage_educ
logwage_df <- cbind(logwage_educ) 

## plot log annual earnings against education
plot(logwage_df, xlab = "Education", ylab = "Log Annual Wage")

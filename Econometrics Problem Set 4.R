library(readstata13)

injury <- read.dta13("INJURY.DTA")

## Create appropriate subsets to calculate mean for kentucky
kentucky <- subset(injury, ky == 1)

kentucky_high <- subset(kentucky, highearn == 1)
kentucky_low <- subset(kentucky, highearn == 0)

kentucky_high_after <- subset(kentucky_high, afchnge == 1)
kentucky_high_before <- subset(kentucky_high, afchnge == 0)
kentucky_low_after <- subset(kentucky_low, afchnge == 1)
kentucky_low_before <- subset(kentucky_low, afchnge == 0)

kymeanha <- mean(kentucky_high_after$durat)
kymeanhb <- mean(kentucky_high_before$durat)
kymeanla <- mean(kentucky_low_after$durat)
kymeanlb <- mean(kentucky_low_before$durat)

## same as above for michigan
michigan <- subset(injury, mi == 1)

michigan_high <- subset(michigan, highearn == 1)
michigan_low <- subset(michigan, highearn == 0)

michigan_high_after <- subset(michigan_high, afchnge == 1)
michigan_high_before <- subset(michigan_high, afchnge == 0)
michigan_low_after <- subset(michigan_low, afchnge == 1)
michigan_low_before <-subset(michigan_low, afchnge == 0)

mimeanha <- mean(michigan_high_after$durat)
mimeanhb <- mean(michigan_high_before$durat)
mimeanla <- mean(michigan_low_after$durat)
mimeanlb <- mean(michigan_low_before$durat)

## reconstruct the results of Table 4 for mean duration
table <- data.frame('High Earning Before Increase' = c(kymeanhb, mimeanhb), 
                    'High Earning After Increase'= c(kymeanha, mimeanha), 
                    'Low Earning Before Increase' = c(kymeanlb, mimeanlb), 
                    'Low Earning After Increase' = c(kymeanla, mimeanla))
table <- cbind(table, '(2) - (1)' = table$High.Earning.After.Increase -
                                    table$High.Earning.Before.Increase)
table <- cbind(table, '(4) - (3)' = table$Low.Earning.After.Increase - 
                                    table$Low.Earning.Before.Increase)
table <- cbind(table, 'Difference in Differences' = table$`(2) - (1)`-table$`(4) - (3)`)
table

## create a linear model as done for Table 6 column (i)
durationlm <- lm(data = kentucky, ldurat ~ afchnge + highearn + afchnge*highearn + lprewage +lprewage
                 *highearn + male + married + lage + manuf + construc + head + neck + upextr + trunk +
                   lowback + lowextr + occdis)
summary(durationlm)


library(readstata13)

## create dataset
card95 <- read.dta13("Card1995.dta")

card95$exp <- (card95$age76 - card95$ed76) + card95$ed76*(card95$age76 - card95$ed76)

lmwageexp <- lm(card95$lwage76 ~ card95$exp, na.action=na.exclude)
lmwageexp

lmwageexp2 <- lm(card95$lwage76 ~ card95$exp + I(card95$exp^2), na.action=na.exclude)
lmwageexp2

test1 <- card95[, c("lwage76", "exp")]
test1$predicted <- predict(lmwageexp)
test1$residuals <- residuals(lmwageexp)
test1 <- na.omit(test1)
cor(test1)

test2 <- card95[, c("lwage76", "exp")]
test2$predicted <- predict(lmwageexp2)
test2$residuals <- residuals(lmwageexp2)
test2$exp2 <- test2$exp^2
test2 <- na.omit(test2)
cor(test2)

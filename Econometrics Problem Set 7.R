library(readstata13)
library(MASS)


kt <- read.dta13("kt_data.dta")

year10 <- subset(kt, year == 10)


firstreg <- lm(data = year10, wage ~ educ + I(exper^2) + abil + Fath_ed)
summary(firstreg)

step1 <- lm(data = year10, wage ~ I(exper^2) + abil + Fath_ed)
step2 <- lm(data = year10, educ ~ I(exper^2) + abil + Fath_ed)

resid1 <- resid(step1)
resid2 <- resid(step2)

residuals <- as.data.frame(cbind(resid1, resid2))

secondreg <- lm(data = residuals, resid1 ~ resid2)
summary(secondreg)

thirdreg <- lm(data = year10, wage ~ I(exper^2) + abil + Fath_ed)
resid3 <- resid(thirdreg) 

year10withresid <- cbind(year10, resid3)
fourthreg <- lm(data = year10, resid3 ~ educ)
summary(fourthreg)


ttests <- vector()


## k = 1
  
alphaestimate <- vector()
betaestimate <- vector()
for (i in 1:1000) {
  
x <- rnorm(n = 100, mean = 1)
e <- rnorm(n = 100)

f <- function(x) {
  1/sqrt(2*pi)*exp((-1/2)*x^2)
}

f2 <- function(x, k) {
  (x^k)*f(x)
}

f3 <- function(k) {
 integrate(f2, -Inf, Inf, k)
  
}

n <- vector()

for (i in 1:100) {
  expect <- f3(1)
  expect2 <- f3(2)
  
  expect <- as.numeric(unlist(expect)[1])
  expect2 <- as.numeric(unlist(expect2)[1])
  
  n[i] <- (e[i]^1 - expect)/(expect2 - expect^2)^(1/2)
  
}
y <- x + n
dataset <- as.data.frame(cbind(y, x, n))

model <- lm(data = dataset, y ~ x + n)

alphaestimate <- append(alphaestimate, as.numeric(unlist(model[1])[1]))
betaestimate <- append(betaestimate, as.numeric(unlist(model[1])[2]))

}
## ttests <- append(ttests, t.test(betaestimate, mu=1))
summary(betaestimate)


## k = 2



alphaestimate <- vector()
betaestimate <- vector()
for (i in 1:1000) {
  
  x <- rnorm(n = 100, mean = 1)
  e <- rnorm(n = 100)
  
  f <- function(x) {
    1/sqrt(2*pi)*exp((-1/2)*x^2)
  }
  
  f2 <- function(x, k) {
    (x^k)*f(x)
  }
  
  f3 <- function(k) {
    integrate(f2, -Inf, Inf, k)
    
  }
  
  n <- vector()
  
  for (i in 1:100) {
    expect <- f3(2)
    expect2 <- f3(4)
    
    expect <- as.numeric(unlist(expect)[1])
    expect2 <- as.numeric(unlist(expect2)[1])
    
    n[i] <- (e[i]^2 - expect)/(expect2 - expect^2)^(1/2)
    
  }
  y <- x + n
  dataset <- as.data.frame(cbind(y, x, n))
  
  model <- lm(data = dataset, y ~ x + n)
  
  alphaestimate <- append(alphaestimate, as.numeric(unlist(model[1])[1]))
  betaestimate <- append(betaestimate, as.numeric(unlist(model[1])[2]))
}
##ttests <- append(ttests, t.test(betaestimate, mu=1))

summary(betaestimate)

## k = 4




alphaestimate <- vector()
betaestimate <- vector()
for (i in 1:1000) {
  
  x <- rnorm(n = 100, mean = 1)
  e <- rnorm(n = 100)
  
  f <- function(x) {
    1/sqrt(2*pi)*exp((-1/2)*x^2)
  }
  
  f2 <- function(x, k) {
    (x^k)*f(x)
  }
  
  f3 <- function(k) {
    integrate(f2, -Inf, Inf, k)
    
  }
  
  n <- vector()
  
  for (i in 1:100) {
    expect <- f3(4)
    expect2 <- f3(8)
    
    expect <- as.numeric(unlist(expect)[1])
    expect2 <- as.numeric(unlist(expect2)[1])
    
    n[i] <- (e[i]^4 - expect)/(expect2 - expect^2)^(1/2)
    
  }
  y <- x + n
  dataset <- as.data.frame(cbind(y, x, n))
  
  model <- lm(data = dataset, y ~ x + n)
  
  alphaestimate <- append(alphaestimate, as.numeric(unlist(model[1])[1]))
  betaestimate <- append(betaestimate, as.numeric(unlist(model[1])[2]))
}
##ttests <- append(ttests, t.test(betaestimate, mu=1))

summary(betaestimate)


## k = 8


alphaestimate <- vector()
betaestimate <- vector()
for (i in 1:1000) {
  
  x <- rnorm(n = 100, mean = 1)
  e <- rnorm(n = 100)
  
  f <- function(x) {
    1/sqrt(2*pi)*exp((-1/2)*x^2)
  }
  
  f2 <- function(x, k) {
    (x^k)*f(x)
  }
  
  f3 <- function(k) {
    integrate(f2, -Inf, Inf, k)
    
  }
  
  n <- vector()
  
  for (i in 1:100) {
    expect <- f3(8)
    expect2 <- f3(16)
    
    expect <- as.numeric(unlist(expect)[1])
    expect2 <- as.numeric(unlist(expect2)[1])
    
    n[i] <- (e[i]^8 - expect)/(expect2 - expect^2)^(1/2)
    
  }
  y <- x + n
  dataset <- as.data.frame(cbind(y, x, n))
  
  model <- lm(data = dataset, y ~ x + n)
  
  alphaestimate <- append(alphaestimate, as.numeric(unlist(model[1])[1]))
  betaestimate <- append(betaestimate, as.numeric(unlist(model[1])[2]))
}
##ttests <- append(ttests, t.test(betaestimate, mu=1))

summary(betaestimate)


library(readstata13)

caschool <- read.dta13("caschool.dta")

firstlm <- lm(data = caschool, testscr ~ str + el_pct + calw_pct + meal_pct)
summary(firstlm)

secondlm <- lm(data = caschool, testscr ~ str + el_pct + meal_pct)
summary(secondlm)

caschool$two_meal <- 2*caschool$meal_pct

thirdlm <- lm(data = caschool, testscr ~ str + el_pct + two_meal)
summary(thirdlm)

fourthlm <- lm(data = caschool, testscr ~ str + el_pct + two_meal + meal_pct)
summary(fourthlm)

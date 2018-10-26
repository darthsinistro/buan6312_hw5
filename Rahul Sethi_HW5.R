library(readstata13)

# Question 7.15

br2 <- read.dta13("br2.dta")
summary(br2)

hist(br2$price)

br2_orig <- br2

br2$price <- br2$price/1000
br2$sqft <- br2$sqft/100

summary(lm(log(price) ~ . , data=br2))

summary(lm(log(price) ~ . + waterfront:traditional , data=br2))

summary(lm(log(price) ~ .*waterfront , data=br2))

anova(lm(log(price) ~ .*waterfront , data=br2),lm(log(price) ~ ., data=br2))

x <- predict(lm(log(price) ~ . + waterfront:traditional , data=br2),data.frame(sqft=25, traditional=1, age=20, owner=1, fireplace=1, bedrooms=3, baths=2, pool=0, waterfront=0))

exp(x)*1000



# Question 7.16

stk <- read.dta13("stckton4.dta")

hist(stk$sprice)

hist(log(stk$sprice))

summary(lm(log(sprice/100)~., data=stk))

summary(lm(log(sprice/100)~. + lgelot:livarea, data=stk))

anova(lm(log(sprice/100)~., data=stk),lm(log(sprice/100)~.*lgelot, data=stk))
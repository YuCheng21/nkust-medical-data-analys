# create a data frame from scratch
# Number of deaths from AIDS in Australia per quarter in 1983 - 1986
週期<-c(1:14)
死亡人數<-c(0,1,2,3,1,4,9,18,23,31,20,25,37,45)
mydata<-data.frame(週期,死亡人數)
mydata
plot(mydata)
plot(log(mydata$週期),log(mydata$死亡人數),xlab="log(週期)",ylab="log(死亡人數)",las=1,main="log")
a1<-glm(死亡人數~週期,family=poisson(link=log),data=mydata)
a1
summary(a1)
coef(a1) # model coefficients
exp(coefficients(a1))

dlc<-read.csv("Danmark lung cancer.csv")
dlc
str(dlc)
View(dlc)
dlc$年齡f<-ordered(dlc$年齡層,levels=c("40-54", "55-59", "60-64", "65-69", "70-74", "≧75") )
dlc
str(dlc)
View(dlc)
m1<-glm(案例數~年齡f+offset(log(居民數)),family=poisson(link=log),data=dlc)
m1
# The first is linear (.L), the second is quadratic (.Q), the third is cubic (.C), and so on. 
summary(m1)
coef(m1)
exp(coefficients(m1))

library(GLMsData)
data(danishlc)
danishlc$Rate <- danishlc$Cases / danishlc$Pop * 1000 # Rate per 1000
# Preserve age-order 
danishlc$Age <- ordered(danishlc$Age, levels=c("40-54", "55-59", "60-64", "65-69", "70-74", ">74") )
danishlc$City <- abbreviate(danishlc$City, 1)
### Part 1
dlc.bin <- glm( cbind(Cases, Pop-Cases) ~ Age, family=binomial, data=danishlc)
dlc.psn <- glm( Cases ~ offset(log(Pop)) + Age, family=poisson, data=danishlc)
# The binomial and Poisson models give nearly identical results:
data.frame( coef(dlc.bin), coef(dlc.psn))
c( Df=df.residual(dlc.bin),Dev.Bin=deviance(dlc.bin),Dev.Poisson=deviance(dlc.psn) )
# The conditions are satisfied, so the binomial and Poisson models are equivalent:
max( fitted(dlc.bin) ) ### Small pi
min( danishlc$Pop ) ### Large m

library(ISwR)
data(eba1977)
names(eba1977)
attach(eba1977)
fit <- glm(cases~city+age+offset(log(pop)), family=poisson)
summary(fit)
# Alternatively, it could have been given as a separate argument as in
glm(cases~city+age, offset = log(pop), family=poisson)
# A goodness-of-fit statistic is provided by comparing the residual deviance to a χ2 distribution on the stated degrees of freedom.
min(fitted(fit))
pchisq(deviance(fit), df.residual(fit), lower=F)
# We could also just have read off the residual deviance and degrees of freedom from the summary output:
pchisq(23.45, 15, lower=F)
# From the coefficient table, it is obvious that there is an age effect, but it is less clear whether there is a city effect.
# We can perform χ2 tests for each term by using drop1 and looking at the changes in the deviance.
drop1(fit, test="Chisq")
# We see that the age term is significant, hardly surprisingly, but the city term apparently is not.
# However, if you can argue a priori that Fredericia could be expected to have a higher cancer rate than the three other cities,
# then it could be warranted to combine the three other cities into one and perform an analysis as below.
fit2 <- glm(cases~(city=="Fredericia")+age+offset(log(pop)), family=poisson)
anova(fit, fit2, test="Chisq")
drop1(fit2, test="Chisq")
summary(fit2)

cf <- coefficients(summary(fit2))
est <- cf[,1]
s.e. <- cf[,2]
rr <- exp(cbind(est, est - s.e.*qnorm(.975), est + s.e.*qnorm(.975) ))
colnames(rr) <- c("RateRatio", "CI.lo","CI.hi")
rr
exp(cbind(coef(fit2), confint(fit2)))
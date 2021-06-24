lung<-read.csv("P:/R/Rcode/lung_cancer_study.csv")
names(lung)
nrow(lung)
str(lung)
fix(lung)
class(lung)

mean(lung$AGE)
median(lung$AGE)
sd(lung$AGE)
min(lung$AGE)
max(lung$AGE)
sum(lung$AGE)

mean(lung$SURVIVAL_MONTHS)
mean(lung$SURVIVAL_MONTHS, na.rm=TRUE)
median(lung$SURVIVAL_MONTHS, na.rm=TRUE)
sd(lung$SURVIVAL_MONTHS, na.rm=TRUE)
min(lung$SURVIVAL_MONTHS, na.rm=TRUE)
max(lung$SURVIVAL_MONTHS, na.rm=TRUE)
sum(lung$SURVIVAL_MONTHS, na.rm=TRUE)

# mean,median,25th and 75th quartiles,min,max
summary(lung$AGE)
summary(lung$SURVIVAL_MONTHS)

# Tukey min,lower-hinge, median,upper-hinge,max
fivenum(lung$AGE)
fivenum(lung$SURVIVAL_MONTHS)

length(lung$AGE)
is.na(lung$AGE)
sum(is.na(lung$AGE))
sum(!is.na(lung$AGE))

length(lung$SURVIVAL_MONTHS)
is.na(lung$SURVIVAL_MONTHS)
sum(is.na(lung$SURVIVAL_MONTHS))
sum(!is.na(lung$SURVIVAL_MONTHS))

lung$AGE
AGE
attach(lung)
AGE
search()

detach(lung)
AGE
search()

# get means for variables in data frame mydata
# excluding missing values
class(list(lung$AGE,lung$SURVIVAL_MONTHS))
sapply(list(lung$AGE,lung$SURVIVAL_MONTHS), mean, na.rm=TRUE)

hist(lung$AGE)
hist(lung$SURVIVAL_MONTHS)
boxplot(lung$AGE)
boxplot(lung$SURVIVAL_MONTHS)
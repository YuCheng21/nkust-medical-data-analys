lc<-read.csv("./dataset/lung_cancer_study.csv")
names(lc)
str(lc)
View(lc)

table(lc$vital_status)
table(lc$Vital.Status)

library(survival)

# create a Surv object
survobj<-with(lc, Surv(SURVIVAL_MONTHS,vital_status))
survobj
summary(survobj)

# Plot survival distribution of the total sample
# Kaplan-Meier estimator
fit0<-survfit(survobj~1,data=lc)
fit0
summary(fit0)
summary(fit0,time=12)
summary(fit0,time=c(12,24,36,48,60))

plot(fit0)
plot(fit0,xlab="Survival Time(months)",ylab="Survival Probability",main="Overall Survival",las=1)
plot(fit0,conf.int=FALSE,mark.time=TRUE)
plot(fit0,xaxt="n")
axis(1,at=12*(1:18))

# Compare the survival distributions of men and women
fit1<-survfit(survobj~GENDER,data=lc)
fit1
table(lc$gender)
table(lc$GENDER)
summary(fit1)
summary(fit1,time=12)
summary(fit1,time=c(12,24,36,48,60))

# plot the survival distributions by sex
plot(fit1)
plot(fit1,lty=1:2)
plot(fit1,lty=2:3,mark.time=TRUE)

plot(fit1,xlab="Survival Time(months)",ylab="Survival Probability",col=c("red","blue"),main="Survival Distributions by Gender",las=1)
legend("topright",title="Gender",c("Female","Male"),fill=c("red","blue"))
# test for difference between male and female
# survival curves (logrank test)
survdiff(survobj~GENDER,data=lc)

lccox<-coxph(survobj~gender+AGE+CHEMO+RT+SMOKING,data=lc)
lccox
summary(lccox)

# predict male survival from age and medical scores
MaleMod<-coxph(survobj~gender+AGE+CHEMO+RT+SMOKING,data=lc,subset=gender==1)
FemaleMod<-coxph(survobj~gender+AGE+CHEMO+RT+SMOKING,data=lc,subset=gender==0)

# display results
MaleMod
FemaleMod

# evaluate the proportional hazards assumption
cox.zph(MaleMod)
cox.zph(FemaleMod)

cvd<-read.csv("./dataset/CVD_All.csv")
class(cvd)
str(cvd)
nrow(cvd)
View(cvd)
plot(cvd$年齡,cvd$收縮壓)
plot(cvd$年齡,cvd$收縮壓,xlab="年齡 (歲)",ylab="收縮壓(mmHg)",las=1)
complete.cases(cvd$年齡,cvd$收縮壓)
sum(is.na(cvd$年齡))
sum(is.na(cvd$收縮壓))
sum(complete.cases(cvd$年齡,cvd$收縮壓))
sum(!complete.cases(cvd$年齡,cvd$收縮壓))

cor(cvd,use="complete.obs")
cv<-cvd
str(cvd)
names(cv)<-c("ID","CVD","age","gender","FU","waist","sys","dia","sugar","HDL","TG","betal","alcohol","FH","smoking","smokeq")
str(cv)

head(cvd)
head(cv)
tail(cvd)
tail(cv)
head(cv["age"],n=10)
tail(cv$age)

myvars<-c("age","waist","sys","dia","sugar","HDL","TG")
cor(cv[myvars])
cor(cv[myvars],use="complete.obs")
cor(cv[myvars],use="pairwise.complete.obs")
Cl<-cor(cv[myvars],use="pairwise.complete.obs")
## Graphical Correlation Matrix:
symnum(Cl) # highly correlated

cor.test(cv$age,cv$waist, method='spearman', conf.level = 0.95)
cor.test(cv$age,cv$sys, method='pearson', conf.level = 0.95)
cor.test(cv$dia,cv$sys, method='pearson', conf.level = 0.95)

# Correlations with significance levels
library(Hmisc)
rcorr(cv[myvars], type="pearson") # type can be pearson or spearman
rcorr(as.matrix(cv[myvars]), type="pearson")
class(cv[myvars])
class(as.matrix(cv[myvars]))
# input must be a matrix, not a data frame 
rcorr(as.matrix(cv[myvars]))
rcorr(as.matrix(cv[myvars]), type="spearman")



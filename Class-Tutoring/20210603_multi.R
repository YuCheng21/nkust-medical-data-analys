cvd<-read.csv("CVD_All.csv")
str(cvd)

# 偵測資料中是有遺失值
na.fail(cvd)
# 若資料中有遺失值，要先去除遺失值
cvd_ok=na.omit(cvd)
str(cvd_ok)
str(cvd)

cvd$smokeg1<-cvd$抽菸量
cvd$ageg50<-cvd$年齡
str(cvd)
cvd$smokeg1[cvd$smokeg1<=1]<-0
cvd$smokeg1[cvd$smokeg1>1]<-1
cvd$ageg50[cvd$ageg50<=50]<-0
cvd$ageg50[cvd$ageg50>50]<-1
table(cvd$smokeg1)
table(cvd$ageg50)
str(cvd)

fit1<-glm(心血管疾病~smokeg1,data=cvd,family=binomial())
fit1
summary(fit1)
coefficients(fit1) # model coefficients
exp(coefficients(fit1))

fit2<-glm(心血管疾病~ageg50+smokeg1,data=cvd,family=binomial())
fit2
summary(fit2)
coefficients(fit2) # model coefficients
exp(coefficients(fit2))

fit3<-glm(心血管疾病~ageg50+smokeg1+ageg50*smokeg1,data=cvd,family=binomial())
fit3
summary(fit3)
coefficients(fit3) # model coefficients
exp(coefficients(fit3))

cvdf<-subset(cvd,性別==0)
cvdm<-subset(cvd,性別==1, select=c(心血管疾病,性別,ageg50,smokeg1))
str(cvdf)
str(cvdm)

fit4<-glm(心血管疾病~ageg50+smokeg1+ageg50*smokeg1,data=cvdf,family=binomial())
fit4
summary(fit4)
coefficients(fit4) # model coefficients
exp(coefficients(fit4))
table(cvdf$smokeg1)

fit5<-glm(心血管疾病~ageg50+smokeg1+ageg50*smokeg1,data=cvdm,family=binomial())
fit5
summary(fit5)
coefficients(fit5) # model coefficients
exp(coefficients(fit5))
table(cvdm$smokeg1)

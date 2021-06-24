cvd<-read.csv("CVD_All.csv")
cv<-cvd
str(cvd)
names(cv)<-c("ID","CVD","age","gender","FU","waist","sys","dia","sugar","HDL","TG","betal","alcohol","FH","smoking","smokeq")
str(cv)

is.na(cvd$年齡)
table(is.na(cvd$年齡))
table(is.na(cvd$收縮壓))

# lm(formula,data=資料名稱)，formula：依變數~自變數1＋自變數2＋…
fit<-lm(收縮壓~年齡,data=cvd)
fit
summary(fit)
names(fit)
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters

# 預測 25歲 的 收縮壓 
# 109.53=93.7881+0.6298*25
new.age<-data.frame(年齡=25)
new.age
predict(fit, new.age)

# 年齡 versus 收縮壓
plot(cvd$年齡,cvd$收縮壓,xlab="年齡 (歲)",ylab="收縮壓(mmHg)",las=1,main="收縮壓=93.79+0.63*年齡")
# Add fit lines
# regression line(y~x)
abline(fit,col="red") 

# 廣義線性迴歸模型generalized linear models (glm)
gfit<-glm(收縮壓~年齡,data=cvd)
gfit
summary(gfit)

# 舒張壓 versus 收縮壓
plot(cvd$舒張壓,cvd$收縮壓,xlab="舒張壓(mmHg)",ylab="收縮壓(mmHg)",las=1)
# Add fit lines
# regression line(y~x)
abline(fit,col="red") 

# 舒張壓 versus 收縮壓
fit<-lm(收縮壓~舒張壓,data=cvd)
fit
summary(fit)
confint(fit, level=0.95) # CIs for model parameters


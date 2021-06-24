cvd<-read.csv("CVD_All.csv")
str(cvd)

table(is.na(cvd$心血管疾病))
table(is.na(cvd$性別))

# 偵測資料中是有遺失值
na.fail(cvd)
# 若資料中有遺失值，要先去除遺失值
cvd_ok=na.omit(cvd)
str(cvd_ok)
str(cvd)

# Logistic Regression 羅吉斯迴歸常用在依變數為二元變數（非0即1）的場合，
# 如：生病/沒生病 - 錄取/不錄取 
# family="binomial" 邏輯迴歸模型
# Logistic Regression
# where F is a binary factor 依變數 and
# x1-x3 are continuous predictors 自變數
# fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())

# 以 性別 為 自變數 
fit<-glm(心血管疾病~性別,data=cvd,family=binomial())
fit
summary(fit)
confint(fit) # 95% CI for the coefficients
coefficients(fit) # model coefficients
# 男性得心血管疾病的勝算是女性的
# 勝算比(odds ratio, OR): 1.1296=exp(0.1219) 倍
exp(0.1219)

# 以 年齡 為 自變數 
fit<-glm(心血管疾病~年齡,data=cvd,family=binomial())
fit
summary(fit)
confint(fit) # 95% CI for the coefficients
coefficients(fit) # model coefficients
exp(0.0715)

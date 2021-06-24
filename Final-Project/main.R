# ==============================================================================
# 1 利用心血管疾病資料 CVD_ALL.csv 分析 "沒有心血管疾病" 成人之【腰圍】與【收縮壓】的關係。
cvd = read.csv("./dataset/CVD_All.csv")
cv = cvd
names(cv)<-c("ID","CVD","age","gender","FU",
             "waist","sys","dia","sugar","HDL",
             "TG","betal","alcohol","FH","smoking","smokeq")

# ==============================================================================
# 1.1 腰圍與收縮壓的 皮爾生相關係數 為何？兩者是否存在顯著的線性關係？
cor.test(cv$waist,cv$sys, method='pearson')
# p-value < 2.2e-16
# p-value < 0.05，達到顯著線性關係

# ==============================================================================
# 1.2 腰圍與收縮壓的 斯皮爾曼等級相關係數 為何？兩者是否存在顯著的等級相關？
cor.test(cv$waist,cv$sys, method='spearman')
# p-value < 2.2e-16
# p-value < 0.05，達到顯著的等級相關

# ==============================================================================
# 1.3 利用 簡單線性迴歸模型 建立 腰圍預測收縮壓 之模型:
fit<-lm(收縮壓~腰圍,data=cvd)
# ===================================================
# 1.3.1 請問此模型為何？
fit
# 簡單線性迴歸模型:
# 公式: 收縮壓 = 腰圍*0.8311 + 58.1406

# ===================================================
# 1.3.2 腰圍是否與收縮壓有顯著相關？
summary(fit)
# Pr(>|t|): <2e-16
# p-value < 0.05，腰圍與收縮壓顯著相關

# ===================================================
# 1.3.3 若有一人腰圍為 100 公分，預測此人平均收壓縮為何？
person = data.frame(腰圍 =100)
result = predict(fit, newdata = person)
print(result)
# 腰圍: 100 -> 收縮壓: 141.2519

# ==============================================================================
# 2 使用 lung_cancer_study.csv 資料檔
lc = read.csv("./dataset/lung_cancer_study.csv")

# ==============================================================================
# 2.1 計算肺癌病人中接受化療（CHEMO="Yes"）的存活曲線的 Kaplan-Meier 估計及圖。
survobj<-with(lc, Surv(SURVIVAL_MONTHS,vital_status))
fit0<-survfit(survobj~CHEMO,data=lc)
summary(fit0)
# 'survival' column 為存活曲線的估計

plot(fit0,xlab="Survival Time(months)",ylab="Survival Probability",col=c("red","blue"),main="Survival Distributions by Gender",las=1)
legend("topright",title="CHEMO",c("No","Yes"),fill=c("red","blue"))

# ==============================================================================
# 2.2 接第 (1) 題，計算 存活曲線的中位數。
fit0
# CHEMO=No 曲線中位數: 79.5
# CHEMO=Yes 曲線中位數: 51.0

# ==============================================================================
# 2.3 計算肺癌病人中是否接受放療（rt）的存活表現，檢定是否接受放療病人間存活的差異。
fit1<-survfit(survobj~rt,data=lc)
plot(fit1,xlab="Survival Time(months)",ylab="Survival Probability",col=c("red","blue"),main="Survival Distributions by Gender",las=1)
legend("topright",title="CHEMO",c("rt=0","rt=1"),fill=c("red","blue"))

survdiff(survobj~rt,data=lc)
# p=0.01，p<0.05 達到統計上顯著差異。

# ==============================================================================
# 2.4 使用 COX 模型:
# ===================================================
# 2.4.1 做單一變數的分析，探討 AGE、gender、SMOKING、CHEMO、T_STAGE 是否為風險因子？
lccox<-coxph(survobj~AGE,data=lc)
summary(lccox)
# p-value=2.79e-05<0.05 達到統計上顯著差異，屬於風險因子。

lccox<-coxph(survobj~gender,data=lc)
summary(lccox)
# p-value=0.01<0.05 達到統計上顯著差異，屬於風險因子。

lccox<-coxph(survobj~SMOKING,data=lc)
summary(lccox)
# SMOKINGNever smoked: p-value=0.144>0.05 未達到統計上顯著差異，不屬於風險因子。
# SMOKINGSmoked in the past: p-value=0.253>0.05 未達到統計上顯著差異，不屬於風險因子。
# => SMOKING 不屬於風險因子。

lccox<-coxph(survobj~CHEMO,data=lc)
summary(lccox)
# p-value=0.00061<0.05 達到統計上顯著差異，屬於風險因子。

lccox<-coxph(survobj~T_STAGE,data=lc)
summary(lccox)
# T_STAGET2 or T3: p-value=0.389>0.05 未達到統計上顯著差異，不屬於風險因子。
# T_STAGET2, T3 or T4: p-value=6.67e-07<0.05 達到統計上顯著差異，屬於風險因子。
# => T_STAGE 屬於風險因子。

# ===================================================
# 2.4.2 使用上述因子作多變量分析，並做出結論。
lccox<-coxph(survobj~AGE+gender+SMOKING+CHEMO+T_STAGE,data=lc)
summary(lccox)
# (1)
# 在控制其他變量的狀況下，沒有接受化療的肺癌患者相較於有接受化療的肺癌患者，
# 其死亡的風險為 '2.1862' 倍，p-value=7.08e-06<0.05 達統計上顯著差異，
# 且沒有接受化療的肺癌患者死亡的風險較高。
#
#                                 coef exp(coef) se(coef)      z Pr(>|z|)
# AGE                        0.03519   1.03582  0.00829  4.245 2.18e-05 ***
# gender                     0.08243   1.08593  0.16431  0.502   0.6159
# SMOKINGNever smoked       -1.07125   0.34258  0.36091 -2.968   0.0030 **
# SMOKINGSmoked in the past -0.60611   0.54547  0.26984 -2.246   0.0247 *
# CHEMOYes                   0.78215   2.18618  0.17415  4.491 7.08e-06 ***
# T_STAGET2 or T3            0.04186   1.04275  0.22320  0.188   0.8512
# T_STAGET2, T3 or T4        0.91142   2.48786  0.19512  4.671 2.99e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#                           exp(coef) exp(-coef) lower .95 upper .95
# AGE                          1.0358     0.9654    1.0191    1.0528
# gender                       1.0859     0.9209    0.7869    1.4985
# SMOKINGNever smoked          0.3426     2.9190    0.1689    0.6950
# SMOKINGSmoked in the past    0.5455     1.8333    0.3214    0.9257
# CHEMOYes                     2.1862     0.4574    1.5540    3.0755
# T_STAGET2 or T3              1.0427     0.9590    0.6733    1.6150
# T_STAGET2, T3 or T4          2.4879     0.4020    1.6972    3.6468

sur<-read.csv("survival.csv")
str(sur)
View(sur)
# 偵測資料中是有遺失值
na.fail(sur)
# 若資料中有遺失值，要先去除遺失值
sur_ok=na.omit(sur)

table(sur$Organ)
by(sur$Survival,sur$Organ,summary)
by(sur$Survival,sur$Organ,sd)
tapply(sur$Survival,sur$Organ,summary)
summary(sur$Survival)
sd(sur$Survival)

library(Hmisc)
describe(sur$Survival)

# One Way Anova
fit<-aov(Survival~Organ,data=sur)
fit
summary(fit)
names(fit)
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

boxplot(Survival~Organ,data=sur)
boxplot(Survival~Organ,data=sur,col=c(2:6))

# Multiple Comparisons
# No Adjustment
pairwise.t.test(sur$Survival,sur$Organ,p.adj="none")
# Bonferroni Adjustment
pairwise.t.test(sur$Survival,sur$Organ,p.adj="bonf")

# Kruskal Wallis Test One Way Anova by Ranks
# kruskal.test(y~A) # where y1 is numeric and A is a factor
kruskal.test(Survival~Organ,data=sur)

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

# method 指定檢定方法，有5種檢定方法可選擇"lsd"、"bonf"、 "tukey"、"scheffe"、"dunnett"，預設為"tukey"
library(asbio)
pairw.anova(sur$Survival,as.factor(sur$Organ),method="bonf")
pairw.anova(sur$Survival,as.factor(sur$Organ),method="lsd")
pairw.anova(sur$Survival,as.factor(sur$Organ),method="tukey")
pairw.anova(sur$Survival,as.factor(sur$Organ),method="scheffe")
pairw.anova(sur$Survival,as.factor(sur$Organ),method="dunnett",control="Bronchus")

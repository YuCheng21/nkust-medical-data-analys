lung<-read.csv("i:/R/Rcode/lung_cancer_study.csv")
names(lung)
nrow(lung)
str(lung)
class(lung)

plot(lung$AGE,lung$SURVIVAL_MONTHS,col=c("blue","red"))

hist(lung$AGE)
hist(lung$SURVIVAL_MONTHS)
boxplot(lung$AGE)
boxplot(lung$SURVIVAL_MONTHS)

# 設定亂數種子
set.seed(3)# 
# 常態分佈
x = rnorm(100, 8, 2)
# 不是常態分佈
y = rweibull(100, 2, 6)
hist(x)
hist(y)
shapiro.test(x)
shapiro.test(y)
shapiro.test(lung$AGE)
shapiro.test(lung$SURVIVAL_MONTHS)

# mean,median,25th and 75th quartiles,min,max
summary(lung$SURVIVAL_MONTHS)

# Finding Outliers
out <- boxplot.stats(lung$SURVIVAL_MONTHS)$out
length(out)
Q <- quantile(lung$SURVIVAL_MONTHS, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(lung$SURVIVAL_MONTHS, na.rm = TRUE)
up <-  Q[2]+1.5*iqr # Upper Range
low<- Q[1]-1.5*iqr # Lower Range

# Eliminating Outliers
elim<-subset(lung, lung$SURVIVAL_MONTHS>(Q[1]-1.5*iqr) & lung$SURVIVAL_MONTHS<(Q[2]+1.5*iqr))
nrow(elim)
nrow(lung)

nl <- lung[-which(lung$SURVIVAL_MONTHS %in% out),]
nrow(nl)
nrow(lung)
nnl <- lung[(!lung$SURVIVAL_MONTHS %in% out),]
nrow(nnl)
nrow(lung)

# remove outliers
lung$ns <- lung$SURVIVAL_MONTHS
lung$ns[lung$SURVIVAL_MONTHS > (Q[2]+1.5*iqr)] <- NA
lung$ns[lung$SURVIVAL_MONTHS < (Q[1]-1.5*iqr)] <- NA
sum(is.na(lung$SURVIVAL_MONTHS))
sum(!is.na(lung$SURVIVAL_MONTHS))
sum(is.na(lung$ns))
sum(!is.na(lung$ns))

boxplot(lung$SURVIVAL_MONTHS)
boxplot(lung$ns)
summary(lung$ns)

View(lung)
edit(lung)
fix(lung) # edit in place


pphn<-read.csv("I:/R/Rcode/PPHN.csv")
class(pphn)
names(pphn)
str(pphn)
nrow(pphn)
View(pphn)

summary(pphn$PH)

# one sample t-test
t.test(pphn$PH,mu=7) # Ho: mu=7
wilcox.test(pphn$PH,mu=7)

# paired t-test
#t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
t.test(pphn$X1分鐘apgar分數, pphn$X5分鐘apgar分數,paired=TRUE)
t.test(pphn$X5分鐘apgar分數, pphn$X1分鐘apgar分數,paired=TRUE)
Pair(pphn$X1分鐘apgar分數, pphn$X5分鐘apgar分數)
t.test(Pair(X1分鐘apgar分數, X5分鐘apgar分數)~1,data=pphn)

# independent 2-group t-test
#t.test(y~x) # where y is numeric and x is a binary factor
t.test(pphn$體重~pphn$死亡)

# independent 2-group t-test
#t.test(y1,y2) # where y1 and y2 are numeric
x1<-pphn[which(pphn$死亡==0),]
x2<-pphn[which(pphn$死亡==1),]
str(x1)
str(x2)
t.test(x1$體重,x2$體重)

y1<-subset(pphn, 死亡==0, select=c(體重,死亡))
y2<-subset(pphn, 死亡==1, select=c(體重,死亡))
str(y1)
str(y2)
t.test(y1$體重,y2$體重)

wilcox.test(pphn$體重~pphn$死亡)

#單一樣本比例檢定
table(pphn$死亡)
prop.table(table(pphn$死亡))
#prop.test(x,n,p=NULL)
prop.test(19,131,p=0.1,alternative="greater",correct=F)
binom.test(19,131,p=0.1,alternative="greater")

#雙樣本比例檢定
table(pphn$早期破水,pphn$死亡)
prop.table(table(pphn$早期破水,pphn$死亡),2)
100*prop.table(table(pphn$早期破水,pphn$死亡),2)
prop.test(table(pphn$早期破水,pphn$死亡))
prop.test(table(pphn$死亡,pphn$早期破水))
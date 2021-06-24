pphn<-read.csv("./dataset/PPHN.csv")
names(pphn)
str(pphn)
View(pphn)

pphn$wkg34<-pphn$¶g¼Æ
str(pphn)
pphn$wkg34
pphn$wkg34[pphn$wkg34 < 34]<-0
pphn$wkg34[pphn$wkg34 >= 34]<-1
pphn$wkg34
table(pphn$wkg34)
hist(pphn$¶g¼Æ)

# 2-Way Frequency Table
#mytable <- table(A,B) # A will be rows, B will be columns
mytable<-table(pphn$¦º¤`,pphn$wkg34)
mytable

margin.table(mytable,1) # A frequencies (summed over B)
margin.table(mytable,2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable,1) # row percentages
prop.table(mytable,2) # column percentages
100*prop.table(mytable,2) # column percentages

chisq.test(mytable)
fisher.test(mytable)



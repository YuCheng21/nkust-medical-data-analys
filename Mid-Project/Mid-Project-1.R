# Read data =====================================================
pphn = read.csv("./dataset/PPHN.csv")

# Template =====================================================
row = c("Mean","Median","Variance","IQR")
col = c("week", "weight")

template = function(param){
  value = c(round(mean(param,na.rm=T),4),
            median(param,na.rm=T),
            round(var(param,na.rm=T),4),
            IQR(param,na.rm=T))
  result = cbind(row, value)
  colnames(result) = c("label", "value")
  return (result)
}

# 1-1 =====================================================
week_1 = template(pphn$週數)
weight_1 = template(pphn$體重)
merge_1 = merge(week_1, weight_1, by="label")
sort_1 = merge_1[order(factor(merge_1$label, levels = row)),]
result_1 = data.frame(
  label = sort_1$label,
  week = sort_1$value.x,
  weight = sort_1$value.y
)
result_1

# 1-2 =====================================================
filter_gender_20 = pphn[which(pphn$性別==0),]
week_20 = template(filter_gender_20$週數)
weight_20 = template(filter_gender_20$體重)
gender_20 = rep(0, times=length(row))
merge_20 = merge(week_20, weight_20, by="label")
cbind_20 = cbind(gender_20,merge_20)
sort_20 = cbind_20[order(factor(cbind_20$label, levels = row)),]
colnames(sort_20) = c("gender", names(sort_20)[2:4])

filter_gender_21 = pphn[which(pphn$性別==1),]
week_21 = template(filter_gender_21$週數)
weight_21 = template(filter_gender_21$體重)
gender_21 = rep(1, times=length(row))
merge_21 = merge(week_21, weight_21, by="label")
cbind_21 = cbind(gender_21,merge_21)
sort_21 = cbind_21[order(factor(cbind_21$label, levels = row)),]
colnames(sort_21) = c("gender", names(sort_21)[2:4])

rbind_2 = rbind.data.frame(sort_20,sort_21)
sort_22 = rbind_2[order(factor(rbind_2$label, levels = row)),]

result_2 = data.frame(
  label = sort_22$label,
  gender = sort_22$gender,
  week = sort_22$value.x,
  weight = sort_22$value.y
)
result_2

# 1-3 =====================================================
less_3 = pphn[which(pphn$週數 < week_1[1,2]),]
weight_3 = template(less_3$體重)
result_3 = data.frame(
  label = weight_3[,1],
  weight = weight_3[,2]
)
result_3

# Result =====================================================
cat("請計算懷胎週期及體重的平均數、中位數、變異數及IQR")
result_1
cat("接續第(1)小題,請分別以男女嬰兒的結果呈現")
result_2
cat("接續第(1)小題,請針對懷胎週期小於平均數下的小孩,計算他們體重的平均數、中位數、變異數及IQR")
result_3
# =====================================================
# 1

x1<-pphn[which(pphn$死亡==0),]
x2<-pphn[which(pphn$死亡==1),]
t.test(x1$週數,x2$週數)
t.test(x1$體重,x2$體重)
t.test(x1$X1分鐘apgar分數,x2$X1分鐘apgar分數)
t.test(x1$X5分鐘apgar分數,x2$X5分鐘apgar分數)
t.test(x1$PH,x2$PH)
t.test(x1$AaDO2,x2$AaDO2)

# 平均數上具有顯著差異的變量 : 體重
# 使用的統計方法 : 獨立雙樣本 t 檢定

# 2

wilcox.test(x1$週數,x2$週數,alternative="greater")
wilcox.test(x1$體重,x2$體重,alternative="greater")
wilcox.test(x1$X1分鐘apgar分數,x2$X1分鐘apgar分數,alternative="greater")
wilcox.test(x1$X5分鐘apgar分數,x2$X5分鐘apgar分數,alternative="greater")
wilcox.test(x1$PH,x2$PH,alternative="greater")
wilcox.test(x1$AaDO2,x2$AaDO2,alternative="greater")

# 中位數具有顯著差異的變量 : 體重、PH
# 使用的統計方法 : 獨立雙樣本中位數差異檢定

# 3

prop.test(table(pphn$胎便吸入,pphn$死亡))
prop.test(table(pphn$早期破水,pphn$死亡))
prop.test(table(pphn$剖婦產,pphn$死亡))

# 比例分布具有顯著差異的變量 : 剖腹產
# 使用的統計方法 : 獨立雙樣本比例差異檢定

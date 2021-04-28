# Read data =====================================================
cvd <- read.csv("./dataset/CVD_All.csv")

# Template =====================================================
row = c("median","min","max","Q1","Q3")

template = function(param){
  value = c(median(param,na.rm=T),
            min(param,na.rm=T),
            max(param,na.rm=T),
            as.numeric(quantile(param, 1 / 4,na.rm=T)),
            as.numeric(quantile(param, 3 / 4,na.rm=T)))
  result = cbind(row, value)
  colnames(result) = c("label", "value")
  return (result)
}

# 2-1 =====================================================
temp_1 = template(cvd$收縮壓)
result_1 = data.frame(
  label = temp_1[,1],
  value = temp_1[,2]
)
result_1

# 2-2 =====================================================
temp_2 = template(cvd$空腹血糖)
result_2 = data.frame(
  label = temp_2[,1],
  value = temp_2[,2]
)
result_2

# 2-3 =====================================================
temp_3 = template(cvd$三酸甘油酯)
result_3 = data.frame(
  label = temp_3[,1],
  value = temp_3[,2]
)
result_3

# 2-4 =====================================================
filter_gender_40 = cvd[which(cvd$性別==0),]

temp_400 = template(filter_gender_40$收縮壓)
temp_401 = template(filter_gender_40$空腹血糖)
temp_402 = template(filter_gender_40$三酸甘油酯)
gender_40 = rep(0, times=length(row))
merge_401 = merge(temp_400,temp_401,by="label")
merge_402 = merge(merge_401,temp_402,by="label")
cbind_40 = cbind(gender_40,merge_402)
colnames(cbind_40) = c("gender", names(cbind_40)[2:5])

filter_gender_41 = cvd[which(cvd$性別==1),]

temp_410 = template(filter_gender_41$收縮壓)
temp_411 = template(filter_gender_41$空腹血糖)
temp_412 = template(filter_gender_41$三酸甘油酯)
gender_41 = rep(1, times=length(row))
merge_411 = merge(temp_410,temp_411,by="label")
merge_412 = merge(merge_411,temp_412,by="label")
cbind_41 = cbind(gender_41,merge_412)
colnames(cbind_41) = c("gender", names(cbind_41)[2:5])

rbind_4 = rbind.data.frame(cbind_40,cbind_41)
sort_4 = rbind_4[order(factor(rbind_4$label, levels = row)),]

result_4 = data.frame(
  label = sort_4$label,
  gender = sort_4$gender,
  value_1 = sort_4$value.x,
  value_2 = sort_4$value.y,
  value_3 = sort_4$value
)
result_4

# 3 =====================================================
boxplot(cvd$收縮壓 ~ cvd$性別, cvd)
boxplot(cvd$空腹血糖 ~ cvd$性別, cvd)
boxplot(cvd$三酸甘油酯 ~ cvd$性別, cvd)
# 探討 :
# 收縮壓以全距來看的話，女生的資料分散程度較男生分散。
# 而如果去除離散值，以四分位距來看，男女的資料分散程度差不多。
# 
# 空腹血糖以全距來看的話，男生的資料分散程度較女生分散。
# 而如果去除離散值，以四分位距來看，男女的資料分散程度差不多。
# 
# 三酸甘油酯以全距來看的話，男生的資料分散程度較女生分散。
# 而如果去除離散值，以四分位距來看，男生的資料分散程度較女生分散。

# 4-1 =====================================================
hist(cvd$收縮壓)

mean(cvd$收縮壓, na.rm=T)
median(cvd$收縮壓, na.rm=T)
as.numeric(names(table(cvd$收縮壓)))[which.max(table(cvd$收縮壓))]
# 4-2 =====================================================
hist(cvd$舒張壓)

mean(cvd$舒張壓, na.rm=T)
median(cvd$舒張壓, na.rm=T)
as.numeric(names(table(cvd$舒張壓)))[which.max(table(cvd$舒張壓))]
# 4-3 =====================================================
# 結論 :
# 發現收縮壓與舒張壓的資料呈現正偏態分布，絕大多數的值位於平均值的左側，
# 大部份人的收縮壓壓與舒張壓都低於平均值，可能是受到太多離群值的影響，
# 若去除過大的離群值，資料回歸常態分佈後，收縮壓大約 120 左右，舒張壓大約 70 左右。


# 5 =====================================================
plot(cvd$收縮壓,cvd$舒張壓,pch=20)
# 探討 :
# 由結果判斷，收縮壓與舒張壓呈線性比例，收縮壓越高，舒張壓也越高，也就是正比關係。

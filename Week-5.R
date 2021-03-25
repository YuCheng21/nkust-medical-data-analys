?read.csv
help("read.csv")

# -------------- 
# read csv
lung = read.csv("dataset/lung_cancer_study.csv")
# get stored variable
ls()
# get all col
names(lung)
# get one row
lung$GENDER
# histogram
hist(lung$AGE)
# structure
str(lung)
# count all row number
nrow(lung)
# view the dataframe
View(lung)
# edit the dataframe
fix(lung)

# -------------- 

# 기출2회 

setwd('C:/Users/Administrator/Downloads/PART 06. 기출복원문제')

# 작업형2
# Data input
X_train=read.csv("stroke_X_train.csv")
X_test=read.csv("stroke_X_test.csv")
y_train=read.csv("stroke_y_train.csv")

# 결측값 확인하기
apply(is.na(X_train), 2, sum)
apply(X_train == '', 2, sum)

apply(is.na(X_test), 2, sum)
apply(X_test == '', 2, sum)

# bmi결측치 채우기기
X_train$bmi[is.na(X_train$bmi)] = mean(X_train$bmi, na.rm = TRUE)
X_test$bmi[is.na(X_test$bmi)] = mean(X_test$bmi, na.rm = TRUE)

# smoking _status 열 제거
X_train = subset(X_train, select = -c(smoking_status))
X_test = subset(X_test, select = -c(smoking_status))


# data style
str(X_train)

# 열 범주화
X_train$gender = factor(X_train$gender)
X_train$hypertension = factor(X_train$hypertension)
X_train$heart_disease = factor(X_train$heart_disease)
X_train$ever_married = factor(X_train$ever_married)
X_train$work_type = factor(X_train$work_type)
X_train$Residence_type = factor(X_train$Residence_type)

X_test$gender = factor(X_test$gender)
X_test$hypertension = factor(X_test$hypertension)
X_test$heart_disease = factor(X_test$heart_disease)
X_test$ever_married = factor(X_test$ever_married)
X_test$work_type = factor(X_test$work_type)
X_test$Residence_type = factor(X_test$Residence_type)

# scale
X_train$age = scale(X_train$age)
X_train$avg_glucose_level = scale(X_train$avg_glucose_level)

X_test$age = scale(X_test$age)
X_test$avg_glucose_level = scale(X_test$avg_glucose_level)


# train data 가꾸기
X_train = subset(X_train, select = -c(id))
X_train$stroke = factor(y_train$stroke)

# modeling
library(randomForest)
model=randomForest(stroke ~ ., 
             data = X_train, 
             ntree = 500,
             mtry = 3
             )

y_pred=predict(model, X_test[,-1], type = 'class')

df=data.frame(X_test$id, y_pred)
colnames(df) = c('id', 'stroke')

head(df)

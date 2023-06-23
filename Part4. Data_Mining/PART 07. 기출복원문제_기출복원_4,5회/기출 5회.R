# 기출 5회
#setwd('C:/Users/Administrator/Downloads/PART 07. 기출복원문제_기출복원_4,5회/data')

train=read.csv('carprice_train.csv')
test=read.csv('carprice_test.csv')

# 결측값 확인하기
apply(is.na(train),2, sum)
apply(train == '',2, sum)

apply(is.na(test),2, sum)
apply(test == '',2, sum)

# 열 범주화
train$model = factor(train$model)
train$transmission = factor(train$transmission)
train$fuelType = factor(train$fuelType)

test$model = factor(test$model)
test$transmission = factor(test$transmission)
test$fuelType = factor(test$fuelType)

# scale
train[,c('year', 'mileage','tax','mpg','engineSize')] = scale(train[,c('year', 'mileage','tax','mpg','engineSize')])
test[,c('year', 'mileage','tax','mpg','engineSize')] = scale(test[,c('year', 'mileage','tax','mpg','engineSize')])

# modeling
library(randomForest)

model = randomForest(price~., 
             data = train, 
             ntree = 300, 
             mtry = (9/3),
             importance = TRUE)
pred=predict(model, new_data = test)

df=data.frame(1:nrow(test), pred)
head(df)
colnames(df) = c('number', 'prediction')
head(df)

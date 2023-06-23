# 기출 4회
# setwd('C:/Users/Administrator/Downloads/PART 07. 기출복원문제_기출복원_4,5회/data')

train=read.csv('bodyPerfor_train.csv')
test=read.csv('bodyPerfor_test.csv')


# 결측값 없는 걸로 확인
str(train)

# train id열 제거
train = subset(train, select = -c(id))

# 범주화
train$gender = factor(train$gender)
test$gender = factor(test$gender)

train$class = factor(train$class)

# scale
train[,c(1,3,4,5,6,7,8,9,10,11)] = scale(train[,c(1,3,4,5,6,7,8,9,10,11)])
test[,c(2,4,5,6,7,8,9,10,11,12)] = scale(test[,c(2,4,5,6,7,8,9,10,11,12)])

# modeling
library(randomForest)
model = randomForest(class ~.,
             data = train, 
             ntree = 300, 
             mtry = sqrt(11))
pred = predict(model, test[,-1])

df = data.frame(test$id, pred)
colnames(df) = c('id','class')

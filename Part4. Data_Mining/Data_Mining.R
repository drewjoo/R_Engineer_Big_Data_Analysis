#################################################################
#                    PART 04) 데이터 마이닝
#################################################################

##===============================================================
##### 1장. 데이터 분할과 성과분석

##### 1절 | 데이터 분할
rm(list=ls())

# ===================================================================== #
# sample
# ===================================================================== #

# Q) credit 데이터를 train, validation, test로 분할해보자.
# 데이터 분할 전, 데이터 불러오기
setwd("C:/Users/Administrator/Desktop/R_Engineer_Big_Data_Analysis/Part4. Data_Mining/")   # 개인적으로 working directory를 반드시 지정하시기 바랍니다.
credit.df <- read.csv("credit_final.csv")

nrow(credit.df)  # 분석 데이터의 행 개수를 파악

set.seed(1111) # 난수를 동일하게 추출되도록 고정시키는 함수

idx <- sample(3, nrow(credit.df), # idx에 1,2,3을 credit 데이터 행 개수와 동일하게 추출
              replace = T, # 랜덤 복원 추출
              prob = c(0.5,0.3,0.2)) # 1은 train으로 전체의 50%,
# 2는 validation으로 전체의 30%,
# 3은 test로 전체의 20%

train <- credit.df[idx==1,]
valid <- credit.df[idx==2,]
test <- credit.df[idx==3,]

nrow(train) # train 데이터의 행 개수 파악
nrow(valid) # validation 데이터의 행 개수 파악
nrow(test) # test 데이터의 행 개수 파악

# ===================================================================== #
# createDataPartition
# ===================================================================== #
# Q) credit 데이터를 train, test로 분할해보자.

# install.packages("caret")
library(caret)
setwd("C:/Users/Administrator/Desktop/")
credit.df <- read.csv("credit_final.csv")

part<-createDataPartition(credit.df$credit.rating, # 목적변수인 credit.rating를 지정
                          times = 1, # 생성할 데이터 분할은 1개로 지정
                          p = 0.7) # 훈련데이터를 70%로 설정

parts <- as.vector(part$Resample1) # 행번호를 벡터형으로 변환
train <- credit.df[parts,]
test <- credit.df[-parts,]

nrow(train)
nrow(test)

##### 2절 | 성과 분석

# ===================================================================== #
# 오분류표(Confusion Matrix) (분석지표 포함)
# ===================================================================== #

# install.packages("caret")
library(caret)

predicted <- factor(c(1,0,0,1,1,1,0,0,0,1,1,1))
actual <- factor(c(1,0,0,1,1,0,1,1,0,1,1,1))

cm <- caret::confusionMatrix(predicted,actual)    # predicted 값과 actual 값 모두 factor형 변수로 되어 있어야 됨

# 저장된 confusionMarix는 list형으로 list명을 출력
# 각 리스트에 원하는 지표들을 개별적으로 출력이 가능함
names(cm)
# [1] "positive" "table"    "overall"  "byClass"  "mode"     "dots"   
# 지표들은 "overall"과 "byClass"에 list형으로 저장되어있음
# overall에 저장되어 있는 지표 확인
names(cm$overall)
# byClass에 저장되어 있는 지표 확인
names(cm$byClass)

# confusion matrix 출력
cm$table

# 정분류율 값 출력
cm$overall[["Accuracy"]]
cm$overall[[1]]

# 오분류율 값 출력
1 - cm$overall[["Accuracy"]]
1 - cm$overall[[1]]

# 민감도 값 출력
cm$byClass[["Sensitivity"]]
cm$byClass[[1]]

# 특이도 값 출력
cm$byClass[["Specificity"]]
cm$byClass[[2]]

# 정밀도 값 출력
cm$byClass[["Precision"]]
cm$byClass[[5]]

# 재현율 값 출력
cm$byClass[["Recall"]]
cm$byClass[[6]]

# F1-score 값 출력
cm$byClass[["F1"]]
cm$byClass[[7]]

# ===================================================================== #
# AUC
# ===================================================================== #
# install.packages("ModelMetrics ")
library(ModelMetrics)

predicted <- factor(c(1,0,0,1,1,1,0,0,0,1,1,1))
actual <- factor(c(1,0,0,1,1,0,1,1,0,1,1,1))
auc(actual,predicted) # AUC 값 확인


# ===================================================================== #
# 예측 지표
# ===================================================================== #
# Q) 임의의 값을 활용하여 MSE, RMSE, MAE를 구해보자

# install.packages("ModelMetrics ")
library(ModelMetrics)

set.seed(1234)
predicted <- runif(100,0,1)
actual <- runif(100,0,1)

# MSE(Mean of Squared Errors)
mse(actual,predicted)
# mean((predicted-actual)^2)

# RMSE(Root MSE)
rmse(actual,predicted)
# sqrt(mean((predicted-actual)^2))

# MAE(Mean of Absolute Errors)
mae(actual,predicted)
# mean(abs(predicted-actual))

##===============================================================
##### 2장. 분류 분석

##### 1절. 로지스틱 회귀분석

# Q. credit 데이터를 분할하고, train 데이터로 로지스틱 회귀모델을 만들어 보자.
# credit 데이터 불러오기
setwd("C:/Users/Administrator/Desktop/")     # 개인적으로 working directory를 반드시 지정하시기 바랍니다.
credit <- read.csv("credit_final.csv")
class(credit$credit.rating)  # 종속변수 형식 파악 결과, integer형인 것 확인
credit$credit.rating <- factor(credit$credit.rating)  # 종속변수 factor 변환

str(credit) # 데이터 구조 파악

# 데이터 분할 : train 70%, test 30%
set.seed(123)
idx <- sample(1:nrow(credit), nrow(credit)*0.7, replace=FALSE)
train <- credit[idx,]
test <- credit[-idx,]

# 로지스틱 회귀분석 실시
logit.model <- glm(credit.rating ~ .,
                   data = train,
                   family = "binomial")
summary(logit.model) # 회귀계수의 p-value가 유의수준 0.05보다 높게 나타나는 변수가 많음

# step 함수를 활용하여 다시 분석 실시

# step 함수의 scope 인자 내 upper에 모든 변수를 넣기 위함  
full <- ~ account.balance + credit.duration.months + previous.credit.payment.status +
  credit.purpose + credit.amount + savings + employment.duration + installment.rate +
  marital.status + guarantor + residence.duration + current.assets + age + other.credits +
  apartment.type + bank.credits + occupation + dependents + telephone + foreign.worker

step.logit.model <- step(glm(credit.rating ~ 1, data = train, family = "binomial"),
                         scope = list(lower = ~ 1, upper = full), # 상수항(lower=~1)부터 모든 변수(upper=~변수 모두 기입)
                         direction = "both") # 단계별 선택(“both”) 진행
summary(step.logit.model)

# 예측을 통한 분석 지표 출력
library(caret)
pred <- predict(step.logit.model, test[,-1], type = "response") # 예측값을 “response”로 지정하여 확률값을 출력
pred1 <- as.data.frame(pred)    # 결과를 data.frame으로 변형
pred.logit <- ifelse(pred1$pred < 0.5, 0, 1) # 확률값만 나오므로, 0.5를 기준으로 0, 1 범주 생성

cm.logit <- caret::confusionMatrix(data = as.factor(pred.logit), reference = test[,1], positive = '1')

cm.logit$table # confusion matrix 출력

cm.logit$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.logit$overall[["Accuracy"]] # 오분류율 값 출력

cm.logit$byClass[["Sensitivity"]] # 민감도 값 출력
cm.logit$byClass[["Specificity"]] # 특이도 값 출력
cm.logit$byClass[["Precision"]] # 정밀도 값 출력
cm.logit$byClass[["Recall"]] # 재현율 값 출력
cm.logit$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
# AUC 값 산출
library(ModelMetrics)
pred <- predict(step.logit.model, test[,-1], type = "response") # 예측값을 “response”로 지정하여 확률값을 출력
auc(actual = test[,1], predicted = pred.logit) # AUC값 0.6702182


# Q) iris 데이터의 Species를 분류하는 다항 로지스틱 회귀분석을 실시하고 오분류표를 만들어보자.
data(iris)
set.seed(123)
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)
train.iris <- iris[idx,]
test.iris <- iris[-idx,]

# train 데이터로 다항 로지스틱 회귀분석 실시
library(nnet)
mul.logit.model <- multinom(Species ~. , data = train.iris)

# 예측을 통한 분석 지표 출력
library(caret)
pred.mul.logit <- predict(mul.logit.model, test.iris[,-5])

cm.mul.logit <- caret::confusionMatrix(data = pred.mul.logit, reference =  test.iris[,5], positive = '1')
print(cm.mul.logit)


##### 2절. 의사결정나무
# 앞서 분할한 credit 데이터의 train 데이터로 의사결정나무 모델을 만들어 보자.

library(rpart)
library(rpart.plot) # 연습에서만 사용가능

# 의사결정나무 실시
dt.model <- rpart(credit.rating ~ .,          # 종속변수(credit.rating)와 모든 변수를 독립변수로 사용
                  method = "class",           # method는 분류인 “class” 선택
                  data = train,
                  control = rpart.control(maxdepth = 5,    # 의사결정나모의 최대 깊이는 5개까지
                                          minsplit = 15))  # 노드에서 최소 관측치는 15개 이상
# 
# dt.model2 <- train(credit.rating ~ .,
#                    data = train,
#                    method = "rpart",
#                    trControl=trainControl())

prp(dt.model, type = 4, extra = 2) # 시각화로 확인(시험에서는 불가,연습에서만 사용가능)

print(dt.model)

# # rpart 함수를 활용하여 의사결정나무분석 실시(최적 나무 선정)
# dt.model$cptable  
# opt<-which.min(dt.model$cptable[,"xerror"]) # x-error이 가장 낮은 split 개수를 선택
# cp<-dt.model$cptable[opt,"CP"] 
# prune.c <- prune(dt.model,cp=cp) 

# plotcp(dt.model)

# 예측을 통한 분석 지표 출력
library(caret)
pred.dt <- predict(dt.model, test[,-1], type = "class")     # 예측값을 “class”로 지정하여 분류 그룹을 출력
cm.dt <- caret::confusionMatrix(data = pred.dt, reference = test[,1], positive='1')

cm.dt$table # confusion matrix 출력

cm.dt$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.dt$overall[["Accuracy"]] # 오분류율 값 출력
cm.dt$byClass[["Sensitivity"]] # 민감도 값 출력
cm.dt$byClass[["Specificity"]] # 특이도 값 출력
cm.dt$byClass[["Precision"]] # 정밀도 값 출력
cm.dt$byClass[["Recall"]] # 재현율 값 출력
cm.dt$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.dt <- predict(dt.model, test[,-1], type = "prob")[,2]     # 예측값을 “prob”로 지정하여 확률 값을 출력
auc(actual = test[,1], predicted = pred.dt) # AUC값 0.7036714

# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 의사결정나무분석을 실시하고 오분류표를 만들어보자.
# train 데이터로 다항 분류 나무 실시
library(rpart)
library(rpart.plot) # 연습에서만 사용가능
dt.model2 <- rpart(Species ~. , data = train.iris)
prp(dt.model2,type = 4,extra = 2) # 시각화로 확인(시험에서는 불가,연습에서만 사용가능)

# 예측을 통한 분석 지표 출력
library(caret)
pred.dt2 <- predict(dt.model2, test.iris[,-5], type="class")

cm.dt2 <- caret::confusionMatrix(data = pred.dt2, reference =  test.iris[,5], positive = '1')
print(cm.dt2)

##### 3절. 앙상블(ensemble) 기법 - 배깅(Bagging)

# install.packages("ipred")
library(ipred)

# bagging 함수를 활용하여 bagging분석 실시
bag.model <- bagging(credit.rating ~ .,
                     data = train,
                     nbagg = 15) #반복 또는 트리의 수는 15

# 예측을 통한 분석 지표 출력
library(caret)
pred.bg <- predict(bag.model, test[,-1], type = "class")
cm.bg <- caret::confusionMatrix(data = pred.bg, reference = test[,1], positive = '1')

cm.bg$table # confusion matrix 출력

cm.bg$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.bg$overall[["Accuracy"]] # 오분류율 값 출력
cm.bg$byClass[["Sensitivity"]] # 민감도 값 출력
cm.bg$byClass[["Specificity"]] # 특이도 값 출력
cm.bg$byClass[["Precision"]] # 정밀도 값 출력
cm.bg$byClass[["Recall"]] # 재현율 값 출력
cm.bg$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.bg <- predict(bag.model, test[,-1], type = "prob")[,2]      # 예측값을 “prob”로 지정하여 확률 값을 출력
auc(actual = test[,1], predicted = pred.bg) # AUC값 0.7803338

# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 배깅을 실시하고 오분류표를 만들어보자.
# train 데이터로 다항  bagging분석 실시
bag.model2 <- bagging(Species ~.,
                      data = train.iris,
                      nbagg = 100) # 반복 또는 트리의 수는 15

# 예측을 통한 분석 지표 출력
library(caret)
pred.bg2 <- predict(bag.model2, test.iris[,-5], type="class")

cm.bag2 <- caret::confusionMatrix(data = pred.bg2, reference =  test.iris[,5], positive = '1')
print(cm.bag2)

##### 3절. 앙상블(ensemble) 기법 - 부스팅(boosting)

# xgboost 함수를 활용하여 boosting 분석 실시
# install.packages("xgboost")
library(xgboost)

# 1) xgboost의 모든 변수는 num 형태
# 2) 매트릭스 기반
# 3) 인덱스는 0부터 시작

train_x <- data.matrix(train[,-1])
train_y <- as.integer(train[,1]) - 1 # 인덱스 0부터 시작해, 수치형 클래스(1,0)로 변환 

test_x <- data.matrix(test[,-1])

xgb.train <- xgb.DMatrix(data = train_x, label = train_y) # xgboost용 DMatrix 구조로 변환

xgb.model <- xgboost(data = xgb.train,
                     max.depth = 2,
                     eta = 1,
                     # nthread = 2,
                     nrounds = 2,
                     objective = "binary:logistic") #logloss

# 예측을 통한 분석 지표 출력
library(caret)

pred <- predict(xgb.model,test_x)
pred.xgb <- ifelse(pred < 0.5, 0, 1) # 확률값만 나오므로, 0.5를 기준으로 0, 1 범주 생성

cm.xgb <- caret::confusionMatrix(data = as.factor(pred.xgb), reference = test[,1], positive = '1')

cm.xgb$table # confusion matrix 출력

cm.xgb$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.xgb$overall[["Accuracy"]] # 오분류율 값 출력
cm.xgb$byClass[["Sensitivity"]] # 민감도 값 출력
cm.xgb$byClass[["Specificity"]] # 특이도 값 출력
cm.xgb$byClass[["Precision"]] # 정밀도 값 출력
cm.xgb$byClass[["Recall"]] # 재현율 값 출력
cm.xgb$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred <- predict(xgb.model,test_x)
auc(actual = test[,1], predicted = pred.xgb) # AUC값 0.6437741


# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 부스팅을 실시하고 오분류표를 만들어보자.
# train 데이터로 다항  boosting분석 실시

train_x <- data.matrix(train.iris[,-5])
train_y <- as.integer(train.iris[,5]) - 1 # 인덱스 0부터 시작해, 수치형 클래스(2,1,0)로 변환 
test_x <- data.matrix(test.iris[,-5])

xgb.train <- xgb.DMatrix(data = train_x, label = train_y) # xgboost용 DMatrix 구조로 변환

xgb.model2 <- xgboost(data = xgb.train,
                      max.depth = 2,
                      eta = 1,
                      nrounds = 2,
                      objective = "multi:softprob",
                      num_class = 3,
)

# 예측을 통한 분석 지표 출력
library(caret)
pred <- as.data.frame(predict(xgb.model2, test_x, reshape = T)) # reshape = T는 print할 예측값이 case 당 여러 개 일 경우,
# 예측 값(벡터형)를 행렬 형태로 바꾸는 것

# 확률 값만 나오므로 행별로 가장 큰 확률을 가지는 변수명을 선택하는 방법으로 예측 라벨값 추출함
colnames(pred) = levels(iris$Species) # 행렬 형태의 예측 값의 변수명을 라벨값(Species)으로 지정함
pred.xgb2 = colnames(pred)[apply(pred,1,which.max)] # 각 행마다 가장 큰 확률을 가지는 열의 인덱스를 골라 라벨값 추출
cm.xgb2 <- caret::confusionMatrix(data = as.factor(pred.xgb2), reference =  test.iris[,5], positive = '1')
print(cm.xgb2)

##### 3절. 앙상블(ensemble) 기법 - 랜덤포레스트

# install.packages("randomForest")
library(randomForest)
# randomForest 함수를 활용하여 RandomForest분석 실시
rf.model <- randomForest(credit.rating ~ .,
                         data = train,
                         ntree = 50, # 나무 50개 사용
                         mtry = sqrt(20), # 사용할 변수의 개수(classification이므로 sqrt(20)개)
                         importance = T) #변수중요도 결과를 확인

rf.model  

rf.model$importance #변수중요도 결과 확인
#varImpPlot(rf.model) #변수중요도 시각화(시험에서는 불가, 연습에서만 사용 가능)

# 예측을 통한 분석 지표 출력
library(caret)
pred.rf <- predict(rf.model, test[,-1], type = "class")
cm.rf <- caret::confusionMatrix(data = pred.rf, reference = test[,1], positive = '1')
cm.rf$table # confusion matrix 출력

cm.rf$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.rf$overall[["Accuracy"]] # 오분류율 값 출력
cm.rf$byClass[["Sensitivity"]] # 민감도 값 출력
cm.rf$byClass[["Specificity"]] # 특이도 값 출력
cm.rf$byClass[["Precision"]] # 정밀도 값 출력
cm.rf$byClass[["Recall"]] # 재현율 값 출력
cm.rf$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.rf <- predict(rf.model, test[,-1], type = "prob")[,2]      # 예측값을 “prob”로 지정하여 확률 값을 출력
auc(actual = test[,1], predicted = pred.rf) # AUC값 0.7724005

# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 랜덤포레스트를 실시하고 오분류표를 만들어보자.
# train 데이터로 다항 랜덤포레스트 실시
library(randomForest)

rf.model2 <- randomForest(Species ~. ,
                          data = train.iris,
                          ntree = 500, # 나무 500개 사용
                          mtry = sqrt(4), # 사용할 변수의 개수(classification이므로 sqrt(4)개)
                          importance = T) #변수중요도 결과를 확인

# 예측을 통한 분석 지표 출력
library(caret)
pred.rf2 <- predict(rf.model2, test.iris[,-5], type="class")

cm.rf2 <- caret::confusionMatrix(data = pred.rf2, reference =  test.iris[,5], positive = '1')
print(cm.rf2)

##### 4절. 서포트벡터머신(SVM)

# install.packages("e1071")
library(e1071)

# tune.svm 함수를 활용하여 최적의 파라미터값 찾기
tune.svm(credit.rating ~ .,
         data = credit,
         gamma = 10^(-6:-1), #여기서는 6*2=12개의 조합에서 모수조율이 이루어짐
         cost = 10^(1:2)) 

# svm 함수를 활용하여 SVM분석 실시
svm.model <- svm(credit.rating ~ .,
                 data = train,
                 kernel = "radial",
                 gamma = 0.01,
                 cost = 10,
                 probability = TRUE) # 확률값 출력시 필요
summary(svm.model) 

# 예측을 통한 분석 지표 출력
library(caret)
pred.svm <- predict(svm.model, test, type="class")   
cm.svm <- caret::confusionMatrix(data = pred.svm, reference = test[,1], positive='1')
cm.svm$table # confusion matrix 출력

cm.svm$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.svm$overall[["Accuracy"]] # 오분류율 값 출력
cm.svm$byClass[["Sensitivity"]] # 민감도 값 출력
cm.svm$byClass[["Specificity"]] # 특이도 값 출력
cm.svm$byClass[["Precision"]] # 정밀도 값 출력
cm.svm$byClass[["Recall"]] # 재현율 값 출력
cm.svm$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.svm <- predict(svm.model, test, probability = TRUE) # 확률값 출력시 probability = TRUE이 필요
pred.svm <-  attr(pred.svm, 'probabilities')[,1] # 확률값 출력을 위해서는 해당 코드를 사용해야함
auc(actual = test[,1], predicted = pred.svm) # AUC값 0.7575353


# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 SVM분석을 실시하고 오분류표를 만들어보자.
library(e1071)

tune.svm(Species ~., 
         data = iris, 
         gamma = 2^(-1:1),        
         cost = 2^(2:4))      

svm.model2 <- svm(Species ~ .,
                  data = train.iris,
                  kernel = "radial",
                  gamma = 0.5,
                  cost = 16)

# 예측을 통한 분석 지표 출력
library(caret)
pred.svm2 <- predict(svm.model2, test.iris[,-5], type="class")

cm.svm2 <- caret::confusionMatrix(data = pred.svm2, reference =  test.iris[,5], positive = '1')
print(cm.svm2)

##### 5절. 나이브 베이즈 분류(Naive Bayes Classification)

# install.packages("e1071")
library(e1071)

# naiveBayes 함수를 활용하여 나이브 베이즈 분류분석 실시
nb.model <- naiveBayes(credit.rating ~ .,
                       data = train,
                       laplace = 0)
nb.model

# 예측을 통한 분석 지표 출력
library(caret)
pred.nb <- predict(nb.model, test, type="class")   
cm.nb <- caret::confusionMatrix(data = pred.nb, reference = test[,1], positive='1')
cm.nb$table # confusion matrix 출력

cm.nb$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.nb$overall[["Accuracy"]] # 오분류율 값 출력
cm.nb$byClass[["Sensitivity"]] # 민감도 값 출력
cm.nb$byClass[["Specificity"]] # 특이도 값 출력
cm.nb$byClass[["Precision"]] # 정밀도 값 출력
cm.nb$byClass[["Recall"]] # 재현율 값 출력
cm.nb$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.nb <- predict(nb.model, test, type = "raw")[,2]   # 확률값 출력시 type을 raw로 지정 필요
auc(actual = test[,1], predicted = pred.nb) # AUC값 0.7708858

# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 나이브 베이즈 분류분석을 실시하고 오분류표를 만들어보자.
library(e1071)

nb.model2 <- naiveBayes(Species ~ .,
                        data = train.iris,
                        laplace = 0)

# 예측을 통한 분석 지표 출력
library(caret)
pred.nb2 <- predict(nb.model2 , test.iris[,-5], type="class")

cm.nb2 <- caret::confusionMatrix(data = pred.nb2, reference =  test.iris[,5], positive = '1')
print(cm.nb2)

##### 6절. K-NN(K-Nearest Neighbor)

# install.packages("class")
library(class)

# knn 함수를 활용하여 K-NN 분석 실시
train_x <- train[,-1]
test_x <- test[,-1]
train_y <- train[,1]

knn.3 <- knn(train = train_x, test = test_x, cl = train_y, k = 3)
knn.7 <- knn(train = train_x, test = test_x, cl = train_y, k = 7)
knn.10 <- knn(train = train_x, test = test_x, cl = train_y, k = 10)

# 각각의 k에 대해 분류 정확도 확인
cm.knn.3 <- caret::confusionMatrix(data = knn.3, reference = test[,1], positive = '1') # k = 3일 때, knn 분류
cm.knn.3$overall[["Accuracy"]]

cm.knn.7 <- caret::confusionMatrix(data = knn.7, reference = test[,1], positive = '1') # k = 7일 때, knn 분류
cm.knn.7$overall[["Accuracy"]]

cm.knn.10 <- caret::confusionMatrix(data = knn.10, reference = test[,1], positive = '1') # k = 10일 때, knn 분류
cm.knn.10$overall[["Accuracy"]]

# 분류를 가장 잘하는 최적의 k값을 찾기 위한 함수구현(정분류율 기준)
result <- numeric()
k=1:20

for(i in k){
  
  knn.i <- knn(train = train_x, test = test_x, cl = train_y, k = i)
  cm.knn.i <- caret::confusionMatrix(data = knn.i, reference = test[,1], positive = '1')
  result[i] <- cm.knn.i$overall[["Accuracy"]]
  
}
result
sort(result,decreasing=T)
which(result==max(result))  # 정확도가 가장 높은 k

# 예측을 통한 분석 지표 출력
library(caret)
knn.17 <- knn(train = train_x, test = test_x, cl = train_y, k = 17) # k = 17일 때, knn 분류
cm.knn.17 <- caret::confusionMatrix(data = knn.17, reference = test[,1], positive='1')
cm.knn.17$table # confusion matrix 출력

cm.knn.17$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.knn.17$overall[["Accuracy"]] # 오분류율 값 출력
cm.knn.17$byClass[["Sensitivity"]] # 민감도 값 출력
cm.knn.17$byClass[["Specificity"]] # 특이도 값 출력
cm.knn.17$byClass[["Precision"]] # 정밀도 값 출력
cm.knn.17$byClass[["Recall"]] # 재현율 값 출력
cm.knn.17$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
knn.17 <- knn(train = train_x, test = test_x, cl = train_y, k = 17,  # k = 17일 때, knn 분류
              prob = TRUE)
knn.17 <- attr(knn.17, "prob") 
auc(actual = test[,1], predicted = knn.17) # AUC값 0.5339923


# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 KNN 을 실시하고 오분류표를 만들어보자.
library(class)

# knn 함수를 활용하여 K-NN 분석 실시
train_x <- train.iris[,-5]
test_x <- test.iris[,-5]
train_y <- train.iris[,5]

knn.iris.21 <- knn(train = train_x, test = test_x, cl = train_y, k = 21)

# 예측을 통한 분석 지표 출력
library(caret)
cm.knn.iris.21 <- caret::confusionMatrix(data = knn.iris.21, reference =  test.iris[,5], positive = '1')
print(cm.knn.iris.21)

##### 7절. 인공신경망 모형(Artificial Neural Network)

# nnet 함수를 활용하여 인공신경망 분석 실시
# install.packages("nnet")
library(nnet)
set.seed(1231)
nn.model <- nnet(credit.rating ~ ., # 45개의 가중치가 주어졌고 iteration이 반복될수록 error이 줄고 있음.
                 data = train,
                 size=2,
                 maxit=200,
                 decay=5e-04)
nn.model

summary(nn.model)

# 예측을 통한 분석 지표 출력
library(caret)
pred.nn <- predict(nn.model, test[,-1], type="class")    

cm.nn <- caret::confusionMatrix(data = as.factor(pred.nn), reference = test[,1], positive='1')
cm.nn$table # confusion matrix 출력

cm.nn$overall[["Accuracy"]] # 정분류율 값 출력
1 - cm.nn$overall[["Accuracy"]] # 오분류율 값 출력
cm.nn$byClass[["Sensitivity"]] # 민감도 값 출력
cm.nn$byClass[["Specificity"]] # 특이도 값 출력
cm.nn$byClass[["Precision"]] # 정밀도 값 출력
cm.nn$byClass[["Recall"]] # 재현율 값 출력
cm.nn$byClass[["F1"]] # F1-score 값 출력

# AUC 값 산출
library(ModelMetrics)
pred.nn <- predict(nn.model, test[,-1])     # 확률값 출력시 type 별도 지정 필요 없음
auc(actual = test[,1], predicted = as.factor(pred.nn)) # AUC값 0.7336329

# Q) 앞서 분리한 iris 데이터의 Species를 분류하는 인공신경망 모형(Artificial Neural Network)을 실시하고 오분류표를 만들어보자.
# nnet 함수를 활용하여 인공신경망 분석 실시
library(nnet)
set.seed(1231)
nn.model2 <- nnet(Species ~ .,
                  data = train.iris,
                  size = 2,
                  maxit = 200,
                  decay = 5e-04)
nn.model2

# 예측을 통한 분석 지표 출력
library(caret)
pred.nn2 <- predict(nn.model2 , test.iris[,-5], type="class")

cm.nn2 <- caret::confusionMatrix(data = as.factor(pred.nn2), reference =  test.iris[,5], positive = '1')
print(cm.nn2)

##===============================================================
##### 3장. 군집 분석

##### 2절. 계층적 군집 분석

# Q) iris 데이터로 거리를 구하고 최단, 최장, 평균연결법을 실시해보자.
# dist 함수를 활용하여 거리 구하기
data(iris)
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)
train.iris <- iris[idx,]
train.data <- train.iris[,-5]

dist <- dist(train.data, "euclidean")
dist

# hclust 함수를 활용하여 계층적 군집분석
# 군집화 결과를 더 잘 보이기 위해 dist를 제곱하여 반영함
iris.single <- hclust(dist^2, method = "single") # method="single"를 통해 최단 연결법 실행
plot(iris.single) # 덴드로그램은 계층적 군집화 결과를 확인할 수 있음(연습만 가능)

iris.complete <- hclust(dist^2, method = "complete") # method="complete"를 통해 최장 연결법 실행
plot(iris.complete) # 덴드로그램은 계층적 군집화 결과를 확인할 수 있음(연습만 가능)

iris.average <- hclust(dist^2, method = "average") # method="average"를 통해 평균 연결법 실행
plot(iris.average)# 덴드로그램은 계층적 군집화 결과를 확인할 수 있음(연습만 가능)

# 계층적 군집 결과 그룹 나누기와 덴드로그램 구분 짓기
group <- cutree(iris.average, k = 3)
group

plot(iris.average)
rect.hclust(iris.average, k = 3, border = "red")

##### 3절. 비계층적 군집 분석

# Q) iris 데이터로 kmeans 군집분석을 해보자.
# kmeans 함수를 활용하여 kmeans 군집분석 실시
iris.kmeans <- kmeans(train.data, centers=3) # k = 3인 k-means 군집분석 수행
iris.kmeans

table(train.iris[,5], iris.kmeans$cluster)


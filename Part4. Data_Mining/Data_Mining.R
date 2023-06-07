#################################################################
#                    PART 04) ������ ���̴�
#################################################################

##===============================================================
##### 1��. ������ ���Ұ� �����м�

##### 1�� | ������ ����
rm(list=ls())

# ===================================================================== #
# sample
# ===================================================================== #

# Q) credit �����͸� train, validation, test�� �����غ���.
# ������ ���� ��, ������ �ҷ�����
setwd("C:/Users/Administrator/Desktop/R_Engineer_Big_Data_Analysis/Part4. Data_Mining/")   # ���������� working directory�� �ݵ�� �����Ͻñ� �ٶ��ϴ�.
credit.df <- read.csv("credit_final.csv")

nrow(credit.df)  # �м� �������� �� ������ �ľ�

set.seed(1111) # ������ �����ϰ� ����ǵ��� ������Ű�� �Լ�

idx <- sample(3, nrow(credit.df), # idx�� 1,2,3�� credit ������ �� ������ �����ϰ� ����
              replace = T, # ���� ���� ����
              prob = c(0.5,0.3,0.2)) # 1�� train���� ��ü�� 50%,
# 2�� validation���� ��ü�� 30%,
# 3�� test�� ��ü�� 20%

train <- credit.df[idx==1,]
valid <- credit.df[idx==2,]
test <- credit.df[idx==3,]

nrow(train) # train �������� �� ���� �ľ�
nrow(valid) # validation �������� �� ���� �ľ�
nrow(test) # test �������� �� ���� �ľ�

# ===================================================================== #
# createDataPartition
# ===================================================================== #
# Q) credit �����͸� train, test�� �����غ���.

# install.packages("caret")
library(caret)
setwd("C:/Users/Administrator/Desktop/")
credit.df <- read.csv("credit_final.csv")

part<-createDataPartition(credit.df$credit.rating, # ���������� credit.rating�� ����
                          times = 1, # ������ ������ ������ 1���� ����
                          p = 0.7) # �Ʒõ����͸� 70%�� ����

parts <- as.vector(part$Resample1) # ���ȣ�� ���������� ��ȯ
train <- credit.df[parts,]
test <- credit.df[-parts,]

nrow(train)
nrow(test)

##### 2�� | ���� �м�

# ===================================================================== #
# ���з�ǥ(Confusion Matrix) (�м���ǥ ����)
# ===================================================================== #

# install.packages("caret")
library(caret)

predicted <- factor(c(1,0,0,1,1,1,0,0,0,1,1,1))
actual <- factor(c(1,0,0,1,1,0,1,1,0,1,1,1))

cm <- caret::confusionMatrix(predicted,actual)    # predicted ���� actual �� ��� factor�� ������ �Ǿ� �־�� ��

# ����� confusionMarix�� list������ list���� ���
# �� ����Ʈ�� ���ϴ� ��ǥ���� ���������� ����� ������
names(cm)
# [1] "positive" "table"    "overall"  "byClass"  "mode"     "dots"   
# ��ǥ���� "overall"�� "byClass"�� list������ ����Ǿ�����
# overall�� ����Ǿ� �ִ� ��ǥ Ȯ��
names(cm$overall)
# byClass�� ����Ǿ� �ִ� ��ǥ Ȯ��
names(cm$byClass)

# confusion matrix ���
cm$table

# ���з��� �� ���
cm$overall[["Accuracy"]]
cm$overall[[1]]

# ���з��� �� ���
1 - cm$overall[["Accuracy"]]
1 - cm$overall[[1]]

# �ΰ��� �� ���
cm$byClass[["Sensitivity"]]
cm$byClass[[1]]

# Ư�̵� �� ���
cm$byClass[["Specificity"]]
cm$byClass[[2]]

# ���е� �� ���
cm$byClass[["Precision"]]
cm$byClass[[5]]

# ������ �� ���
cm$byClass[["Recall"]]
cm$byClass[[6]]

# F1-score �� ���
cm$byClass[["F1"]]
cm$byClass[[7]]

# ===================================================================== #
# AUC
# ===================================================================== #
# install.packages("ModelMetrics ")
library(ModelMetrics)

predicted <- factor(c(1,0,0,1,1,1,0,0,0,1,1,1))
actual <- factor(c(1,0,0,1,1,0,1,1,0,1,1,1))
auc(actual,predicted) # AUC �� Ȯ��


# ===================================================================== #
# ���� ��ǥ
# ===================================================================== #
# Q) ������ ���� Ȱ���Ͽ� MSE, RMSE, MAE�� ���غ���

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
##### 2��. �з� �м�

##### 1��. ������ƽ ȸ�ͺм�

# Q. credit �����͸� �����ϰ�, train �����ͷ� ������ƽ ȸ�͸��� ����� ����.
# credit ������ �ҷ�����
setwd("C:/Users/Administrator/Desktop/")     # ���������� working directory�� �ݵ�� �����Ͻñ� �ٶ��ϴ�.
credit <- read.csv("credit_final.csv")
class(credit$credit.rating)  # ���Ӻ��� ���� �ľ� ���, integer���� �� Ȯ��
credit$credit.rating <- factor(credit$credit.rating)  # ���Ӻ��� factor ��ȯ

str(credit) # ������ ���� �ľ�

# ������ ���� : train 70%, test 30%
set.seed(123)
idx <- sample(1:nrow(credit), nrow(credit)*0.7, replace=FALSE)
train <- credit[idx,]
test <- credit[-idx,]

# ������ƽ ȸ�ͺм� �ǽ�
logit.model <- glm(credit.rating ~ .,
                   data = train,
                   family = "binomial")
summary(logit.model) # ȸ�Ͱ���� p-value�� ���Ǽ��� 0.05���� ���� ��Ÿ���� ������ ����

# step �Լ��� Ȱ���Ͽ� �ٽ� �м� �ǽ�

# step �Լ��� scope ���� �� upper�� ��� ������ �ֱ� ����  
full <- ~ account.balance + credit.duration.months + previous.credit.payment.status +
  credit.purpose + credit.amount + savings + employment.duration + installment.rate +
  marital.status + guarantor + residence.duration + current.assets + age + other.credits +
  apartment.type + bank.credits + occupation + dependents + telephone + foreign.worker

step.logit.model <- step(glm(credit.rating ~ 1, data = train, family = "binomial"),
                         scope = list(lower = ~ 1, upper = full), # �����(lower=~1)���� ��� ����(upper=~���� ��� ����)
                         direction = "both") # �ܰ躰 ����(��both��) ����
summary(step.logit.model)

# ������ ���� �м� ��ǥ ���
library(caret)
pred <- predict(step.logit.model, test[,-1], type = "response") # �������� ��response���� �����Ͽ� Ȯ������ ���
pred1 <- as.data.frame(pred)    # ����� data.frame���� ����
pred.logit <- ifelse(pred1$pred < 0.5, 0, 1) # Ȯ������ �����Ƿ�, 0.5�� �������� 0, 1 ���� ����

cm.logit <- caret::confusionMatrix(data = as.factor(pred.logit), reference = test[,1], positive = '1')

cm.logit$table # confusion matrix ���

cm.logit$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.logit$overall[["Accuracy"]] # ���з��� �� ���

cm.logit$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.logit$byClass[["Specificity"]] # Ư�̵� �� ���
cm.logit$byClass[["Precision"]] # ���е� �� ���
cm.logit$byClass[["Recall"]] # ������ �� ���
cm.logit$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
# AUC �� ����
library(ModelMetrics)
pred <- predict(step.logit.model, test[,-1], type = "response") # �������� ��response���� �����Ͽ� Ȯ������ ���
auc(actual = test[,1], predicted = pred.logit) # AUC�� 0.6702182


# Q) iris �������� Species�� �з��ϴ� ���� ������ƽ ȸ�ͺм��� �ǽ��ϰ� ���з�ǥ�� ������.
data(iris)
set.seed(123)
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)
train.iris <- iris[idx,]
test.iris <- iris[-idx,]

# train �����ͷ� ���� ������ƽ ȸ�ͺм� �ǽ�
library(nnet)
mul.logit.model <- multinom(Species ~. , data = train.iris)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.mul.logit <- predict(mul.logit.model, test.iris[,-5])

cm.mul.logit <- caret::confusionMatrix(data = pred.mul.logit, reference =  test.iris[,5], positive = '1')
print(cm.mul.logit)


##### 2��. �ǻ��������
# �ռ� ������ credit �������� train �����ͷ� �ǻ�������� ���� ����� ����.

library(rpart)
library(rpart.plot) # ���������� ��밡��

# �ǻ�������� �ǽ�
dt.model <- rpart(credit.rating ~ .,          # ���Ӻ���(credit.rating)�� ��� ������ ���������� ���
                  method = "class",           # method�� �з��� ��class�� ����
                  data = train,
                  control = rpart.control(maxdepth = 5,    # �ǻ���������� �ִ� ���̴� 5������
                                          minsplit = 15))  # ��忡�� �ּ� ����ġ�� 15�� �̻�
# 
# dt.model2 <- train(credit.rating ~ .,
#                    data = train,
#                    method = "rpart",
#                    trControl=trainControl())

prp(dt.model, type = 4, extra = 2) # �ð�ȭ�� Ȯ��(���迡���� �Ұ�,���������� ��밡��)

print(dt.model)

# # rpart �Լ��� Ȱ���Ͽ� �ǻ���������м� �ǽ�(���� ���� ����)
# dt.model$cptable  
# opt<-which.min(dt.model$cptable[,"xerror"]) # x-error�� ���� ���� split ������ ����
# cp<-dt.model$cptable[opt,"CP"] 
# prune.c <- prune(dt.model,cp=cp) 

# plotcp(dt.model)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.dt <- predict(dt.model, test[,-1], type = "class")     # �������� ��class���� �����Ͽ� �з� �׷��� ���
cm.dt <- caret::confusionMatrix(data = pred.dt, reference = test[,1], positive='1')

cm.dt$table # confusion matrix ���

cm.dt$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.dt$overall[["Accuracy"]] # ���з��� �� ���
cm.dt$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.dt$byClass[["Specificity"]] # Ư�̵� �� ���
cm.dt$byClass[["Precision"]] # ���е� �� ���
cm.dt$byClass[["Recall"]] # ������ �� ���
cm.dt$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.dt <- predict(dt.model, test[,-1], type = "prob")[,2]     # �������� ��prob���� �����Ͽ� Ȯ�� ���� ���
auc(actual = test[,1], predicted = pred.dt) # AUC�� 0.7036714

# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� �ǻ���������м��� �ǽ��ϰ� ���з�ǥ�� ������.
# train �����ͷ� ���� �з� ���� �ǽ�
library(rpart)
library(rpart.plot) # ���������� ��밡��
dt.model2 <- rpart(Species ~. , data = train.iris)
prp(dt.model2,type = 4,extra = 2) # �ð�ȭ�� Ȯ��(���迡���� �Ұ�,���������� ��밡��)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.dt2 <- predict(dt.model2, test.iris[,-5], type="class")

cm.dt2 <- caret::confusionMatrix(data = pred.dt2, reference =  test.iris[,5], positive = '1')
print(cm.dt2)

##### 3��. �ӻ��(ensemble) ��� - ���(Bagging)

# install.packages("ipred")
library(ipred)

# bagging �Լ��� Ȱ���Ͽ� bagging�м� �ǽ�
bag.model <- bagging(credit.rating ~ .,
                     data = train,
                     nbagg = 15) #�ݺ� �Ǵ� Ʈ���� ���� 15

# ������ ���� �м� ��ǥ ���
library(caret)
pred.bg <- predict(bag.model, test[,-1], type = "class")
cm.bg <- caret::confusionMatrix(data = pred.bg, reference = test[,1], positive = '1')

cm.bg$table # confusion matrix ���

cm.bg$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.bg$overall[["Accuracy"]] # ���з��� �� ���
cm.bg$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.bg$byClass[["Specificity"]] # Ư�̵� �� ���
cm.bg$byClass[["Precision"]] # ���е� �� ���
cm.bg$byClass[["Recall"]] # ������ �� ���
cm.bg$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.bg <- predict(bag.model, test[,-1], type = "prob")[,2]      # �������� ��prob���� �����Ͽ� Ȯ�� ���� ���
auc(actual = test[,1], predicted = pred.bg) # AUC�� 0.7803338

# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� ����� �ǽ��ϰ� ���з�ǥ�� ������.
# train �����ͷ� ����  bagging�м� �ǽ�
bag.model2 <- bagging(Species ~.,
                      data = train.iris,
                      nbagg = 100) # �ݺ� �Ǵ� Ʈ���� ���� 15

# ������ ���� �м� ��ǥ ���
library(caret)
pred.bg2 <- predict(bag.model2, test.iris[,-5], type="class")

cm.bag2 <- caret::confusionMatrix(data = pred.bg2, reference =  test.iris[,5], positive = '1')
print(cm.bag2)

##### 3��. �ӻ��(ensemble) ��� - �ν���(boosting)

# xgboost �Լ��� Ȱ���Ͽ� boosting �м� �ǽ�
# install.packages("xgboost")
library(xgboost)

# 1) xgboost�� ��� ������ num ����
# 2) ��Ʈ���� ���
# 3) �ε����� 0���� ����

train_x <- data.matrix(train[,-1])
train_y <- as.integer(train[,1]) - 1 # �ε��� 0���� ������, ��ġ�� Ŭ����(1,0)�� ��ȯ 

test_x <- data.matrix(test[,-1])

xgb.train <- xgb.DMatrix(data = train_x, label = train_y) # xgboost�� DMatrix ������ ��ȯ

xgb.model <- xgboost(data = xgb.train,
                     max.depth = 2,
                     eta = 1,
                     # nthread = 2,
                     nrounds = 2,
                     objective = "binary:logistic") #logloss

# ������ ���� �м� ��ǥ ���
library(caret)

pred <- predict(xgb.model,test_x)
pred.xgb <- ifelse(pred < 0.5, 0, 1) # Ȯ������ �����Ƿ�, 0.5�� �������� 0, 1 ���� ����

cm.xgb <- caret::confusionMatrix(data = as.factor(pred.xgb), reference = test[,1], positive = '1')

cm.xgb$table # confusion matrix ���

cm.xgb$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.xgb$overall[["Accuracy"]] # ���з��� �� ���
cm.xgb$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.xgb$byClass[["Specificity"]] # Ư�̵� �� ���
cm.xgb$byClass[["Precision"]] # ���е� �� ���
cm.xgb$byClass[["Recall"]] # ������ �� ���
cm.xgb$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred <- predict(xgb.model,test_x)
auc(actual = test[,1], predicted = pred.xgb) # AUC�� 0.6437741


# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� �ν����� �ǽ��ϰ� ���з�ǥ�� ������.
# train �����ͷ� ����  boosting�м� �ǽ�

train_x <- data.matrix(train.iris[,-5])
train_y <- as.integer(train.iris[,5]) - 1 # �ε��� 0���� ������, ��ġ�� Ŭ����(2,1,0)�� ��ȯ 
test_x <- data.matrix(test.iris[,-5])

xgb.train <- xgb.DMatrix(data = train_x, label = train_y) # xgboost�� DMatrix ������ ��ȯ

xgb.model2 <- xgboost(data = xgb.train,
                      max.depth = 2,
                      eta = 1,
                      nrounds = 2,
                      objective = "multi:softprob",
                      num_class = 3,
)

# ������ ���� �м� ��ǥ ���
library(caret)
pred <- as.data.frame(predict(xgb.model2, test_x, reshape = T)) # reshape = T�� print�� �������� case �� ���� �� �� ���,
# ���� ��(������)�� ��� ���·� �ٲٴ� ��

# Ȯ�� ���� �����Ƿ� �ະ�� ���� ū Ȯ���� ������ �������� �����ϴ� ������� ���� �󺧰� ������
colnames(pred) = levels(iris$Species) # ��� ������ ���� ���� �������� �󺧰�(Species)���� ������
pred.xgb2 = colnames(pred)[apply(pred,1,which.max)] # �� �ึ�� ���� ū Ȯ���� ������ ���� �ε����� ��� �󺧰� ����
cm.xgb2 <- caret::confusionMatrix(data = as.factor(pred.xgb2), reference =  test.iris[,5], positive = '1')
print(cm.xgb2)

##### 3��. �ӻ��(ensemble) ��� - ����������Ʈ

# install.packages("randomForest")
library(randomForest)
# randomForest �Լ��� Ȱ���Ͽ� RandomForest�м� �ǽ�
rf.model <- randomForest(credit.rating ~ .,
                         data = train,
                         ntree = 50, # ���� 50�� ���
                         mtry = sqrt(20), # ����� ������ ����(classification�̹Ƿ� sqrt(20)��)
                         importance = T) #�����߿䵵 ����� Ȯ��

rf.model  

rf.model$importance #�����߿䵵 ��� Ȯ��
#varImpPlot(rf.model) #�����߿䵵 �ð�ȭ(���迡���� �Ұ�, ���������� ��� ����)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.rf <- predict(rf.model, test[,-1], type = "class")
cm.rf <- caret::confusionMatrix(data = pred.rf, reference = test[,1], positive = '1')
cm.rf$table # confusion matrix ���

cm.rf$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.rf$overall[["Accuracy"]] # ���з��� �� ���
cm.rf$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.rf$byClass[["Specificity"]] # Ư�̵� �� ���
cm.rf$byClass[["Precision"]] # ���е� �� ���
cm.rf$byClass[["Recall"]] # ������ �� ���
cm.rf$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.rf <- predict(rf.model, test[,-1], type = "prob")[,2]      # �������� ��prob���� �����Ͽ� Ȯ�� ���� ���
auc(actual = test[,1], predicted = pred.rf) # AUC�� 0.7724005

# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� ����������Ʈ�� �ǽ��ϰ� ���з�ǥ�� ������.
# train �����ͷ� ���� ����������Ʈ �ǽ�
library(randomForest)

rf.model2 <- randomForest(Species ~. ,
                          data = train.iris,
                          ntree = 500, # ���� 500�� ���
                          mtry = sqrt(4), # ����� ������ ����(classification�̹Ƿ� sqrt(4)��)
                          importance = T) #�����߿䵵 ����� Ȯ��

# ������ ���� �м� ��ǥ ���
library(caret)
pred.rf2 <- predict(rf.model2, test.iris[,-5], type="class")

cm.rf2 <- caret::confusionMatrix(data = pred.rf2, reference =  test.iris[,5], positive = '1')
print(cm.rf2)

##### 4��. ����Ʈ���͸ӽ�(SVM)

# install.packages("e1071")
library(e1071)

# tune.svm �Լ��� Ȱ���Ͽ� ������ �Ķ���Ͱ� ã��
tune.svm(credit.rating ~ .,
         data = credit,
         gamma = 10^(-6:-1), #���⼭�� 6*2=12���� ���տ��� ��������� �̷����
         cost = 10^(1:2)) 

# svm �Լ��� Ȱ���Ͽ� SVM�м� �ǽ�
svm.model <- svm(credit.rating ~ .,
                 data = train,
                 kernel = "radial",
                 gamma = 0.01,
                 cost = 10,
                 probability = TRUE) # Ȯ���� ��½� �ʿ�
summary(svm.model) 

# ������ ���� �м� ��ǥ ���
library(caret)
pred.svm <- predict(svm.model, test, type="class")   
cm.svm <- caret::confusionMatrix(data = pred.svm, reference = test[,1], positive='1')
cm.svm$table # confusion matrix ���

cm.svm$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.svm$overall[["Accuracy"]] # ���з��� �� ���
cm.svm$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.svm$byClass[["Specificity"]] # Ư�̵� �� ���
cm.svm$byClass[["Precision"]] # ���е� �� ���
cm.svm$byClass[["Recall"]] # ������ �� ���
cm.svm$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.svm <- predict(svm.model, test, probability = TRUE) # Ȯ���� ��½� probability = TRUE�� �ʿ�
pred.svm <-  attr(pred.svm, 'probabilities')[,1] # Ȯ���� ����� ���ؼ��� �ش� �ڵ带 ����ؾ���
auc(actual = test[,1], predicted = pred.svm) # AUC�� 0.7575353


# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� SVM�м��� �ǽ��ϰ� ���з�ǥ�� ������.
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

# ������ ���� �м� ��ǥ ���
library(caret)
pred.svm2 <- predict(svm.model2, test.iris[,-5], type="class")

cm.svm2 <- caret::confusionMatrix(data = pred.svm2, reference =  test.iris[,5], positive = '1')
print(cm.svm2)

##### 5��. ���̺� ������ �з�(Naive Bayes Classification)

# install.packages("e1071")
library(e1071)

# naiveBayes �Լ��� Ȱ���Ͽ� ���̺� ������ �з��м� �ǽ�
nb.model <- naiveBayes(credit.rating ~ .,
                       data = train,
                       laplace = 0)
nb.model

# ������ ���� �м� ��ǥ ���
library(caret)
pred.nb <- predict(nb.model, test, type="class")   
cm.nb <- caret::confusionMatrix(data = pred.nb, reference = test[,1], positive='1')
cm.nb$table # confusion matrix ���

cm.nb$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.nb$overall[["Accuracy"]] # ���з��� �� ���
cm.nb$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.nb$byClass[["Specificity"]] # Ư�̵� �� ���
cm.nb$byClass[["Precision"]] # ���е� �� ���
cm.nb$byClass[["Recall"]] # ������ �� ���
cm.nb$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.nb <- predict(nb.model, test, type = "raw")[,2]   # Ȯ���� ��½� type�� raw�� ���� �ʿ�
auc(actual = test[,1], predicted = pred.nb) # AUC�� 0.7708858

# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� ���̺� ������ �з��м��� �ǽ��ϰ� ���з�ǥ�� ������.
library(e1071)

nb.model2 <- naiveBayes(Species ~ .,
                        data = train.iris,
                        laplace = 0)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.nb2 <- predict(nb.model2 , test.iris[,-5], type="class")

cm.nb2 <- caret::confusionMatrix(data = pred.nb2, reference =  test.iris[,5], positive = '1')
print(cm.nb2)

##### 6��. K-NN(K-Nearest Neighbor)

# install.packages("class")
library(class)

# knn �Լ��� Ȱ���Ͽ� K-NN �м� �ǽ�
train_x <- train[,-1]
test_x <- test[,-1]
train_y <- train[,1]

knn.3 <- knn(train = train_x, test = test_x, cl = train_y, k = 3)
knn.7 <- knn(train = train_x, test = test_x, cl = train_y, k = 7)
knn.10 <- knn(train = train_x, test = test_x, cl = train_y, k = 10)

# ������ k�� ���� �з� ��Ȯ�� Ȯ��
cm.knn.3 <- caret::confusionMatrix(data = knn.3, reference = test[,1], positive = '1') # k = 3�� ��, knn �з�
cm.knn.3$overall[["Accuracy"]]

cm.knn.7 <- caret::confusionMatrix(data = knn.7, reference = test[,1], positive = '1') # k = 7�� ��, knn �з�
cm.knn.7$overall[["Accuracy"]]

cm.knn.10 <- caret::confusionMatrix(data = knn.10, reference = test[,1], positive = '1') # k = 10�� ��, knn �з�
cm.knn.10$overall[["Accuracy"]]

# �з��� ���� ���ϴ� ������ k���� ã�� ���� �Լ�����(���з��� ����)
result <- numeric()
k=1:20

for(i in k){
  
  knn.i <- knn(train = train_x, test = test_x, cl = train_y, k = i)
  cm.knn.i <- caret::confusionMatrix(data = knn.i, reference = test[,1], positive = '1')
  result[i] <- cm.knn.i$overall[["Accuracy"]]
  
}
result
sort(result,decreasing=T)
which(result==max(result))  # ��Ȯ���� ���� ���� k

# ������ ���� �м� ��ǥ ���
library(caret)
knn.17 <- knn(train = train_x, test = test_x, cl = train_y, k = 17) # k = 17�� ��, knn �з�
cm.knn.17 <- caret::confusionMatrix(data = knn.17, reference = test[,1], positive='1')
cm.knn.17$table # confusion matrix ���

cm.knn.17$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.knn.17$overall[["Accuracy"]] # ���з��� �� ���
cm.knn.17$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.knn.17$byClass[["Specificity"]] # Ư�̵� �� ���
cm.knn.17$byClass[["Precision"]] # ���е� �� ���
cm.knn.17$byClass[["Recall"]] # ������ �� ���
cm.knn.17$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
knn.17 <- knn(train = train_x, test = test_x, cl = train_y, k = 17,  # k = 17�� ��, knn �з�
              prob = TRUE)
knn.17 <- attr(knn.17, "prob") 
auc(actual = test[,1], predicted = knn.17) # AUC�� 0.5339923


# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� KNN �� �ǽ��ϰ� ���з�ǥ�� ������.
library(class)

# knn �Լ��� Ȱ���Ͽ� K-NN �м� �ǽ�
train_x <- train.iris[,-5]
test_x <- test.iris[,-5]
train_y <- train.iris[,5]

knn.iris.21 <- knn(train = train_x, test = test_x, cl = train_y, k = 21)

# ������ ���� �м� ��ǥ ���
library(caret)
cm.knn.iris.21 <- caret::confusionMatrix(data = knn.iris.21, reference =  test.iris[,5], positive = '1')
print(cm.knn.iris.21)

##### 7��. �ΰ��Ű�� ����(Artificial Neural Network)

# nnet �Լ��� Ȱ���Ͽ� �ΰ��Ű�� �м� �ǽ�
# install.packages("nnet")
library(nnet)
set.seed(1231)
nn.model <- nnet(credit.rating ~ ., # 45���� ����ġ�� �־����� iteration�� �ݺ��ɼ��� error�� �ٰ� ����.
                 data = train,
                 size=2,
                 maxit=200,
                 decay=5e-04)
nn.model

summary(nn.model)

# ������ ���� �м� ��ǥ ���
library(caret)
pred.nn <- predict(nn.model, test[,-1], type="class")    

cm.nn <- caret::confusionMatrix(data = as.factor(pred.nn), reference = test[,1], positive='1')
cm.nn$table # confusion matrix ���

cm.nn$overall[["Accuracy"]] # ���з��� �� ���
1 - cm.nn$overall[["Accuracy"]] # ���з��� �� ���
cm.nn$byClass[["Sensitivity"]] # �ΰ��� �� ���
cm.nn$byClass[["Specificity"]] # Ư�̵� �� ���
cm.nn$byClass[["Precision"]] # ���е� �� ���
cm.nn$byClass[["Recall"]] # ������ �� ���
cm.nn$byClass[["F1"]] # F1-score �� ���

# AUC �� ����
library(ModelMetrics)
pred.nn <- predict(nn.model, test[,-1])     # Ȯ���� ��½� type ���� ���� �ʿ� ����
auc(actual = test[,1], predicted = as.factor(pred.nn)) # AUC�� 0.7336329

# Q) �ռ� �и��� iris �������� Species�� �з��ϴ� �ΰ��Ű�� ����(Artificial Neural Network)�� �ǽ��ϰ� ���з�ǥ�� ������.
# nnet �Լ��� Ȱ���Ͽ� �ΰ��Ű�� �м� �ǽ�
library(nnet)
set.seed(1231)
nn.model2 <- nnet(Species ~ .,
                  data = train.iris,
                  size = 2,
                  maxit = 200,
                  decay = 5e-04)
nn.model2

# ������ ���� �м� ��ǥ ���
library(caret)
pred.nn2 <- predict(nn.model2 , test.iris[,-5], type="class")

cm.nn2 <- caret::confusionMatrix(data = as.factor(pred.nn2), reference =  test.iris[,5], positive = '1')
print(cm.nn2)

##===============================================================
##### 3��. ���� �м�

##### 2��. ������ ���� �м�

# Q) iris �����ͷ� �Ÿ��� ���ϰ� �ִ�, ����, ��տ������ �ǽ��غ���.
# dist �Լ��� Ȱ���Ͽ� �Ÿ� ���ϱ�
data(iris)
idx <- sample(1:nrow(iris), nrow(iris)*0.7, replace=FALSE)
train.iris <- iris[idx,]
train.data <- train.iris[,-5]

dist <- dist(train.data, "euclidean")
dist

# hclust �Լ��� Ȱ���Ͽ� ������ �����м�
# ����ȭ ����� �� �� ���̱� ���� dist�� �����Ͽ� �ݿ���
iris.single <- hclust(dist^2, method = "single") # method="single"�� ���� �ִ� ����� ����
plot(iris.single) # ����α׷��� ������ ����ȭ ����� Ȯ���� �� ����(������ ����)

iris.complete <- hclust(dist^2, method = "complete") # method="complete"�� ���� ���� ����� ����
plot(iris.complete) # ����α׷��� ������ ����ȭ ����� Ȯ���� �� ����(������ ����)

iris.average <- hclust(dist^2, method = "average") # method="average"�� ���� ��� ����� ����
plot(iris.average)# ����α׷��� ������ ����ȭ ����� Ȯ���� �� ����(������ ����)

# ������ ���� ��� �׷� ������� ����α׷� ���� ����
group <- cutree(iris.average, k = 3)
group

plot(iris.average)
rect.hclust(iris.average, k = 3, border = "red")

##### 3��. ������� ���� �м�

# Q) iris �����ͷ� kmeans �����м��� �غ���.
# kmeans �Լ��� Ȱ���Ͽ� kmeans �����м� �ǽ�
iris.kmeans <- kmeans(train.data, centers=3) # k = 3�� k-means �����м� ����
iris.kmeans

table(train.iris[,5], iris.kmeans$cluster)

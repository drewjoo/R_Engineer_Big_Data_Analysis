# 기출3회
setwd('C:/Users/Administrator/Downloads/PART 06. 기출복원문제')

# 작업형2
# Data input

X_train = read.csv("job_change_X_train.csv")
X_test = read.csv("job_change_X_test.csv")
y_train = read.csv("job_change_y_train.csv")

# 결측값 확인하기
apply(is.na(X_train),2,sum) # 없음으로 확인
apply(X_train == '',2, sum)

apply(is.na(X_test),2,sum) # 없음으로 확인
apply(X_test == '',2, sum)

# 결측치 1000이상인 값은 제거
X_train = subset(X_train, select = -c(major_discipline,company_size,company_type))
X_test = subset(X_test, select = -c(major_discipline,company_size,company_type))


# city열 제거
X_train=subset(X_train, select = -c(city))
X_test=subset(X_test, select = -c(city))


# 다른 결측치에 대해서는 최빈값 대체
# table(X_train$enrolled_university) # No 최다
# table(X_train$education_level) # Graduate 최다

# table(X_test$enrolled_university) # No 최다
# table(X_test$education_level) # Graduate 최다

X_train$enrolled_university[X_train$enrolled_university == ''] = 'No'
X_test$enrolled_university[X_test$enrolled_university == ''] = 'No'

X_train$education_level[X_train$education_level == ''] = 'Graduate'
X_test$education_level[X_test$education_level == ''] = 'Graduate'

# 범주화
X_train$gender=factor(X_train$gender)
X_train$relevent_experience = factor(X_train$relevent_experience)
X_train$enrolled_university = factor(X_train$enrolled_university)
X_train$education_level = factor(X_train$education_level)

X_test$gender=factor(X_test$gender)
X_test$relevent_experience = factor(X_test$relevent_experience)
X_test$enrolled_university = factor(X_test$enrolled_university)
X_test$education_level = factor(X_test$education_level)

# 범주형 변수 -> 연속형 변수
X_train$last_new_job=ifelse(X_train$last_new_job == 'never', 0, X_train$last_new_job)
X_train$last_new_job=ifelse(X_train$last_new_job == '>4', 5, X_train$last_new_job)
X_train$experience = ifelse(X_train$experience == '<1',0, X_train$experience)
X_train$experience = ifelse(X_train$experience == '>20',21, X_train$experience)
X_train$last_new_job = as.numeric(X_train$last_new_job)
X_train$experience = as.numeric(X_train$experience)

X_test$last_new_job=ifelse(X_test$last_new_job == 'never', 0, X_test$last_new_job)
X_test$last_new_job=ifelse(X_test$last_new_job == '>4', 5, X_test$last_new_job)
X_test$experience = ifelse(X_test$experience == '<1',0, X_test$experience)
X_test$experience = ifelse(X_test$experience == '>20',21, X_test$experience)
X_test$last_new_job = as.numeric(X_test$last_new_job)
X_test$experience = as.numeric(X_test$experience)


# Scale
X_train$city_development_index = scale(X_train$city_development_index)
X_train$experience = scale(X_train$experience)
X_train$last_new_job = scale(X_train$last_new_job)
X_train$training_hours = scale(X_train$training_hours)

X_test$city_development_index = scale(X_test$city_development_index)
X_test$experience = scale(X_test$experience)
X_test$last_new_job = scale(X_test$last_new_job)
X_test$training_hours = scale(X_test$training_hours)

# X_train data에 id열 제거
X_train = subset(X_train, select = -c(enrollee_id))
# target열 생성
X_train$target = factor(y_train$target)


# modeling
library(randomForest)
model=randomForest(target~., 
             data = X_train, 
             ntree = 500,
             mtry = sqrt(8), 
             importance = TRUE)
y_pred<-predict(model, X_test[,-1], type = 'class')
length(y_pred)

df=data.frame(X_test$enrollee_id, y_pred)
colnames(df) = c('enrollee_id', 'target')
head(df)

# write.csv(df, '12345.csv', row.names = FALSE)




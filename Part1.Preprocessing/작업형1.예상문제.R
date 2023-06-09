#################################################################
#                  PART 02) 데이터 전처리 예상문제
#################################################################

##===============================================================
rm(list=ls())

# 연습 문제1 Solution
setwd('C:/Users/Administrator/Downloads/Part1.Preprocessing')
exam1 <- read.csv("Cars93.csv")
Wheelbase <- exam1$Wheelbase

# Wheelbase의 평균
Wheelbase_avg <- mean(Wheelbase)

# Wheelbase의 표준편차
Wheelbase_sd <- sd(Wheelbase)

# Case1. 평균 값에서 표준편차의 1.5배를 더하거나 빼는 경우
Low1.5 <- Wheelbase_avg - 1.5 * Wheelbase_sd 
Upp1.5 <- Wheelbase_avg + 1.5 * Wheelbase_sd
case1 <- Wheelbase_avg - mean(Wheelbase[Wheelbase > Low1.5 & Wheelbase < Upp1.5]) 

# Case2. 평균 값에서 표준편차의 2배를 더하거나 빼는 경우
Low2 <- Wheelbase_avg - 2 * Wheelbase_sd 
Upp2 <- Wheelbase_avg + 2 * Wheelbase_sd
case2 <- Wheelbase_avg - mean(Wheelbase[Wheelbase > Low2 & Wheelbase < Upp2]) 

# Case3. 평균 값에서 표준편차의 2.5배를 더하거나 빼는 경우
Low2.5 <- Wheelbase_avg - 2.5 * Wheelbase_sd 
Upp2.5 <- Wheelbase_avg + 2.5 * Wheelbase_sd
case3 <- Wheelbase_avg - mean(Wheelbase[Wheelbase > Low2.5 & Wheelbase < Upp2.5])

# 결과를 result에 할당
result <- round(case1 + case2 + case3, 4)

#결과 출력
print(result) 


##===============================================================
# 연습 문제2 Solution
exam2 <- read.csv("Cars93.csv")

# 순위를 구한 후 rank에 할당
rank <- rank(exam2$Length, ties.method = "average")

# 결과를 result에 할당
result <- round(sd(exam2$Length[rank <= 30]), 3)

# 결과 출력
print(result)


##===============================================================
# 연습 문제3 Solution
exam3 <- read.csv("Cars93.csv")

# 내림차순으로 정렬해 MaxPrice_sort에 할당
MaxPrice_sort <- sort(exam3$Max.Price, decreasing = TRUE)

# 오름차순으로 정렬해 MinPrice_sort에 할당
MinPrice_sort <- sort(exam3$Min.Price, decreasing = FALSE) # decreasing = FALSE 없어도 가능 

# 결과를 result에 할당
result <- round(sd(MaxPrice_sort - MinPrice_sort), 3)

# 결과 출력
print(result)


##===============================================================
# 연습 문제4 Solution
exam4 <- read.csv("Cars93.csv")

# min-max 정규화로 변환
Weight <- exam4$Weight
Weight_std <- (Weight - min(Weight))/(max(Weight) - min(Weight))

# 0.5보다 큰값들의 평균
avg_over0.5 <- mean(Weight_std[Weight_std >= 0.5])

# 0.5보다 작은 값들의 평균
avg_under0.5 <- mean(Weight_std[Weight_std < 0.5])

# 결과를 result에 할당
result <- round(abs(avg_over0.5 - avg_under0.5), 3)

# 결과 출력
result


##===============================================================
# 연습 문제5 Solution
exam5 <- read.csv("Cars93.csv")

# raw 유일값 조합
uniq1 <- unique(exam5[,c("Manufacturer", "Origin")]) 

# 앞 두글자만 추출
exam5$ss <- substr(exam5$Manufacturer, 1, 2)

# 유일값 조합
uniq2 <- unique(exam5[,c("ss", "Origin")])

# 결과를 result에 할당
result <- nrow(uniq1) + nrow(uniq2)

# 결과 출력
result


##===============================================================
# 연습문제6 Solution
exam6 <- read.csv("Cars93.csv")

# 그룹별 레코드 수
count_RPM_gp <- aggregate(RPM ~ Type + Man.trans.avail, data = exam6, FUN = length)$RPM

# 그룹별 RPM 합계
sum_RPM_gp <- aggregate(RPM ~ Type + Man.trans.avail, data = exam6, FUN = sum)$RPM

# 그룹별 RPM 중앙값
med_RPM_gp <- aggregate(RPM ~ Type + Man.trans.avail, data = exam6, FUN = median)$RPM

# 결과를 result에 할당
result <- round(sum(med_RPM_gp - sum_RPM_gp/count_RPM_gp), 0)

# 결과 출력
print(result)


##===============================================================
# 연습문제7 Solution
exam7 <- read.csv("Cars93.csv")

# RPM 변수의 결측치를 평균으로 대체
exam7$RPM[is.na(exam7$RPM)] <- mean(exam7$RPM, na.rm = TRUE)  

# RPM 변수 z-점수 표준화
RPM <- exam7$RPM
RPM_std <- (RPM - mean(RPM))/sd(RPM)

# Wheelbase 변수 z-점수 표준화
Wb <- exam7$Wheelbase
Wb_std <- (Wb - mean(Wb))/sd(Wb)

# 결과를 result에 할당
# 표준화된 Wheelbase에 상수 ???36을 곱한 값과 표준화된 RPM 변수의 차이값
result <- round(sd(Wb_std * (-36) - RPM_std), 3)

# 결과 출력
print(result)


##===============================================================
# 연습문제8 Solution
exam8 <- read.csv("Cars93.csv") 
df1 <- exam8
df2 <- exam8

## Case1.
# Price 변수의 결측치를 평균으로 대체
df1$Price[is.na(df1$Price)] <- mean(df1$Price, na.rm = T)

# Price 변수가 Max.Price 변수와 Min.Price의 평균보다 작은 레코드만을 추출
df1_1 <- df1[df1$Price < ((df1$Max.Price + df1$Min.Price)/2), ]

# Origin 그룹별 Price의 합계
origin_sum_price1 <- aggregate(Price ~ Origin, data = df1_1, FUN = sum)

##Case2.
# Price 변수의 결측치를 중앙값으로 대체
df2$Price[is.na(df2$Price)] <- median(df2$Price, na.rm = T)

# Price 변수가 Max.Price 변수와 Min.Price의 평균보다 작은 레코드만을 추출
df2_1 <- df2[df2$Price < quantile(df2$Min.Price, probs = 0.75),]

# Origin 그룹별 Price의 합계
origin_sum_price2 <- aggregate(Price ~ Origin, data = df2_1, FUN = sum)

# 결과를 result에 할당
result <- floor(max(origin_sum_price1$Price + origin_sum_price2$Price))

# 결과 출력
print(result)


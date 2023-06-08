# Set Working Directory
setwd('C:/Users/Administrator/Downloads/Secret PART 02. 작업형03_모의고사/data2')

# Test1.

#Q1.
exam1 = read.csv('Cars93.csv')

#a. 13.85
large = exam1$Price[exam1$Type == 'Large']
small = exam1$Price[exam1$Type == 'Small']

model1 = t.test(large, small, paired = F, var.equal = T)
res1 = round(model1$estimate[1]-model1$estimate[2],2)[[1]]
print(res1)

#b. 1.61
res2=round(model1$stderr,2)
print(res2)

#c. 17.1534, 기각
res3=round(model1$conf.int[2],4)
print(res3)
model1$p.value

#-----------------------------------

#Q2.

#a.100
exam2 = read.csv('dices.csv')
tb=table(exam2$scale)

model2=chisq.test(tb)
res1 = as.integer(model2$expected[[3]])
print(res1)

#b.307
res2=as.integer(model2$statistic)
print(res2)

#c.0, 기각각
res3= if(round(model2$p.value,4) < 0.0001){
  print(0)
} else print(round(model2$p.value))
print(res3)

#-----------------------------------

# Test2.

#Q1.
exam1=read.csv('survey_subset.csv', fileEncoding = 'euc-kr')
tb = table(exam1$성별, exam1$X1번문항)

model1 = chisq.test(tb)

#a. 87
res1 = model1$expected[1,3]
print(res1)

#b. 10
res2=as.integer(model1$statistic)
print(res2)

#c. 0.0404, 기각각
res3=round(model1$p.value,4)
print(res3)

#-----------------------------------

#Q2.
exam2=read.csv('Cars93.csv')
usa=exam2$Max_Price[exam2$Origin == 'USA']
non_usa=exam2$Max_Price[exam2$Origin == 'non-USA']

model2 = t.test(usa, non_usa, var.equal = F, paired = F)

#a. 2.63
res1 = round(model2$estimate[[2]] - model2$estimate[[1]],2)
print(res1)

#b. 2.32
res2=round(model2$stderr,2)
print(res2)

#c. 1.335, 채택택
res3 = -round(model2$statistic,4)[[1]]
print(res3)
model2$p.value

#-----------------------------------

# Test3.

#Q1.
exam1=read.csv('영화_순위리스트.csv', fileEncoding = 'euc-kr')

# 필요한 컬럼 각각 할당
genre = exam1['장르']
budget = exam1['예산']

# 장르별 예산 값 할당
budget_thriller = budget[genre == 'Thriller']
budget_comedy = budget[genre == 'Comedy']
budget_drama = budget[genre == 'Drama']
budget_action = budget[genre == 'Action']

## (a) 합동분산(pooled variancer)
# 집단별 표본 분산
var_i = c(var(budget_thriller), var(budget_comedy), var(budget_drama), var(budget_action))

# 집단별 관측치 수
n_i = c(length(budget_thriller), length(budget_comedy), length(budget_drama), length(budget_action))

# 합동분산 계산
N = sum(n_i)
k = 4 # 집단의 수

log_sp2 = log(sum((n_i-1)* var_i)/(N-k))
log_sp2 = round(log_sp2, 3)

cat(log_sp2)

#b. 13.44
model1 = bartlett.test(예산~장르, data = exam1)
res2 = round(model1$statistic[[1]],2)
print(res2)

#c. 0.0038 , 기각각
res3=round(model1$p.value,4)
print(res3)

#-----------------------------------

#Q2.
exam2 = read.csv('영화_순위리스트.csv', fileEncoding ='euc-kr')

#a. 548.1
res1=max(tapply(exam2$예산, exam2$장르, mean))-min(tapply(exam2$예산, exam2$장르, mean))
res1 = round(res1,1)
print(res1)

#b. 0.57
model2 = aov(예산 ~ 장르, data = exam2)
model2 = anova(model2)
res2= round(model2$`F value`[1],2)
print(res2)

#c. 0.6352, 채택택
res3=round(model2$`Pr(>F)`[[1]],4)
print(res3)

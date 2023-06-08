# Set working directory
setwd('C:/Users/Administrator/Downloads/Secret PART 01_작업형03_예상문제/data')

#-----------------------------------

#Q1.
exam1 = read.csv("Rabbit_Five.csv")

MDL = exam1$BP_change[exam1$Treatment == 'MDL']
Control = exam1$BP_change[exam1$Treatment == 'Control']

model1 = t.test(MDL, Control, paired = T)

#a. -4.68
res1=round(model1$estimate,2)
print(res1)

#b.-3.67
res2=round(model1$statistic,2)
print(res2)

#c. 0.001, 기각
res3 = round(model1$p.value,3)
print(res3)

#-----------------------------------

#Q2.
exam2 = read.csv('mtcars2.csv')

auto = exam2$hp[exam2$am == 0]
manual = exam2$hp[exam2$am == 1]

model2 = var.test(manual, auto, alternative = 'greater')

#a. 2.43
res1 = round(model2$estimate,2)
print(res1)

#b. 2.43
res2 = round(model2$statistic,2)
print(res2)

#3. 0.043, 기각
res3 = round(model2$p.value, 3)
print(res3)

#-----------------------------------

#Q3.
exam3 = read.csv('고객_등급리스트.csv', fileEncoding = 'euc-kr')

tb = table(exam3$Segment, exam3$Region)

model3 = chisq.test(tb)

#a. 15.74
res1 = round(model3$expected[2,3],2)
print(res1)

#b. 9
res2 = as.integer(model3$statistic)
print(res2)

#c. 0.148, 채택
res3 = round(model3$p.value, 3)
print(res3)

#-----------------------------------

#Q4.
exam4 = read.csv('Cars93.csv')

#a. 19.05
res1 = round(mean(exam4$Price, na.rm = TRUE),2)
print(res1)

#b.0.85
model4 = shapiro.test(exam4$Price)
res2 = round(model4$statistic,2)
print(res2)

#c. 0, 기각
round(model4$p.value,4) < 0.0001

#-----------------------------------

#Q5.
exam5 = read.csv('Cars93.csv')

model5 = cor.test(exam5$Rev_per_mile, exam5$Horsepower)

#a. -0.502
res1=round(model5$estimate,3)
print(res1)

#b.-5.54
res2 = round(model5$statistic, 2)
print(res2)

#c. 0, 기각
round(model5$p.value,4) < 0.0001

#-----------------------------------

#Q6.
exam6 = read.csv('USArrests.csv')

model6 = princomp(exam6)

#a. 0.995
res1 = round(model6$loadings[,1][2],3)
print(res1)

#b. -127.496
round(model6$scores[34,][1],3)

#c. 0.97, 1
summary(model6)

#-----------------------------------

#Q7.
exam7 = read.csv('Cars93.csv')

df = exam7[c('Rev_per_mile', 'Weight', 'Length','EngineSize', 'Price')]
df = na.omit(df)

model7 = lm(Price ~. , data = df)

#a. 0.3962
summary(model7)

#b. -2.6152
res2 = round(coef(model7)[1],4)
print(res2)

#c.
# 0.0023
# -0.021
res3_1=round(coef(model7)['Weight'],4)
print(res3_1)
res3_2=round(coef(model7)['Length'],4)
print(res3_2)

#8.
exam8 = read.csv('job.csv')

exam8$y = factor(exam8$y)
exam8$x2 = ifelse(exam8$x2 == 'M',1,0)
exam8$x2 = factor(exam8$x2)

#a. 0.323
model8 = glm(y ~., data = exam8)
res1 = round(coef(model8)[[1]],3)
print(res1)

#b.
odds_ratio = exp(coef(model8)[[3]])
print(odds_ratio)

#c.
res3 = predict(model8)[[9]]
print(res3)

# Model 4 softmax value Analysis

setwd("C:/Users/Administrator/Desktop/softmax_value/model4")

softmax1=read.table('confidence_Baseline_In.txt')
softmax1=softmax1$V1
sotmax2=read.table('confidence_Baseline_Out.txt')
softmax2 = sotmax2$V1

summary(softmax1)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4671  1.0000  1.0000  0.9933  1.0000  1.0000 

summary(softmax2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4599  0.9997  1.0000  0.9797  1.0000  1.0000 

# 1값이 지나치게 많아보인다. 1을 제거하고 분석을 실행한다.


# 1의 비율
sum(softmax1 == 1)/length(softmax1)# 0.8394893
sum(softmax2 == 1)/length(softmax2)# 0.5193432


# softmax value 1은 제거하고 분석하기
x1 = softmax1[softmax1 != 1] 
x2 = softmax2[softmax2 != 1] 

mean(x1);mean(x2)
sd(x1);sd(x2)

# Boxplot
boxplot(x1, x2, names = c('In','Out'), main='Boxplot of In dist & Out dist')

par(mfrow = c(1,2))
hist(x1, col=adjustcolor("red",alpha=0.5), freq=F, ylim = c(0,20),breaks = 'scott')
hist(x2, col=adjustcolor("steelblue",alpha=0.5),freq=F, breaks = 'scott')
par(mfrow = c(1,1))

# 정규성 검정

shapiro.test(x1) # p-value < 2.2e-16
shapiro.test(x2) # p-value < 2.2e-16
# 정규성을 만족하지 않는 것으로 보인다. 

qqnorm(x1, main = 'In Distribution')
abline(a=0,b=1, col='red', lwd = 2)

qqnorm(x2, main = 'Out of Distribution')
abline(a=0,b=1, col='red',lwd=2)

qqplot(x1, x2, main = 'qqplot of In dist & Out dist')
abline(a = 0, b= 1, col='red')
# 육안으로 보았을 때 두 분포가 비슷해 보인다.

# Kolmogorov-Smirnov
ks.test(x1,x2)
# D = 0.039737, p-value = 0.4086
# H0:  The two samples come from the same population.
# H1:  The two samples come from the different population.
# p-value < 0.05, H0를 채택한다.

# 둥분산성 검정
var.test(x1,x2)
# p-value = 0.2475
# 두 분산이 서로 같다고 볼 수 있다.
var(x1) # 0.00920107
var(x2) # 0.009909139


# 정규성 불만족
# 등분산성 만족
# x1과 x2값 사이에 유의성 검정을 하기 위해서 비모수검정 
wilcox.test(x1, x2)
# W = 615674, p-value = 0.6206


# What if t.test를 실행한다면?
t.test(x1,x2)
# p-value = 0.8867

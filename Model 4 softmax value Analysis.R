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

# 1���� ����ġ�� ���ƺ��δ�. 1�� �����ϰ� �м��� �����Ѵ�.


# 1�� ����
sum(softmax1 == 1)/length(softmax1)# 0.8394893
sum(softmax2 == 1)/length(softmax2)# 0.5193432


# softmax value 1�� �����ϰ� �м��ϱ�
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

# ���Լ� ����

shapiro.test(x1) # p-value < 2.2e-16
shapiro.test(x2) # p-value < 2.2e-16
# ���Լ��� �������� �ʴ� ������ ���δ�. 

qqnorm(x1, main = 'In Distribution')
abline(a=0,b=1, col='red', lwd = 2)

qqnorm(x2, main = 'Out of Distribution')
abline(a=0,b=1, col='red',lwd=2)

qqplot(x1, x2, main = 'qqplot of In dist & Out dist')
abline(a = 0, b= 1, col='red')
# �������� ������ �� �� ������ ����� ���δ�.

# Kolmogorov-Smirnov
ks.test(x1,x2)
# D = 0.039737, p-value = 0.4086
# H0:  The two samples come from the same population.
# H1:  The two samples come from the different population.
# p-value < 0.05, H0�� ä���Ѵ�.

# �պл꼺 ����
var.test(x1,x2)
# p-value = 0.2475
# �� �л��� ���� ���ٰ� �� �� �ִ�.
var(x1) # 0.00920107
var(x2) # 0.009909139


# ���Լ� �Ҹ���
# ��л꼺 ����
# x1�� x2�� ���̿� ���Ǽ� ������ �ϱ� ���ؼ� �������� 
wilcox.test(x1, x2)
# W = 615674, p-value = 0.6206


# What if t.test�� �����Ѵٸ�?
t.test(x1,x2)
# p-value = 0.8867
setwd('C:/Users/Administrator/Desktop/R_Engineer_Big_Data_Analysis/kaggle')

#Q1.
library(dplyr)
df = read.csv('titanic/train.csv')
df%>%mutate(Q1=fivenum(Fare)[2],Q3=fivenum(Fare)[4],IQR=Q3-Q1)%>%
  filter((Fare < Q1 -1.5*IQR) | (Fare < Q3 +1.5*IQR))%>%
  nrow()

#Q2.
df = read.csv('basic1.csv')
df%>%
  filter((age*10)%%10!=0) %>% mutate(pre1=ceiling(age),pre2=floor(age),pre3=trunc(age)) 

#Q3.
df = read.csv('basic1.csv')

apply(is.na(df),2,sum)/nrow(df)

df1<-df%>%
  group_by(city)%>%
  mutate(pre_f1 = ifelse(is.na(f1),median(df$f1, na.rm = TRUE),f1))
mean(df1$pre_f1)

#Q5.
df = read.csv('basic1.csv')
df1 = df%>%
  filter(f4 %in% c("ENFJ", "INFP"))%>%
  group_by(f4)%>%
  summarise(value = sd(f1, na.rm = TRUE))
result = abs(df1[1,2]-df[2,2])[[1]]
print(result)

#Q6.
df = read.csv('basic1.csv')
df1<-df%>%
  filter(!is.na(f1))%>%
  group_by(city, f2)%>%
  summarise(value = sum(f1))%>%
  filter(city == '경기' & f2 == 0)%>%
  subset(select = value)
result=df1[[1]]
print(result)

#Q7.
df = read.csv('basic1.csv')
df%>%
  mutate(pre_f4 = ifelse(f4 == 'ESFJ', 'ISFJ', f4))%>%
  filter(city == '경기' & pre_f4 == 'ISFJ')%>%
  summarise(value = max(age))

#Q8.
library(zoo)
df = read.csv('basic1.csv')
df %>% filter(f2==1) %>% 
  mutate(f1_cs=cumsum(ifelse(is.na(f1),0,f1))+f1*0) %>% mutate(f1_cs2=na.locf(f1_cs,fromLast=T)) %>% summarise(value=mean(f1_cs2))

#Q9.
df = read.csv('basic1.csv')

f5_scale = scale(df$f5)
median(f5_scale, na.rm=T)

#Q11.
df = read.csv('basic1.csv')
mm = (df$f5 - min(df$f5)) / (max(df$f5) - min(df$f5))
df$mm = mm
result = sum(quantile(mm, 0.05),quantile(mm, 0.95))
print(result)

#Q12.
df = read.csv('covid-vaccination-vs-death_ratio.csv')

df1=df %>% group_by(country) %>% summarise(value=max(ratio)) %>% filter(value<=100) %>% arrange(-value) %>% select(value)
result=round(mean(head(df1,10)$value)-mean(tail(df1,10)$value),1)
print(result)

#Q13.
df = read.csv("winequality-red.csv")

df%>% 
  cor%>%
  data.frame%>%
  select(quality)%>% 
  arrange(-quality)%>%slice(-1)%>%
  summarise(MAX = max(quality), MIN = min(quality))%>%
  sum()%>%
  round(2)

#Q14.
df = read.csv('basic1.csv')
result=df%>%
  group_by(city, f4)%>%
  summarise(value = mean(f5))%>%
  arrange(-value)%>%
  data.frame()%>%
  slice(1:7)%>%
  summarise(value = sum(value))%>%
  round(2)
result = result[[1]]
print(result)

#Q15.
df = read.csv('basic1.csv')
result=df%>%
  arrange(-age)%>%
  slice(1:20)%>%
  mutate(pre_f1 = ifelse(is.na(f1), median(f1, na.rm = TRUE),f1))%>%
  filter(f4 == 'ISFJ' & f5 >= 20)%>%
  summarise(value = mean(pre_f1))
result = result[[1]]
print(result)

#Q16.
df = read.csv('basic1.csv')

result=df%>%
  filter(f2 == 0)%>%
  arrange(age)%>%
  slice(1:20)%>%
  mutate(pre_f1 = ifelse(is.na(f1), median(f1, na.rm = TRUE),f1))%>% summarise(be_var=var(f1,na.rm=T),af_var=var(pre_f1))%>%
  apply(1,diff)%>%
  abs()%>%
  round(2)

print(result)  

#Q17.
df = read.csv('basic2.csv')
library(lubridate)

df%>%
  mutate(year = year(Date), month = month(Date))%>%
  filter(year == 2022 & month == 5)%>%
  summarise(value = median(Sales))

#Q18.
df = read.csv('basic2.csv')

df1=df%>%
  mutate(year = year(Date), month = month(Date), wday = wday(Date, label=T))%>%
  filter(year == 2022, month == 5)%>%
  data.frame()

weekend<-df1%>%
  filter(wday %in% c('토','일'))%>%
  summarise(value = mean(Sales))
weekday=df1%>%
  filter(!wday %in% c('토','일'))%>%
  summarise(value = mean(Sales))

result = abs(weekend - weekday)[[1]]
print(result)

#Q19.
df = read.csv('basic2.csv')
df1=df %>% mutate(year=year(Date),month=month(Date),pre_sales=ifelse(Events==1,Sales*0.8,Sales))
max_2022=df1 %>% filter(year==2022) %>% group_by(month) %>% summarise(value=sum(pre_sales)) %>% max
max_2023=df1 %>% filter(year==2023) %>% group_by(month) %>% summarise(value=sum(pre_sales)) %>% max
round(abs(max_2022-max_2023))

#Q20.
df1 = read.csv('basic1.csv')
df2 = read.csv('basic3.csv')

df1%>%
  inner_join(df2, by = 'f4')%>%
  filter(r2 != '')%>%
  head(20)%>%
  summarise(value = sum(f2))


#Q21.
df=read.csv('basic1.csv')
head(df)

result<-df%>%
  filter((age*10)%%10==0 & age>0)%>%
  mutate(group = cut(age,3))%>%
  group_by(group)%>%
  summarise(med = median(age))%>%
  summarise(value = sum(med))

result = result[[1]]
print(result)

#Q22.
library(readr)
df = read_csv('basic2.csv')

df%>% 
  mutate(week=cut(Date,breaks='week'))%>% 
  group_by(week)%>%
  summarise(value=sum(Sales))%>%
  summarise(MAX=max(value),MIN=min(value))%>%
  apply(1,diff)%>%
  abs

#Q24.
library(data.table)

df = read.csv('basic2.csv')
df %>% 
  mutate(lag=shift(PV,1))%>%
  filter(Events==1 & Sales<=1000000) %>% 
  summarise(value=sum(lag))


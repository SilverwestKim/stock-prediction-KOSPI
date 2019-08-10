rm(list=ls())
getwd()
setwd("C:/Users/User/OneDrive - inha.edu/����/��ǰ������/A")

library(readxl)
library(dplyr)
library(e1071)

data = read.csv("ndat.csv",header=TRUE)
str(data)
prepre = read.csv('prepre.csv',header = TRUE)
data = data[,-c(1,2,3,7)]
data= na.omit(data)
########################################(1)size(�ð��Ѿ�)�ʹ� ���� �ֵ� �������ֱ�
##���� 5% ~ 10% 
sum(data$chong <=50000)
idx = which(data$chong <=50000)
data = data[-idx,]

###########�ٸ� ������ (Y ���ͷ� ����) �شܰ� �������ִ� �ڵ� ���� �ֱ�
a = quantile(data$market, 0.005)
b = quantile(data$market, 0.995)
data$market = ifelse(data$market<a,a,ifelse(data$market>b,b,data$market))

# a = quantile(data$chong, 0.005)
# b = quantile(data$chong, 0.995)
# data$chong = ifelse(data$chong<a,a,ifelse(data$chong>b,b,data$chong))

a = quantile(data$Y, 0.005)
b = quantile(data$Y, 0.995)
data$Y = ifelse(data$Y<a,a,ifelse(data$Y>b,b,data$Y))

a = quantile(data$profit, 0.005)
b = quantile(data$profit, 0.995)
data$profit = ifelse(data$profit<a,a,ifelse(data$profit>b,b,data$profit))

a = quantile(data$tuja, 0.005)
b = quantile(data$tuja, 0.995)
data$tuja = ifelse(data$tuja<a,a,ifelse(data$tuja>b,b,data$tuja))

a = quantile(data$bm, 0.005)
b = quantile(data$bm, 0.995)
data$bm = ifelse(data$bm<a,a,ifelse(data$bm>b,b,data$bm))












################################################


#############################(2)mean ��ſ� median �ֱ�!!! 




data = data %>%group_by(gr) %>% mutate(sizerank = order(chong, decreasing = T))
data=data%>% group_by(gr) %>% mutate(tujarank = order(tuja, decreasing = T))
data = data%>% group_by(gr) %>% mutate(bmrank = order(bm, decreasing = T))
data = data%>% group_by(gr) %>% mutate(prorank = order(profit, decreasing = T))

data$totalrank = data$sizerank + data$tujarank + data$prorank + data$bmrank

#size�� �������� ���� ���̱� ������ �߾Ӱ� ���� ������ Ŭ���� 1��
data = data%>%group_by(gr)%>% mutate(s_class=ifelse(chong<median(chong),1,0))
#bm�� Ŭ���� ���� ���̱� ������ �߾Ӱ����� ũ�� Ŭ���� 1��
data = data%>%group_by(gr)%>% mutate(bm_class=ifelse(bm>median(bm),1,0))
#���ͼ��� Ŭ���� ���� ���̱� ������ �߾Ӱ����� ũ�� Ŭ���� 1��
data = data%>%group_by(gr)%>% mutate(p_class=ifelse(profit>median(profit),1,0))
#tuja�� ���� ���� ���� ���̱� ������ �߾Ӱ����� ������ Ŭ���� 1��
data = data%>%group_by(gr)%>% mutate(t_class=ifelse(tuja<median(tuja),1,0))

#total class ���� ���� ���� ���� �ͺ��� �������
data = data%>% group_by(gr) %>% mutate(total_class = ifelse(totalrank<median(totalrank),1,0))
data=data%>%group_by(gr)%>%mutate(real_lab = ifelse(Y>median(Y),1,0))
# write.csv(data,'ffff.csv',row.names=FALSE)
data = as.data.frame(data)
date = unique(data$gr)

########���ο� data frame �����
longshort = data.frame(date)


#size�� ���ؼ�
longshort$sizelong = NULL
longshort$sizeshort = NULL
for ( i in 1:245){
  id = which(longshort$date[i]==data$gr & data$s_class==1)
  me = mean(data[id,1])
  longshort$sizelong[i] = me
  
  jd = which(longshort$date[i]==data$gr & data$s_class==0)
  yo = mean(data[jd,1])
  longshort$sizeshort[i] = yo
}

#bm�� ���ؼ�
longshort$bmlong = NULL
longshort$bmshort = NULL
for ( i in 1:245){
  id = which(longshort$date[i]==data$gr & data$bm_class==1)
  me = mean(data[id,1])
  longshort$bmlong[i] = me
  
  jd = which(longshort$date[i]==data$gr & data$bm_class==0)
  yo = mean(data[jd,1])
  longshort$bmshort[i] = yo
}
#pro�� ���ؼ�
longshort$prolong = rep(-1,245)
longshort$proshort = rep(-1,245)
for ( i in 1:245){
  id = which(longshort$date[i]==data$gr & data$p_class==1)
  me = mean(data[id,1])
  longshort$prolong[i] = me
  
  jd = which(longshort$date[i]==data$gr & data$p_class==0)
  yo = mean(data[jd,1])
  longshort$proshort[i] = yo
}
#inv�� ���ؼ�
longshort$tlong = NULL
longshort$tshort = NULL
for ( i in 1:245){
  id = which(longshort$date[i]==data$gr & data$t_class==1)
  me = mean(data[id,1])
  longshort$tlong[i] = me
  
  jd = which(longshort$date[i]==data$gr & data$t_class==0)
  yo = mean(data[jd,1])
  longshort$tshort[i] = yo
}

#inv�� ���ؼ�
longshort$totallong = NULL
longshort$totalshort = NULL
for ( i in 1:245){
  id = which(longshort$date[i]==data$gr & data$total_class==1)
  me = mean(data[id,1])
  longshort$totallong[i] = me
  
  jd = which(longshort$date[i]==data$gr & data$total_class==0)
  yo = mean(data[jd,1])
  longshort$totalshort[i] = yo
}

str(longshort)

#total ���� �ռ��� Ȯ���غ���

longshort$total_ls = longshort$totallong - longshort$totalshort
summary(longshort$total_ls)
q=var(longshort$total_ls)
w=mean(longshort$total_ls)
qw = cbind(q,w)
#���� ����... �ٸ��͵� �ٽ� Ȯ���ϱ� 
#size
longshort$size_ls = longshort$sizelong-longshort$sizeshort
e=var(longshort$size_ls)
r=mean(longshort$size_ls)
er = cbind(e,r)
#bm
longshort$bm_ls = longshort$bmlong-longshort$bmshort
t=var(longshort$bm_ls)
y=mean(longshort$bm_ls)
ty = cbind(t,y)
#profit
longshort$profit_ls = longshort$prolong-longshort$proshort
u=var(longshort$profit_ls)
i=mean(longshort$profit_ls)
ui = cbind(u,i)
#investment
longshort$inv_ls = longshort$tlong-longshort$tshort
o=var(longshort$inv_ls)
p=mean(longshort$inv_ls)
op = cbind(o,p)

rst = rbind(qw,er,ty,ui,op)
rst = as.data.frame(rst)
colnames(rst) = c('var','avg')
rst$cate = c('total','size','b/m','profit','investment')
View(rst)
######����� �� �̻��ϱ� ������ hit ratio�� �غ��� 

longshort$shr=NULL
longshort$bmhr=NULL
longshort$phr=NULL
longshort$ihr = NULL
longshort$total_hr = NULL

for (i in 1:245){
  a = sum(longshort$date[i]==data$gr & data$real_lab == data$total_class)
  b = sum(longshort$date[i] == data$gr)
  longshort$total_hr[i]= a/b

  a = sum(longshort$date[i]==data$gr & data$real_lab == data$s_class)
  longshort$shr[i]= a/b
  
  a = sum(longshort$date[i]==data$gr & data$real_lab == data$bm_class)
  longshort$bmhr[i]= a/b
  
  a = sum(longshort$date[i]==data$gr & data$real_lab == data$t_class)
  longshort$ihr[i]= a/b
  
  a = sum(longshort$date[i]==data$gr & data$real_lab == data$p_class)
  longshort$phr[i]= a/b
}

write.csv(longshort, 'real_last.csv',row.names=FALSE)
data= as.data.frame(data)
str(data)

tt= longshort

ls = tt[,12:16]; hr = tt[,17:21]


#total
total.test = t.test(ls$total_ls,alternative = 'greater',mu=0)
total.test


#size
size.test = t.test(ls$size_ls,algernative = 'greater',mu=0)
simze.test

#bm
bm.test = t.test(ls$bm_ls,alternative = 'greater',mu=0)
bm.test

#profit
profit.test = t.test(ls$profit_ls, alternative='greater', mu=0)
profit.test

#inv
inv.test = t.test(ls$inv_ls,alternative = 'greater', mu = 0)
inv.test


##summary
name = c('total','size','bm','profit','inv')
t_stat = c(total.test$statistic,size.test$statistic,bm.test$statistic,profit.test$statistic,inv.test$statistic)
p_value = c(total.test$p.value,size.test$p.value,bm.test$p.value,profit.test$p.value,inv.test$p.value)

ls_summary = cbind(name,t_stat,p_value)
View(ls_summary)





#long short 0���� ū�� ����


#total
total.test = t.test(hr$total_hr,alternative = 'greater',mu=0.5)
total.test


#size
size.test = t.test(hr$shr,algernative = 'greater',mu=0.5)
size.test

#bm
bm.test = t.test(hr$bmhr,alternative = 'greater',mu=0.5)
bm.test

#profit
profit.test = t.test(hr$phr, alternative='greater', mu=0.5)
profit.test

#inv
inv.test = t.test(hr$ihr,alternative = 'greater', mu = 0.5)
inv.test


##summary
name = c('total','size','bm','profit','inv')
t_stat = c(total.test$statistic,size.test$statistic,bm.test$statistic,profit.test$statistic,inv.test$statistic)
p_value = c(total.test$p.value,size.test$p.value,bm.test$p.value,profit.test$p.value,inv.test$p.value)

hr_summary = cbind(name,t_stat,p_value)
View(hr_summary)






















nrow(data)
data= as.data.frame(data)
data$real_lab = as.factor(data$real_lab)
tmp = data
tmp = tmp[,c(2,3,4,5,8,19)]
which(data$gr == 20141) # 2014����� �����ϴ� �Ϳ� ����
# train = tmp[85000:88000,]
# test = tmp[88001:88646,]

########model train & test

###svm
svm.model<- svm(real_lab~.,data=train, gamma=1, cost=5)
svm.predict = predict(svm.model, newdata = test)

addmargins(table(test$real_lab,svm.predict))

tune.svm<- tune(svm,real_lab~., data=train,
                kernel="radial", ranges =list(gamma=c(0.01,0.1,1,5,10),
                                              cost=c(0.01,0.1,1,5,10)))


###logistic regression
#install.packages("smbinning")
library(smbinning)
logit.model = glm(real_lab~., data= train, family = binomial(link="logit") )

logit.predict.sc<- plogis(predict(logit.model, test))  # predicted scores
# or
logit.predict <- predict(logit.model, test, type="response") 


###neural network
#install.packages('nnet')
setwd("C:/Users/User/OneDrive - inha.edu/����/��ǰ������/A")
library(nnet); library(caret); library(ROCR)
dt = read.csv("finaldata.csv")
tmp = dt[,c(2,3,4,5,8,19)]
tmp$real_lab = as.factor(tmp$real_lab)
tr = createDataPartition(y = tmp$real_lab, p = 0.6, list = FALSE)
train = tmp[tr,]
test = tmp[-tr,]

nn.model = nnet(real_lab~., data = train, size = c(2,1),decay = 0.005)
nn.model2 = nnet(real_lab~., data=train, size = c(3,3),  decay = 0.005,maxit = 100)

library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

plot.nnet(nn.model)

library(NeuralNetTools)
garson(nn.model)

confusionMatrix(as.factor(as.character(predict(nn.model, newdata=test, type="class"))), test$real_lab)
  
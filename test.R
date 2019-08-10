rm(list=ls())
getwd()
setwd("C:/Users/User/OneDrive - inha.edu/한투/상품전략팀")

#install.packages("broom")
library(broom)
library(readxl)
library(dplyr)
library(e1071)

pre = read.csv('preprocess.csv')
dat = read_excel('quan.xlsx',sheet = 1)
company = read_excel('comname.xlsx')
dat = as.data.frame(dat)
length(unique(data2$Name))

company = as.data.frame(company)
str(company); 
company = unique(company$삼성전자)
length(company); head(company)

colnames(dat)=c('Name',rep(company, each = 3))

head(dat$Name)
#pre 데이터에서 투자 변수가 1998년 3월에 발표된 데이터부터 존재함
#전보다 보수성을 줄여서 3개월 정도 이후부터 영향을 준다고 하면, 
#따라서 98년 7월부터 데이터 생성하기

#먼저 수익성부터 쭉 뽑아서 벡터 만들기
#수익성은 다른 변수들에 비해서 다음 시점으로 뽑아야 한다는 사실 기억하기

#수익성 뽑아오기! 시점은 98년 8월이 시작이여 
dim(dat)
suik = NULL
k = seq(2,2314,3)
for (i in 8:258){
  suik = rbind(suik,t(dat[i,k]))
}
#suik vector 완성 ~

#market 뽑아오기 얘는 98년 7월이 시작이여
market = NULL
k = seq(4, 2314,3)
for (i in 7:257){
  market = rbind(market, t(dat[i,k]))
}

#시장수익률 완성~


#이제 시가총액!!
chong =NULL
k = seq(3, 2314,3)
for (i in 7:257){
  chong = rbind(chong, t(dat[i,k]))
}

#시가총액 chong도 완성~~

#이제 문제의 bm을 만들어야대..pre에 불러온 걸 보자
#annual이 섞여 있으므로 5의 배수꺼를 제거


id = seq(5,88665,5)
prepre = pre[-id,]
View(prepre)

idx = which(prepre$회계년 ==1997)
prepre = prepre[-idx,]
nn = order(prepre$주기)
prepre = prepre[nn,]
new=order(prepre$회계년)               
prepre = prepre[new,]

#write.csv(prepre,'prepre.csv',row.names=FALSE)

ndat=cbind(suik,market,chong)
View(ndat)
colnames(ndat) = c('Y','market','chong')
ndat = as.data.frame(ndat)

ndat$chongjabon = rep(0,nrow(ndat))
ndat$profit =rep(0,nrow(ndat))
ndat$tuja = rep(0,nrow(ndat))
#총자본, 투자, profit 전부 3배씩 뻥튀기 

idd=(which(prepre$회계년==2019 & prepre$주기 != '1Q'))
View(prepre[idd,])
prepre = prepre[-idd]


ndat$chongjabon[1:2313]=rep(prepre$chongjabon[1:771],times=3)
ndat$profit[1:2313]=rep(prepre$profit[1:771],times=3)
ndat$tuja[1:2313]=rep(prepre$tuja[1:771],times=3)


p=3
for (j in seq(1,67848, 771)){
  ndat$chongjabon[((771*(p-3))+1):(771*p)]=rep(prepre$chongjabon[j:(j+770)],times=3)
  ndat$profit[((771*(p-3))+1):(771*p)]=rep(prepre$profit[j:(j+770)],times=3)
  ndat$tuja[((771*(p-3))+1):(771*p)]=rep(prepre$tuja[j:(j+770)],times=3)
  p = p+3
}

ndd=ndat[-c(191980:193521),]

dim(na.omit(ndd))
dim(ndd)
# asdf= na.omit(ndd)
# 
# asdf$bm= asdf$chongjabon/asdf$market
# asdf = asdf[,-3]

monvec = rep(1:249,each = 771)
ndd$monvec = monvec

avg =tapply(ndd$Y,ndd$monvec,mean,na.rm=TRUE)
avg = as.vector(avg)
av = rep(avg, each = 771)
ndd$avg = av

ndd$lab = ifelse(ndd$Y <= ndd$avg, 0,ifelse(ndd$Y >= ndd$avg,1,NA))

ndd = ndd[,-c(1,3,4,7)]
nddd = na.omit(ndd)

###########model fitting
n<-nrow(nddd)
sub<-sample(1:n,round(0.5*n))
train<-nddd[sub,]
test<-nddd[-sub,]
tt=test[1:100,]
svm.model.1<- svm(lab~.,data=train, gamma=1, cost=1)
#svm.predict.2<-predict(svm.model.1,newdata=test)
svm.predict.2<-predict(svm.model.1,newdata=tt)
rst = as.data.frame(addmargins(table(tt$lab,svm.predict.2)))
View(rst)

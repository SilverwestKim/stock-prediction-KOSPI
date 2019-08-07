rm(list=ls())
setwd("C:/Users/User/Desktop")
dir()
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages('plotly')
#install.packages('dygraphs')
library(readxl)
library(ggplot2)
library(gridExtra)
library(plotly)
library(dygraphs)
library(xts)

data = read_excel('fund.xlsx')
View(data)

str(data)
data = as.data.frame(data)


dim(data)
data = data[78:1,]

data$bungi = 0

x = rep(1:26,each = 3)
x = c(1,x)
length(x)
x = x[1:78]
data$bungi=x


#투자계약, 실물, 혼합자산은 존재하는 자료가 아예 없거나
#너무 부족하므로 데이터에서 제거한다. 


dim(data)
new = matrix(0,26,22)
new = data.frame(new)
colnames(new) = colnames(data)
for (i in 2:22){
  new[,i] = tapply(data[,i],data$bungi,mean)
}
new$기준일자=1:26
new = new[,-22]
colnames(new)[1] = "분기"

#시작일 지정
start_date <- as.Date("2013-01-01")
#종료일 지정
end_date <- as.Date("2019-05-31")
date_set2 <-seq(as.Date(start_date), as.Date(end_date), by = "quarter")
View(date_set2)
new$분기 = date_set2

eco = xts(new$주식, order.by = as.Date(new$분기))
ecoa = xts(new$주식1, order.by = as.Date(new$분기))
e2 = cbind(eco, ecoa)
colnames(e2) = c('공모 주식', '사모 주식')
dygraph(e2) %>%dyRangeSelector()
















################################################################
a = qplot(new$분기, new[,2])
b = qplot(new$분기, new[,3])
c = qplot(new$분기, new[,4]) 
d = qplot(new$분기, new[,5]) 

grid.arrange(a,b,c,d, nrow=2, ncol=2)


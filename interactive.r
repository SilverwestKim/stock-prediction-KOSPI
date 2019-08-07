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
library(RColorBrewer)

data = read_excel('fund.xlsx')
#View(data)

str(data)
data = as.data.frame(data)


dim(data)



eco = xts(data$주식, order.by = data$기준일자)
ecoa = xts(data$주식1, order.by = data$기준일자)
ecob = xts(data$at, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='주식형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$채권, order.by = data$기준일자)
ecoa = xts(data$채권1, order.by = data$기준일자)
ecob = xts(data$bt, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='채권형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$혼합주식, order.by = data$기준일자)
ecoa = xts(data$혼합주식1, order.by = data$기준일자)
ecob = xts(data$ct, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='혼합주식형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$혼합채권, order.by = data$기준일자)
ecoa = xts(data$혼합채권1, order.by = data$기준일자)
ecob = xts(data$dt, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='혼합채권형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$재간접, order.by = data$기준일자)
ecoa = xts(data$재간접1, order.by = data$기준일자)
ecob = xts(data$et, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='재간접형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$파생형, order.by = data$기준일자)
ecoa = xts(data$파생형1, order.by = data$기준일자)
ecob = xts(data$ft, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='파생형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$부동산, order.by = data$기준일자)
ecoa = xts(data$부동산1, order.by = data$기준일자)
ecob = xts(data$gt, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='부동산형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$특별자산, order.by = data$기준일자)
ecoa = xts(data$특별자산1, order.by = data$기준일자)
ecob = xts(data$ht, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='특별자산형 [단위 : 백억 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


data$합계 = data$합계/100
data$합계1 = data$합계1/100
data$total = data$total/100

eco = xts(data$합계, order.by = data$기준일자)
ecoa = xts(data$합계1, order.by = data$기준일자)
ecob = xts(data$total, order.by = data$기준일자)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('공모', '사모', '합계')


dygraph(e2,main='합계  [단위 : 조 원]')%>% dyRangeSelector(height=20)%>% dySeries("공모",color = 'red',strokeWidth = 3)%>% dySeries('사모',color='blue',strokeWidth = 3)  %>%dySeries('합계',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

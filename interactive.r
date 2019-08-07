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



eco = xts(data$�ֽ�, order.by = data$��������)
ecoa = xts(data$�ֽ�1, order.by = data$��������)
ecob = xts(data$at, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='�ֽ��� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$ä��, order.by = data$��������)
ecoa = xts(data$ä��1, order.by = data$��������)
ecob = xts(data$bt, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='ä���� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$ȥ���ֽ�, order.by = data$��������)
ecoa = xts(data$ȥ���ֽ�1, order.by = data$��������)
ecob = xts(data$ct, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='ȥ���ֽ��� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$ȥ��ä��, order.by = data$��������)
ecoa = xts(data$ȥ��ä��1, order.by = data$��������)
ecob = xts(data$dt, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='ȥ��ä���� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$�簣��, order.by = data$��������)
ecoa = xts(data$�簣��1, order.by = data$��������)
ecob = xts(data$et, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='�簣���� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$�Ļ���, order.by = data$��������)
ecoa = xts(data$�Ļ���1, order.by = data$��������)
ecob = xts(data$ft, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='�Ļ��� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

eco = xts(data$�ε���, order.by = data$��������)
ecoa = xts(data$�ε���1, order.by = data$��������)
ecob = xts(data$gt, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='�ε����� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


eco = xts(data$Ư���ڻ�, order.by = data$��������)
ecoa = xts(data$Ư���ڻ�1, order.by = data$��������)
ecob = xts(data$ht, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='Ư���ڻ��� [���� : ��� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")


data$�հ� = data$�հ�/100
data$�հ�1 = data$�հ�1/100
data$total = data$total/100

eco = xts(data$�հ�, order.by = data$��������)
ecoa = xts(data$�հ�1, order.by = data$��������)
ecob = xts(data$total, order.by = data$��������)
e2 = cbind(eco, ecoa, ecob)
colnames(e2) = c('����', '���', '�հ�')


dygraph(e2,main='�հ�  [���� : �� ��]')%>% dyRangeSelector(height=20)%>% dySeries("����",color = 'red',strokeWidth = 3)%>% dySeries('���',color='blue',strokeWidth = 3)  %>%dySeries('�հ�',fillGraph = TRUE,color = 'green')%>%dyOptions(fillAlpha = 0.1,maxNumberWidth = 100)%>%dyLegend(show='always',width = 800) %>%dyHighlight(highlightCircleSize = 7, highlightSeriesBackgroundAlpha = 0.75)%>%dyCSS("dygraph.css")

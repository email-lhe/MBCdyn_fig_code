library(car)
library(carData)
library(tidyverse)
library(dplyr)
library(agricolae)
library(ggplot2)
library(gridExtra)
#read data
Z<-read.table("MBC_normal.csv", header=T, sep=",")

value<-Z[,5]

qqPlot(lm(treatment~value,data=Z),simulate=TRUE, main="Q-Q plot",labels=FALSE)
######change the number of volume
m<-Z[,5]
m
shapiro.test(m)
########################
max_1<-data.frame(Z[,2])
mean_1<-data.frame(Z[,3])
log_1<-data.frame(Z[,4])
sqrt_1<-data.frame(Z[,5])
normal_max<-data.frame(Z[,6])
normal_mean<-data.frame(Z[,7])
normal_log<-data.frame(Z[,8])
normal_sqrt<-data.frame(Z[,9])
max_2<-data.frame(Z[,11])
names(max_1)<-'max'
names(mean_1)<-'mean'
names(log_1)<-'log'
names(sqrt_1)<-'sqrt'
names(normal_max)<-'normal_max'
names(normal_mean)<-'normal_mean'
names(normal_log)<-'normal_log'
names(normal_sqrt)<-'normal_sqrt'
names(max_2)<-'max_1'

###############
p1<- ggplot(data=max_2 ,aes(max_1,..density..))+
  geom_histogram(color = 'white', fill = '#7080D7',binwidth = 0.15)+
  geom_line(stat = 'density',size=0.8,color = '#FF9200')+
  ylab(label = 'Frequency')+
  xlab(expression(MBCi[max]))+
  theme(panel.background = element_rect(color='black',fill = 'transparent'),
        panel.grid.major.y = element_line(colour = "gray",linetype = 'dashed'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.title.y=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.text.x=element_text(colour='black',size=16,face = NULL,family="serif"),
        axis.text.y=element_text(colour='black',size=16,face = NULL,family="serif"),
        legend.position = 'none')
p1


p2<- ggplot(data=mean_1 ,aes(mean,..density..))+
  geom_histogram(color = 'white', fill = '#7080D7',binwidth = 0.2)+
  geom_line(stat = 'density',color = '#FF9200',size=0.8)+
  ylab(label = 'Frequency')+
  xlab(expression(MBCi[mean]))+
  theme(panel.background = element_rect(color='black',fill = 'transparent'),
        panel.grid.major.y = element_line(colour = "gray",linetype = 'dashed'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.title.y=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.text.x=element_text(colour='black',size=16,face = NULL,family="serif"),
        axis.text.y=element_text(colour='black',size=16,face = NULL,family="serif"),
        legend.position = 'none')
p2
p3<- ggplot(data=log_1 ,aes(log,..density..))+
  geom_histogram(color = 'white', fill = '#7080D7',binwidth = 0.08)+
  geom_line(stat = 'density',color = '#FF9200',size=0.8)+
  ylab(label = 'Frequency')+
  xlab(expression(MBCi[log]))+
  theme(panel.background = element_rect(color='black',fill = 'transparent'),
        panel.grid.major.y = element_line(colour = "gray",linetype = 'dashed'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.title.y=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.text.x=element_text(colour='black',size=16,face = NULL,family="serif"),
        axis.text.y=element_text(colour='black',size=16,face = NULL,family="serif"),
        legend.position = 'none')
p3
p4<- ggplot(data=sqrt_1 ,aes(sqrt,..density..))+
  geom_histogram(color = 'white', fill = '#7080D7',binwidth = 0.08)+
  geom_line(stat = 'density',size=0.8,color = '#FF9200')+
  ylab(label = 'Frequency')+
  xlab(expression(MBCi[sqrt]))+
  theme(panel.background = element_rect(color='black',fill = 'transparent'),
        panel.grid.major.y = element_line(colour = "gray",linetype = 'dashed'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.title.y=element_text(colour='black', size=18,face = NULL,family="serif"),
        axis.text.x=element_text(colour='black',size=16,face = NULL,family="serif"),
        axis.text.y=element_text(colour='black',size=16,face = NULL,family="serif"),
        legend.position = 'none')
p4

p<-arrangeGrob(p3,p2,p1,p4,ncol=2,nrow=2)
p
ggsave("Fig_S1.tiff",p,width = 6,height = 6)

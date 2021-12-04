library(dplyr)
library(tidyr)
library(ggplot2)
library(Metrics)
library(gridExtra)
#install.packages("performance")
#library(performance)
#install.packages("glmnet")
#library(glmnet)
#library(caret)
library(corrplot)



#load the por (d2)and portargues(d2)datasets

d2=read.csv('C:/Users/myada/OneDrive/Desktop/ryreson/CIND820/student-por.csv',sep=";",header=TRUE)


#por dataset (d2)
d2=subset(d2,select = -c(G1,G2))
d2=select_if(d2, is.numeric)
str(d2)
summary(d2)
head(d2)
ggplot(d2, aes(G3)) + geom_histogram(stat = "count")+ggtitle("Distribution of final grades in por course")
corrplot(cor(d2))

grid.arrange(
  ggplot(d2, aes(age)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(Medu)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(Fedu)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(traveltime)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(studytime)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(failures)) + geom_histogram(stat = "count"), 
  ggplot(d2, aes(famrel)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(freetime)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(goout)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(Dalc)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(Walc)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(health)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(absences)) + geom_histogram(stat = "count"),
  ggplot(d2, aes(G3)) + geom_histogram(stat = "count"),
  ncol=4)



agepor<-ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(age))) +
  ggtitle("age ")
agepor

medupor <- ggplot(d2,aes(x=G3)) +
  geom_density(aes(color=as.factor(Medu) )) +
  ggtitle("mother education")
medupor


fedupor <- ggplot(d2,aes(x=G3)) +
  geom_density(aes(color=as.factor(Fedu) )) +
  ggtitle("father education")
fedupor

traveltimepor <- ggplot(d2, aes(x=G3)) +
  geom_density(aes(color=as.factor(traveltime))) +
  ggtitle("traveltime")
traveltimepor

studypor<-ggplot(d2,aes(x = as.factor(studytime),y=G3,fill=studytime))+
  geom_boxplot(show.legend = F)+
  labs(x="Study Time",y="Final Score (G3)")
studypor

fapor<-ggplot(d2,aes(x = as.factor(failures),y=G3,fill=failures))+
  geom_boxplot(show.legend = F)+
  labs(x="failures",y="Final Score (G3)")
fapor

famerlpor <- ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(famrel))) +
  ggtitle("family relationships")
famerlpor

freetime <- ggplot(d2,aes(x=G3)) +
  geom_bar(aes(color=as.factor(freetime ) )) +
  ggtitle("freetime ")
freetime

gopor <- ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(goout))) +
  ggtitle("goout")
gopor

dalcpor <- ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(Dalc))) +
  ggtitle("Daily alcohol consumption ")
dalcpor

Walcpor <- ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(Walc))) +
  ggtitle("Weekend alcohol consumption ")
Walcpor

healthpor <- ggplot(d2, aes(x=G3)) +
  geom_bar(aes(color=as.factor(health))) +
  ggtitle("health ")
healthpor

porabsences<- ggplot(d2,aes(x = absences,y=G3,color=absences))+
  geom_boxplot()+coord_flip()+
  labs(x="absences",y="Final Score(G3)in por")
porabsences

grid.arrange(agepor,medupor,fedupor,traveltimepor,studypor,fapor,famerlpor,freetime,gopor,dalcpor,Walcpor,healthpor,porabsences,ncol=4)

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



#load the math (d1)and portargues(d2)datasets

d1=read.csv('C:/Users/myada/OneDrive/Desktop/ryreson/CIND820/student-mat.csv',sep=";",header=TRUE)
#math dataset (d1)
d1=subset(d1,select = -c(G1,G2))
d1=select_if(d1, is.numeric)
str(d1)
summary(d1)
head(d1)
ggplot(d1, aes(G3)) + geom_histogram(stat = "count")+ggtitle("Distribution of final grades in Math course")
corrplot(cor(d1))

grid.arrange(
  ggplot(d1, aes(age)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(Medu)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(Fedu)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(traveltime)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(studytime)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(failures)) + geom_histogram(stat = "count"), 
  ggplot(d1, aes(famrel)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(freetime)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(goout)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(Dalc)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(Walc)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(health)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(absences)) + geom_histogram(stat = "count"),
  ggplot(d1, aes(G3)) + geom_histogram(stat = "count"),
  ncol=4)



agemath<-ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(age))) +
  ggtitle("age ")
agemath

meduMath <- ggplot(d1,aes(x=G3)) +
  geom_density(aes(color=as.factor(Medu) )) +
  ggtitle("mother education")
meduMath


feduMath <- ggplot(d1,aes(x=G3)) +
  geom_density(aes(color=as.factor(Fedu) )) +
  ggtitle("father education")
feduMath

traveltimeMath <- ggplot(d1, aes(x=G3)) +
  geom_density(aes(color=as.factor(traveltime))) +
  ggtitle("traveltime")
traveltimeMath

studymath<-ggplot(d1,aes(x = as.factor(studytime),y=G3,fill=studytime))+
  geom_boxplot(show.legend = F)+
  labs(x="Study Time",y="Final Score (G3)")
studymath

famath<-ggplot(d1,aes(x = as.factor(failures),y=G3,fill=failures))+
  geom_boxplot(show.legend = F)+
  labs(x="failures",y="Final Score (G3)")
famath

famerlMath <- ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(famrel))) +
  ggtitle("family relationships")
famerlMath

freetime <- ggplot(d1,aes(x=G3)) +
  geom_bar(aes(color=as.factor(freetime ) )) +
  ggtitle("freetime ")
freetime

goMath <- ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(goout))) +
  ggtitle("goout")
goMath

dalcMath <- ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(Dalc))) +
  ggtitle("Daily alcohol consumption ")
dalcMath

WalcMath <- ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(Walc))) +
  ggtitle("Weekend alcohol consumption ")
WalcMath

healthMath <- ggplot(d1, aes(x=G3)) +
  geom_bar(aes(color=as.factor(health))) +
  ggtitle("health ")
healthMath

mathabsences<- ggplot(d1,aes(x = absences,y=G3,color=absences))+
  geom_boxplot()+coord_flip()+
  labs(x="absences",y="Final Score(G3)in math")
mathabsences

grid.arrange(agemath,meduMath,feduMath,traveltimeMath,studymath,famath,famerlMath,freetime,goMath,dalcMath,WalcMath,healthMath,mathabsences,ncol=4)

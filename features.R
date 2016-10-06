library(caret)
library(ROCR)
library(miscTools)
library(randomForest)
library(pROC)
library(RColorBrewer)
library(ggplot2)
library(e1071)
library(C50)
library(rpart) 
library(rpart.plot)
library(DAAG)
library(reshape2)

color.knn <- '#ee7d99'
color.cart <- '#fff68f'
color.randf <- '#3399ff'

data = read.csv('~/Desktop/OnlineNewsPopularity.csv')
data=data[!data$n_unique_tokens==701,]
datamod <- subset(data, select=-c(url,timedelta))
for (i in ncol(datamod)-1){datamod[,i]<-scale(datamod[,i],center=TRUE,scale=TRUE)}
datapop <- datamod
datapop$shares <- as.factor(ifelse(datapop$shares > 1400, 1, 0))

par(mfrow=c(3,4))
for(i in 1:length(datapop)){hist(datapop[,i], xlab=names(datapop)[i])}

datapop <- datapop[datapop[,4]<1,]
for(i in c(11,20,44,45,46,48,49,50,53))datapop <- datapop[datapop[,i]!=0,]
datapop <- datapop[, -c(19,21,23,25)]

datamod2 <- datapop[,-c(13:18, 27:33, 57,58)]

set.seed(100)
split<-sample(2,nrow(datamod2),replace=TRUE,prob=c(0.8,0.2))

datamod2.knn <- knn3(shares ~.,datamod2[split==1,])
datamod2.knn.pred <-predict( datamod2.knn,datamod2[split ==2,],type="class")
datamod2.knn.prob <- predict( datamod2.knn,datamod2[split ==2,],type="prob")
confusionMatrix(datamod2.knn.pred, datamod2[split==2,]$shares)

datamod2.cart<-rpart(shares ~.,datamod2[split==1,],method='class')
datamod2.cart.pred<-predict( datamod2.cart,datamod2[split==2,] ,type="class")
datamod2.cart.prob<-predict( datamod2.cart,datamod2[split==2,] ,type="prob")
confusionMatrix(datamod2.cart.pred, datamod2[split==2,]$shares)

datapop <- datamod
datapop$shares <- as.factor(ifelse(datapop$shares > 1400,1,0))
set.seed(100)
split<-sample(2,nrow(datapop),replace=TRUE,prob=c(0.8,0.2))
datapop.rf<-randomForest(shares ~.,datapop[split==1,],ntree=100,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
plot(datapop.rf)
varImpPlot(datapop.rf)


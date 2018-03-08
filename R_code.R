# 先安装这些包才能用library()函数载入
install.packages("test")
install.packages("gridExtra")
install.packages("imputeMissings")
install.packages("theme_set")
install.packages("waldtest")

library(waldtest)
# caret: 提供获取、使用、评估成百上千个机器学习模型及其拟合效果的系统交互界面
# 为机器学习提供了结构化的方法并且对一系列机器学习过程进行评估
library(caret)
library(ggpairs)
library(discretization)

# e1071: 各类计量经济和机器学习的延伸；我们通过naiveBayes()函数进行朴素贝叶斯判别
library(e1071)
# gridExtra: 绘图辅助功能，将不同的图形组合在一起成为图表
library(gridExtra) 
# lattice: 建立在核心绘图能力上的格子框架图形
library(lattice)
# imputeMissings: 填补缺失值
library(imputeMissings)
# RANN: 应用k邻近算法
library(RANN)
# corrplot: 相关矩阵的高级可视化
library(corrplot)
# nnet: 拟合单个潜层级的神经网络模型
library(nnet)
# car: 回归模型解释和可视化工具，其它附加功能； 其中包括some()和scatterplotMatrix()函数
library(car)
# gpairs: 广义散点图；对混合类别和连续变量产生散点图矩阵
library(gpairs)
# reshape2: 灵活重构和整合数据，主要有两个函数melt()和dcast()
library(reshape2)
# psych: 心理计量学方法和抽样调查分析，尤其是因子分析和项目反应模型；
# 我们会使用包中的describe()函数
library(psych)
# plyr: 可以将数据分割成更小的数据，然后对分割后的数据进行些操作，最后把操作的结果汇总
library(plyr)
library(ggplot2)
library (gridExtra)
redwine.dat<-read.table("/Users/apple/Desktop/img/winequality/WineQuanlity/winequality-red.csv", sep = ";", header=T)
str(redwine.dat)
summary(redwine.dat)
whitewine.dat<-read.table("/Users/apple/Desktop/img/winequality/WineQuanlity/winequality-white.csv", sep = ";", header=T)
WineTianBu<-impute(whitewine.dat,method="median/mode")
q1<-ggplot(aes(x=pH),
           data =  subset(whitewine.dat,type %in% c("fixed.acidity")))+
  geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('pH distribution for White wine')
q2<-ggplot(aes(x=free.sulfur.dioxide),
           data =  subset(whitewine.dat,type %in% c("W")))+a
  ggtitle('Free SO2 distribution for White wine')
q3<-ggplot(aes(x=total.sulfur.dioxide),
           data =  subset(whitewine.dat,type %in% c("W")))+
  geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Total SO2 distribution for White wine')
q4<-ggplot(aes(x=alcohol),
           data =  subset(whitewine.dat,type %in% c("W")))+
  geom_histogram(color =I('black'),fill = I('#099009'))+
  ggtitle('Alcohol distribution for White wine')

library(GGally)

ggpairs(whitewine.dat[sample.int(nrow(whitewine.dat),1000),])
whitewine.dat$citric.acid[which(whitewine.dat$citric.acid==0)]<-NA
summary(whitewine.dat)


train <-  redwine.dat [1:1000,]
test <-redwine.dat [1001:1599,]
train <-  whitewine.dat [1:2500,]
test <-whitewine.dat [2501:4898,]
model <- glm(quality ~.,family=binomial(),data=train)
summary(model)

library("nnet")

whitewine.dat$rating[5 >= whitewine.dat$quality ] = "Poor"
whitewine.dat$rating[5< whitewine.dat$quality & whitewine.dat$quality < 7] = "Good"
whitewine.dat$rating[7<= whitewine.dat$quality ] = "Great"
whitewine.dat$rating = as.factor(whitewine.dat$rating)
whitewine.dat$rating = relevel(whitewine.dat$rating, "Poor")
head(whitewine.dat)
table(whitewine.dat$rating)

set.seed(123)
samp <- sample(nrow(whitewine.dat), 0.6 * nrow(whitewine.dat))
train <- whitewine.dat[samp, ]
test <- whitewine.dat[-samp, ]

#alcohol +  volatile.acidity      Accuracy : 0.5474          
model_whitewine = multinom(rating ~ alcohol +volatile.acidity +chlorides , data = train)
model_whitewine
pred_whitewine = predict(model_whitewine, test)
confusion_ma = table(pred_whitewine,test$rating)
mean(as.character(test$rating) != as.character(pred_whitewine))
summary(model_whitewine)
confusionMatrix (confusion_ma)
confusionMatrix (confusion_ma) $byClass

F.test.cca (rating ~ . -quality, data = test)
?multinom
?train
ctrl <- trainControl(method="cv",number=10,repeats = 3)
model.white <- train(rating ~ alcohol +volatile.acidity +chlorides, data=train, trControl = ctrl, method="multinom")
model.white
plot(model.white)

drop1(model_whitewine, test="F")

step(model_whitewine,
     scope = list(upper=model_whitewine3),
     direction="both",
     test="Chisq",
     data=test)

line1 <- pred_whitewine
plot()

plot(week,x,col="red",pch=22,bg="yellow",xlim=c(0,6),ylim=c(0,30),lwd=2,xlab="WEEK",ylab="STUDENT",main="lesson",sub="class",col.main="green",font.main=2,asp=0,cex=1.2,type="b",lty=1)



#rating ~ alcohol                Accuracy : 0.5301          
model_whitewine1 = multinom(rating ~ alcohol , data = train)
pred_whitewine1 = predict(model_whitewine1, test)
summary(model_whitewine1)
confusion_ma1 = table(pred_whitewine1,test$rating)
confusionMatrix (confusion_ma1)

#   ccuracy : 0.5301 
model_whitewine2 = multinom(rating ~ alcohol +density +chlorides, data = train)
pred_whitewine2 = predict(model_whitewine2, test)
confusion_ma2 = table(pred_whitewine1,test$rating)
confusionMatrix (confusion_ma2)
pred_whitewine2
multinom(model_whitewine2)
weight <- information.gain(rating~., train)
print(weights)



#rating ~ . -quality   Accuracy : 0.5827
model_whitewine3 = multinom(rating ~ . -quality , data = train)
model_whitewine3
pred_whitewine3 = predict(model_whitewine3, test)
confusion_ma3 = table(pred_whitewine3,test$rating)
confusionMatrix (confusion_ma3)
ggplot (pred_whitewine3)

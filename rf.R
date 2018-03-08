install.packages("gridExtra")
install.packages("imputeMissings")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("GGally")
install.packages("rpart")
install.packages("nnet")
install.packages("caret")
install.packages("corrplot")
install.packages("kknn")
install.packages("kernlab")
install.packages("rpart.plot")
install.packages("scales")
install.packages("SparseM")
install.packages('e1071', dependencies=TRUE)
install.packages("ranger")

library(caret)
library(corrplot)
library(kknn)
library(randomForest)
library(kernlab)
library(ranger)


#read data
wine <- read.csv("winequality-white.csv", sep=";", dec = ",", stringsAsFactors=FALSE)
head(wine)
barplot(table(wine$quality))

#categorize quality to "good","normal" and "bad"
wine$rating[5 >= wine$quality ] = "Poor"
wine$rating[5< wine$quality & wine$quality < 7] = "Good"
wine$rating[7<= wine$quality ] = "Great"
wine$rating = as.factor(wine$rating)

wine$rating = relevel(wine$rating, "Poor")

#generate a training and a test set
set.seed(123)
samp <- sample(nrow(wine), 0.6 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

#build the model
library(randomForest)
model <- randomForest(rating ~ . - quality, data = train)
model

#test the model
pred <- predict(model, newdata = test)
#use confusion matrix
confusionMatrix(test$rating, pred,
                positive = "yes")

#use cross validation
ctrl <- trainControl(method="cv",number=10,repeats = 3)
model.white <- train(rating~., data=train, trControl = ctrl, method="ranger")
model.white
plot(model.white)





#draw the trees if you want
options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))
 
library(reprtree)

reprtree:::plot.getTree(model)

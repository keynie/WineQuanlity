install.packages("caret")
install.packages("class")
library(caret)
library(class)

#read csv files
white <- read_delim("~/Downloads/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
white <- winequality_white

#data cleaning if there are missing values
anyNA(white)

#categorize quality to "good","normal" and "bad"
white$rating <- ifelse(white$quality < 6, 'bad', 'good')
white$rating[white$quality == 6] <- 'normal'
white$rating <- as.factor(white$rating)

#generate a training and a test set
inTrain.white <- createDataPartition(white$rating, p = 0.6, list = F)
train.white <- white[inTrain.white,]
test.white <- white[-inTrain.white,]
trainData.white <- train.white[,(1:12)]
testData.white <- test.white[,(1:12)]

#Cross-Validation to find the optimal k value
ctrl <- trainControl(method="cv",number=10,repeats = 3)
model.white <- train(rating~., data=train.white, trControl = ctrl, method="knn")
model.white
plot(model.white)

#testing statistics for white wine with cross-validation
predicted.white <- knn(train = trainData.white, test = testData.white, cl = train.white$rating, k=5)
confusion.matrix.white <- table(test.white$rating, predicted.white)
print(confusion.matrix.white)
confusionMatrix(confusion.matrix.white)
confusionMatrix(confusion.matrix.white)$byClass
#White Wine: Accuracy=55%, Error=45%

#testing statistics for white wine without cross-validation (holdout accuracy)
predicted.white <- knn(train = trainData.white, test = testData.white, cl = train.white$rating)
confusion.matrix.white <- table(test.white$rating, predicted.white)
print(confusion.matrix.white)
confusionMatrix(confusion.matrix.white)
confusionMatrix(confusion.matrix.white)$byClass
#White Wine: Accuracy=64%, Error=36%

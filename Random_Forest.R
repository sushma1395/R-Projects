install.packages("randomForest")
library(randomForest)

#read_excel("C:/Users/gadep/OneDrive/Desktop/Copy of boston.xls")
#Loading the CSV file

library(readxl)
data <- read_excel("C:/Users/gadep/OneDrive/Desktop/Copy of boston.xls")
#View(data)

#data
head(data)
str(data)
summary(data)

set.seed(100)
train <- sample(nrow(data), 0.9*nrow(data), replace = FALSE)

TrainSet <- data[train,]
TestSet <- data[-train,]

summary(TrainSet)
summary(TestSet)

model1 <- randomForest::randomForest(MV ~ . , data = TrainSet , importance = TRUE)
model1

model2 <- randomForest::randomForest(MV ~ ., data = TrainSet, ntree = 400, mtry = 4, importance = TRUE)
model2

predTrain <- predict(model2, TrainSet, type = "class")
predTrain

table(predTrain,TrainSet$MV)

importance(model2)        
varImpPlot(model2) 


a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(MV ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, TrainSet, type = "class")
  a[i-2] = mean(predValid == TrainSet$MV)
}

a

plot(3:8,a)




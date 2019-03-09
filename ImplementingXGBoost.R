install.packages('xgboost')

library('xgboost')
data(agaricus.train, package ='xgboost')
data(agaricus.test,package='xgboost')

train <-agaricus.train
test <- agaricus.test
str(train)

dim(train$data)
str(train)

model<- xgboost(data = train$data, label = train$label, 
                 nrounds = 2, objective = "binary:logistic")

preds = predict(model,test$data)
err <- mean(as.numeric(preds>0.5) != test$label)
print(paste("test-error=",err))

cv.res <- xgb.cv(data=train$data, label = train$label, nfold = 5, nrounds = 2, objective ="binary:logistic")

bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max_depth= 2, eta = 1 , nthread = 2,
                nrounds = 2, objective = "binary:logistic")

pred <- predict(bstDense, test$data)
prediction <- as.numeric(pred>0.5)
print(head(prediction))
#Predicting the error value
err <- mean(as.numeric(pred> 0.5) != test$label)
print(paste("test-error=", err))



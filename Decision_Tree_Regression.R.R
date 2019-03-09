#Installing the required packages
library(rpart)
install.packages("tree")
library(tree)
library(MASS)
install.packages("rattle")
library(rattle)
install.packages("rpart.plot")
install.packages("rpart")
library(rpart)
library(rpart.plot)
#Loading the CSV file 
Energy_Eff_2012_data <- readxl::read_excel("C:/Users/gadep/Downloads/Energy Efficiency ENB2012_data.xlsx")

Energy_Eff_2012_data

data<- sample(2,nrow(Energy_Eff_2012_data),replace = TRUE,prob = c(0.66,0.34))
train_data<- Energy_Eff_2012_data[data == 1,]
test_data<- Energy_Eff_2012_data[data == 2,]

train_data
test_data

#Performing Linear regression for finding the p-value
linear_reg <- lm(Y1~X1+X2+X3+X4+X5+X6+X7+X8, data = train_data)
summary(linear_reg)

#Decision tree and prediction
#Y1 prediction
model_tree= rpart(Y1~ X1+X2+X3+X5+X6+X7+X8,train_data)
p= predict(model_tree,test_data)
p
#Comparison of the Predicted values
table(p,test_data[,2])
p
test_data[,2]


#Plotting the data
print(model_tree)
plot(model_tree)
text(model_tree, pretty=0, use.n = TRUE, cex= 0.75)
fancyRpartPlot(model_tree)

printcp(model_tree)
plotcp(model_tree)
#Pruning the tree
ptree<- prune(model_tree, cp= model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"])
install.packages('RColorBrewer')
library(RColorBrewer)
fancyRpartPlot(ptree, uniform=TRUE, main="Pruned Classification Tree")

#Y2 prediction
model_tree1= rpart(Y2~ X1+X2+X3+X5+X6+X7+X8,train_data)
p1= predict(model_tree1,test_data)

#Comparison of Predicted values
table(p,test_data[,2])
p
test_data[,2]

print(model_tree1)
plot(model_tree1)
text(model_tree1, pretty=0, use.n = TRUE, cex= 0.75)
fancyRpartPlot(model_tree1)

printcp(model_tree1)
plotcp(model_tree1)


ptree<- prune(model_tree1, cp= model_tree1$cptable[which.min(model_tree1$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE, main="Classification Tree")


library(readxl)
library(rJava)
library(e1071)
library(resample)
library(magrittr)
library(dplyr)
library(GGally)
library(gender)
library(readr)
library(ggplot2)
library(lattice)
library(caret)
setwd("F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/")
dataset <- read.csv("newsal2.csv", header = TRUE)
train <- dataset[1:14724,]
test <- dataset[14725:18405,]

# naive-bayes classification

model_naive_bayes <- naiveBayes(JobTitle ~ ., data = train)
predict <- predict(model_naive_bayes, newdata = test)
summary(model_naive_bayes)
class(model_naive_bayes)
summary(predict)
View(predict)
cm <- confusionMatrix(predict, test$JobTitle)
str(cm)
overall <- cm$overall*100
overall
model_naive_bayes
write.table(overall, "F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/result_nb.txt")

# knn classification

library(class)
library(knncat)
train_labels <- train$JobTitle
knn1 <- knn(train = train, test = test, cl = train_labels, k=1)

# svm classification

model.svm <- svm(JobTitle~BasePay+OvertimePay+OtherPay+Benefits+TotalPay+TotalPayBenefits, data = train)

summary(model.svm)

pred <- predict(model.svm, test)
options(scipen = 999)
summary(pred)
confmatrix <- confusionMatrix(pred, test$JobTitle)
str(confmatrix)
overall2 <- confmatrix$overall
overall2
write.table(overall2, "F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/result_svm.txt")

# random forest

library(randomForest)

model.randomforest <- randomForest(JobTitle~BasePay+OvertimePay+OtherPay+Benefits+TotalPay+TotalPayBenefits, data = train)

pr2 <- predict(model.randomforest, test)
options(scipen = 999)
summary(pr2)
confmatrix <- confusionMatrix(pr2, test$JobTitle)
str(confmatrix)
overall3 <- confmatrix$overall
overall3
write.table(overall3, "F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/result_rf.txt")

# decision tree

library(zoo)
library(sandwich)
library(party)

model.dtree <- ctree(JobTitle~BasePay+OvertimePay+OtherPay+Benefits+TotalPay+TotalPayBenefits, data = train)

pr3 <- predict(model.dtree, test)
options(scipen = 999)
summary(pr3)
confmatrix <- confusionMatrix(pr3, test$JobTitle)
str(confmatrix)
overall4 <- confmatrix$overall
overall4
write.table(overall4, "F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/result_dtree.txt")

plot(model.dtree)

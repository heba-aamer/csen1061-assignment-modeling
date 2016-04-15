# Libraries importing
# --------------------

library(ggplot2)
library(dplyr)
library(knitr)
library(stargazer)
library(MASS)
library(pracma)
library(tidyr)
library(lubridate)
library(XML)
library(pastecs)
library(corrplot)
library(rpart)
library(RWeka)
library(grid)
library(partykit)
library(randomForest)
library(e1071)
library(kernlab)
library(compare)
library(caret)
library(klaR)

# ----------------------------------------------------------

# Adding seed value to eliminate randomness
# -----------------------------------------

constant.seed <- 42

# -----------------------------------------------------------

# Reading the sonar data
# -----------------------

sonar.data <- read.csv("/home/mohammed/10th Semester/DataScience/Assignment/sonar.all-data", header = F)
names(sonar.data)
str(sonar.data)
summary(sonar.data)

# ------------------------------------------------------------

# Segment the data into (Training/Validation [80%])
# -------------------------------------------------
# and Testing [20%] partitions 
# ----------------------------
sonar.data.grouped <- sonar.data %>% group_by(V61)

#sonar.data.train <- sample_frac(sonar.data, 0.8)
#sonar.data.test <- setdiff(sonar.data, sonar.data.train)
#sonar.data.test <- sonar.data[-sonar.data.train,]

x <- srswor(as.integer(0.8*nrow(sonar.data)), nrow(sonar.data))
sonar.data.train <- sonar.data[which(x==1),]
sonar.data.test <- sonar.data[which(x==0),]

summary(sonar.data.train)
summary(sonar.data.test)

# ------------------------------------------------------------

# C4.5 Desicion Tree Model
# -----------------------

set.seed(constant.seed)
sonar.data.c45model <- J48(V61 ~ ., data = sonar.data)
table(sonar.data$V61, predict(sonar.data.c45model))
e <- evaluate_Weka_classifier(sonar.data.c45model, class = T)
e
summary(e)
e$details
e$string

# ------------------------------------------------------------

# 10-K Fold C4.5 Desicion Tree Model
# ---------------------------------

set.seed(constant.seed)
sonar.data.c45model.kfold <- J48(V61 ~ ., data = sonar.data.train, control = Weka_control(R = T, N = 10))
e1 <- evaluate_Weka_classifier(sonar.data.c45model.kfold, newdata = sonar.data.test,class = T)
e1

# ------------------------------------------------------------

# Forest Tree Model
# -----------------

set.seed(constant.seed)
#sonar.data.randomforest <- randomForest(V61 ~ ., data=sonar.data)
t.ctrl <- trainControl(method = "cv", number = 10)
sonar.data.randomforest <- train(V61 ~ ., data = sonar.data.train, method = "rf", trControl = t.ctrl)
summary(sonar.data.randomforest)
predictions.randomforest <- predict(sonar.data.randomforest, sonar.data.test, "raw")
confusionMatrix(predictions.randomforest, sonar.data.test$V61)

# ------------------------------------------------------------

# SVM Model
# ---------

set.seed(constant.seed)
#sonar.data.randomforest <- randomForest(V61 ~ ., data=sonar.data)
t.ctrl <- trainControl(method = "cv", number = 10)
sonar.data.svm.linear <- train(V61 ~ ., data = sonar.data.train, method = "svmLinear", trControl = t.ctrl)
predictions.svm.linear <- predict(sonar.data.svm.linear, sonar.data.test, "raw")
confusionMatrix(predictions.svm.linear, sonar.data.test$V61)


set.seed(constant.seed)
#sonar.data.randomforest <- randomForest(V61 ~ ., data=sonar.data)
t.ctrl <- trainControl(method = "cv", number = 10)
sonar.data.svm.radial <- train(V61 ~ ., data = sonar.data.train, method = "svmRadial", trControl = t.ctrl)
predictions.svm.radial <- predict(sonar.data.svm.radial, sonar.data.test, "raw")
confusionMatrix(predictions.svm.radial, sonar.data.test$V61)


# ------------------------------------------------------------

# Naive Bayes Model
# -----------------

set.seed(constant.seed)
t.ctrl <- trainControl(method = "cv", number = 10)
sonar.data.naivebayes <- train(V61 ~ ., data = sonar.data.train, method = "nb", trControl = t.ctrl)
predictions.naivebayes <- predict(sonar.data.naivebayes, sonar.data.test, "raw")
confusionMatrix(predictions.naivebayes, sonar.data.test$V61)

# ------------------------------------------------------------

# NN Model
# --------

set.seed(constant.seed)
t.ctrl <- trainControl(method = "cv", number = 10)
sonar.data.neuralnet <- train(V61 ~ ., data = sonar.data.train, method = "nnet", trControl = t.ctrl)
predictions.neuralnet <- predict(sonar.data.neuralnet, sonar.data.test, "raw")
confusionMatrix(predictions.neuralnet, sonar.data.test$V61)

# ------------------------------------------------------------

# Bagging Model
# -------------

set.seed(constant.seed)
sonar.data.bagging <- Bagging(V61 ~ ., data = sonar.data.train, control = Weka_control(W = list(J48, R = T, N = 10)))
e2 <- evaluate_Weka_classifier(sonar.data.bagging, newdata = sonar.data.test,class = T)
e2

# ------------------------------------------------------------

# Boosting Model
# --------------

set.seed(constant.seed)
sonar.data.adaboost <- AdaBoostM1(V61 ~ ., data = sonar.data.train, control = Weka_control(W = list(J48, R = T, N = 10)))
e3 <- evaluate_Weka_classifier(sonar.data.adaboost, newdata = sonar.data.test,class = T)
e3
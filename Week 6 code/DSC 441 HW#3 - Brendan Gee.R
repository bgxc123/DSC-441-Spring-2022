#Problem #1
df = read.csv("breast_cancer_updated.csv")
head(df)
df = df[-1]
summary(df)
df = na.omit(df)

#part a.)
train_control = trainControl(method = "cv", number = 10)
tree1 = train(Class ~., data =  df, method = "rpart", trControl = train_control)
tree1

#part b.)
library(rattle)
fancyRpartPlot(tree1$finalModel, caption = "")

#Problem #2
library(dplyr)
data("storms")
head(storms)
storms$category = as.factor(storms$category)
summary(storms)
#get rid of NAs
storms = na.omit(storms)
#part a.)
hypers = rpart.control(minsplit = 5, maxdepth = 2, minbucket = 3)
tree2 = train(category~., data = storms, control = hypers, trControl = train_control,
              method = "rpart1SE")
tree2

#part b.)
index = createDataPartition(y=storms$category, p=.7, list=FALSE)
train_set = storms[index,]
test_set = storms[-index,]

tree3 = train(category~., data = train_set, control = hypers, trControl = train_control,
              method = "rpart1SE")
pred_tree = predict(tree3, train_set)
cfm_train = confusionMatrix(train_set$category, pred_tree)
pred_tree = predict(tree3, test_set)
cfm_test = confusionMatrix(test_set$category, pred_tree)
cfm_train
cfm_test

#Problem #3
#a.)
index = createDataPartition(y=storms$category, p=.8, list=FALSE)
train_set = storms[index,]
test_set = storms[-index,]

#b.)
# Initialize cross validation
train_control = trainControl(method = "cv", number = 10)

# Tree 1
hypers = rpart.control(minsplit =  2, maxdepth = 1, minbucket = 2)
tree1 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree1, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree1, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree1$finalModel$frame)

# Form the table
comp_tbl <- data.frame("Nodes" = nodes, "TrainAccuracy" = a_train, "TestAccuracy" = a_test,
                       "MaxDepth" = 1, "Minsplit" = 2, "Minbucket" = 2)
comp_tbl
# Tree 2
hypers = rpart.control(minsplit =  4, maxdepth = 2, minbucket = 2)
tree2 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree2, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree2, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree2$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 2, 4, 4))
comp_tbl
# Tree 3
hypers = rpart.control(minsplit = 5, maxdepth = 2, minbucket = 3)
tree4 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree4, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree4, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree4$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 2, 5, 3))
comp_tbl
# Tree 4
hypers = rpart.control(minsplit = 10, maxdepth = 4, minbucket = 10)
tree7 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree7, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree7, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree7$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 10, 10))
comp_tbl
# Tree 5
hypers = rpart.control(minsplit = 25, maxdepth = 4, minbucket = 25)
tree8 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree8, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree8, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree8$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 4, 25, 25))
comp_tbl
# Tree 6
hypers = rpart.control(minsplit = 50, maxdepth = 5, minbucket = 50)
tree10 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree10, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree10, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree10$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 5, 50, 50))
comp_tbl
# Tree 7
hypers = rpart.control(minsplit = 100, maxdepth = 3, minbucket = 100)
tree11 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree11$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 3, 100, 100))

comp_tbl

# Tree 8
hypers = rpart.control(minsplit = 500, maxdepth = 15, minbucket = 500)
tree11 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree11$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 15, 500, 500))

comp_tbl

# Tree 9
hypers = rpart.control(minsplit = 1000, maxdepth = 20, minbucket = 1000)
tree11 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree11$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 20, 1000, 1000))

comp_tbl

# Tree 10
hypers = rpart.control(minsplit = 5000, maxdepth = 25, minbucket = 5000)
tree11 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix
cfm_test <- confusionMatrix(test_set$category, pred_tree)

# Get training accuracy
a_train <- cfm_train$overall[1]
# Get testing accuracy
a_test <- cfm_test$overall[1]
# Get number of nodes
nodes <- nrow(tree11$finalModel$frame)

# Add rows to the table - Make sure the order is correct
comp_tbl <- comp_tbl %>% rbind(list(nodes, a_train, a_test, 25, 5000, 5000))

comp_tbl

# Visualize with scatter plot
ggplot(comp_tbl, aes(x=Nodes)) + 
  geom_point(aes(y = TrainAccuracy), color = "red") + 
  geom_point(aes(y = TestAccuracy), color="blue") +
  ylab("Accuracy")

#c.)
#final model
hypers = rpart.control(minsplit = 50, maxdepth = 5, minbucket = 50)
tree11 <- train(category ~., data = train_set, control = hypers, trControl = train_control, method = "rpart1SE")

# Training Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, train_set)
# Confusion Matrix
cfm_train <- confusionMatrix(train_set$category, pred_tree)

# Test Set
# Evaluate the fit with a confusion matrix
pred_tree <- predict(tree11, test_set)
# Confusion Matrix / Accuracy Test
cfm_test <- confusionMatrix(test_set$category, pred_tree)
tree11
cfm_train
cfm_test

#Problem #4
df = read.csv("Bank_Modified.csv")
head(df)
df = df[-1]
df$approval = as.factor(df$approval)
summary(df)
df = na.omit(df)
#a.)
hypers = rpart.control(minsplit = 10, maxdepth = 20)
tree1 <- train(approval ~., data = df, method = "rpart1SE", control = hypers, 
               trControl = train_control)
tree1

#b.)
var_imp = varImp(tree1, scale = FALSE)
var_imp

#c.)
plot(var_imp)

#d.)
df_new = df[c(4,6,5,12,3,9,10)]
head(df_new)

hypers = rpart.control(minsplit = 10, maxdepth = 20)
tree2 <- train(approval ~., data = df_new, method = "rpart1SE", control = hypers, 
               trControl = train_control)
tree2

#e.)
fancyRpartPlot(tree1$finalModel, caption = "")
fancyRpartPlot(tree2$finalModel, caption = "")




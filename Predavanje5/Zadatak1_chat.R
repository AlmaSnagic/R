library(R6)
library(caret)
library(e1071)
library(rpart)
library(randomForest)

Predicted <- R6Class("Predicted",
                     public = list(
                       X_train = NULL,
                       X_test = NULL,
                       y_train = NULL,
                       y_test = NULL,
                       
                       data_split = function(data, target_col, split_ratio = 0.7) {
                         set.seed(123)
                         index <- createDataPartition(data[[target_col]], p = split_ratio, list = FALSE)
                         train_data <- data[index, ]
                         test_data <- data[-index, ]
                         
                         self$X_train <- train_data[, !(names(train_data) %in% target_col)]
                         self$y_train <- train_data[[target_col]]
                         self$X_test <- test_data[, !(names(test_data) %in% target_col)]
                         self$y_test <- test_data[[target_col]]
                         
                         message("Data successfully split into training and test sets.")
                       },
                       
                       naive_bayes_model = function() {
                         model <- naiveBayes(self$X_train, as.factor(self$y_train))
                         predictions <- predict(model, self$X_test)
                         cm <- confusionMatrix(predictions, as.factor(self$y_test))
                         print(cm)
                       },
                       
                       decision_tree_model = function() {
                         train_df <- cbind(self$X_train, target = as.factor(self$y_train))
                         model <- rpart(target ~ ., data = train_df, method = "class")
                         predictions <- predict(model, self$X_test, type = "class")
                         cm <- confusionMatrix(predictions, as.factor(self$y_test))
                         print(cm)
                       },
                       
                       random_forest_model = function() {
                         model <- randomForest(x = self$X_train, y = as.factor(self$y_train))
                         predictions <- predict(model, self$X_test)
                         cm <- confusionMatrix(predictions, as.factor(self$y_test))
                         print(cm)
                       },
                       
                       svm_model = function() {
                         model <- svm(x = self$X_train, y = as.factor(self$y_train))
                         predictions <- predict(model, self$X_test)
                         cm <- confusionMatrix(predictions, as.factor(self$y_test))
                         print(cm)
                       }
                     )
)

data("Titanic")
pred <- Predicted$new()
pred$data_split(data = iris, target_col = "Species", split_ratio = 0.7)
pred$naive_bayes_model()
pred$decision_tree_model()
pred$random_forest_model()
pred$svm_model()
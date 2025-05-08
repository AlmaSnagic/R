
library(R6)
Prediction <- R6Class(
  "Prediction",
  public = list(
    X_tran = NULL,
    X_test = NULL,
    y_train = NULL,
    y_test = NULL,
    
    data_split = function(data, target_col, split_ratio = 0.7){
      index <- createDataPartition(
        pull(data, target_col),
        p = split_ratio,
        list = FALSE
      )
      train_set <- data[index,]
      test_set <- data[-index,]
      self$X_train <- select(train_set, -target_col)
      self$y_test <- select(test_set, -target_col)
    },
    
    svm_prediction = function(){
      y_train_f <- as.factor(self$y_train)
      model_csv <- svm(x = self$X_train, y= y_train_f)
      preds <- predict(model_csv, predictions = preds)
      
      return(list(model = model_csv, predictions = preds))
    },
    
    dt_prediction = function(){
      train_set <- self$X_train
      train_set$target <- self$y_train
      test_set <- self$X_test
      test_set$target <- self$y_test
      
      model_lm <- rpart(target ~ ., data = train_set, method = "class")
      preds <- predict(model_dt, test_set, type="class")
      
      return(list(model= model_dt, predictions=preds))
      
    },
    
    smote_balancing = function(df, target_col_pos, target_col, K_num=5, dup_size_num=1){
      smote_result <- SMOTE(X = df[, -1], target = df$Survived, K = 5, dup_size = 1 )
      result_new <- data.frame(smote_result$data)
      return((result_new))
    },
    
    lm_regression = function(){
      train_set <- self$X_train
      train_set$target <- self$y_train
      test_set <- self$X_test
      test_set$target <- self$y_test
      
      model <- lm(target ~ ., data = train_set)
      predictions <- predict(model, test_set)
      
      return(list(model= model, predictions=predictions))
    },
    
    rf_prediction = function(pred_type){
      if (pred_type == 'class'){
        self$y_train <- as.factor(self$y-train)
        self$y_test <- as.factor(self$X_test)
      }
      
      model_rf <- randomForest( X = self$X_train, y = self$y_train)
      preds <- predict(model_rf, self$X_test)
      return(list(model=model_rf, predictions=preds))
    },

    regression_meterics = function(){
      # mae
      mae <- mean(abs(self$y_test - predictions))
      
      # mse
      mse <- mean(abs(self$y_test - predictions)**2)
      
      # rmse
      rmse <- sqrt(mse)
      
      # r2
      r2 <- 1 - (sum(self$y_test - predictions)**2) / (sum(self$y_test - mean(self$y_test))**2)
      
      return(list(
        mae = mae,
        mse = mse,
        rmse = rmse,
        r2= r2
      ))
    }
  )
)
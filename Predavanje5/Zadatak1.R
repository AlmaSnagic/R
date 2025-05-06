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
    }
    
    svm_prediction = function(){
      y_train_f <- as.factor(self$y_train)
      model_csv <- svm(x = self$X_train, y= y_train_f)
      preds <- predict(model_csv, predictions = preds)
      
      return(list(model = model_csv, predictions = preds))
    }
    
    dt_prediction = function(){
      train_set <- self$X_train
      train_set$target <- self$y_train
      test_set <- self$X_test
      test_set$target <- self$<y_test
      
      model_dt <- rpart(target ~ ., data = train_set, method = "class")
      preds <- predict(model_dt, test_set, type="class")
      
      return(list(model= model_dt, predictions=preds))
      
    }
  )
)
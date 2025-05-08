df <- df%>%mutate(Age = ifelse(is.na(Age), mean(Age, na.rm=TRUE), Age))
df <- df%>%mutate(Fare = ifelse(is.na(Fare), mean(Fare, na.rm=TRUE), Fare))


index <- createDataPartition(
  pull(df, "Survived"),
  p = 0.7,
  list = FALSE
)

train_set <- df[index, ]
test_set <- df[-index, ]

X_train <- select(train_set, -Survived)
X_test <- select(test_set, -Survived)

y_train <- pull(train_set, "Survived")
y_test <- pull(test_set, "Survived")

model <- rpart(Survived ~ ., data = train_set, method = "class")
predicted <- predict(model, test_set, type = "class")

#Decision Tree
model <- randonForest(x = X_train, y = y_train)
preds <- predict(model, X_test)


library(randomForest)
library(e1071)
library(pROC)

# Random tree
model_rf <- randomForest(x = X_train, y = y_train)
preds <- predict(model_rf, X_test)

# naive bayes
model_nb <- naiveBayes(X_train, y_train)
preds <- predict(model_nb, X_test)

X_test[4,]

# SVM
model_csv <- svm(x = X_train, y = y_train, scale = FALSE)
preds <- predict(model_csv, X_test)


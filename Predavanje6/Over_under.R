# over/under fitting
library(e1071)
library(rpart)
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

y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

model_rf <- naiveBayes(x = X_train, y = y_train)
preds <- predict(model_rf, X_test)

training_probability <- predict(model_rf, X_train, type = "raw")[,2]
test_probability <- predict(model_rf, X_test, type = "raw")[,2]
library(pROC)

train_roc = roc(y_train, training_probability)
test_roc = roc(y_test, test_probability)

train_df <- data.frame(
  FPR = 1-train_roc$specificities,
  TPR = train_roc$sensitivities,
  Dataset = "train"
)

test_df <- data.frame(
  FPR = 1-test_roc$specificities,
  TPR = test_roc$sensitivities,
  Dataset = "test"
)
roc_df <- bind_rows(train_df, test_df)

ggplot(roc_df, aes(x=FPR, y=TPR, color=Dataset)) + geom_line(size = 1.2)

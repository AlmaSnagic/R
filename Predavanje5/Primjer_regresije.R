index <- createDataPartition(
  pull(df, "Fare"),
  p = 0.7,
  list = FALSE
)

train_set <- df[index, ]
test_set <- df[-index, ]

X_train <- select(train_set, -Fare)
X_test <- select(test_set, -Fare)

y_train <- pull(train_set, "Fare")
y_test <- pull(test_set, "Fare")

model <- lm(Fare ~ ., data = train_set)
predictions <- predict(model, test_set)

library(randomForest)
model_rf <- randomForest(Fare ~ ., data = train_set)
preds <- predict(model_rf, X_test)


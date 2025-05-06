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

model <- randonForest(x = X_train, y = y_train)
preds <- predict(model, X_test)
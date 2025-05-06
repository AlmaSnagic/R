install.packages("smotefamily")
library(smotefamily)
smote_result <- SMOTE(X = df[, -1], target = df$Survived, K = 5, dup_size = 1 )
result_new <- data.frame(smote_result$data)


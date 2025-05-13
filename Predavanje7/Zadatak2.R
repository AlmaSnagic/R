library(R6)

NLP <- R6Class("NLP",
      public = list(
        one_hot_encong = function(s){
          words <- unlist(strsplit(tolower(s), " "))
          vocab <- unique(words)
          one_hot_matrix <- t(sapply(words, function(w) as.integer(vocab==w)))
        },
      
      
      cosine_function = function(a,b){
        dot_product <- sum(a*b)
        norm_a <- sqrt(sum(a**2))
        norm_b <- sqrt(sum(b**2))
        simil <- dot_product / (norm_a*norm_b)
        return(simil)
      }
    )
)



install.packages("reticulate")
reticulate::py_install("sentence-transformers")

library(reticulate)
sberts <- import("sentence_transformers")

model <- sberts$SentenceTransformer("all-MiniM-L6-v2")

sentence <- "This is an example sentence."
emb <- model$encode(sentence)

py_install("sentence-transformers", pip = TRUE)
py_install("torch", pip = TRUE)
py_install("sympy", pip = TRUE)

library(reticulate)
sberts <- import("sentence_transformers")

model <- sberts$SentenceTransformer("all-MiniM-L6-v2")

spam_data <- read.csv("C:/Users/Python/Desktop/spam.csv")
library(dplyr)

spam_data <- spam_data %>% select(v1, v2)
colnames(spam_data) <- c("label", "text")
# embeddings <- model$encode(spam_data$text)

embeddings <- c()
for (i in 1:length(spam_data[,1])) {
  embl <- model$encode(spam_data[1,2])
  embeddings <- append(embeddings, embl)
}
embeddings_df <- as.data.frame(embeddings)
library(e1071)

X_train <- embeddings_df[1:4500,]
spam_data <- spam_data %>% mutate(label = ifelse(label=="spam",1, 0))
y_train <- as.factor(spam_data[1:4500, 1])
X_test <- embeddings_df[4500:nrow(embeddings_df),]
y_test <- as.factor(spam_data[4500:nrow(embeddings_df), 1])

nb <- naiveBayes(x= X_train, y=y_train)
predictions<- predict(nb, X_test)

accuracy <- sum(y_test == predictions) / length((y_test))



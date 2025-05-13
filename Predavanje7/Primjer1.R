install.packages(c("tm", "SnowballC", "textstem"))
library(tm)
library(SnowballC)
library(textstem)

texts <- c(
  "Running, jumping and swimming are fun!",
  "This is example sentence to show removal."
)

corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument)

cleaned_texts <- sapply(corpus, as.character)

lemmatized <- lemmatize_strings(texts)


s <- "The cat sat on the mat"
words <- unlist(strsplit(tolower(s), " "))

vocab <- unique(words)
one_hot_matrix <- t(sapply(words, function(w) as.integer(vocab==w)))
colnames(one_hot_matrix) <- vocab
rownames(one_hot_matrix) <- words



cosine_function <- function(a,b){
  dot_product <- sum(a*b)
  norm_a <- sqrt(sum(a**2))
  norm_b <- sqrt(sum(b**2))
  simil <- dot_product / (norm_a*norm_b)
  return(simil)
}
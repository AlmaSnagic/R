text_data <- data.frame(
  doc_id = 1:4,
  category = c("science", "science", "sports", "sports"),
  text = c(
    "I love data science and machine learning",
    "Statistical analysis of data is interesting",
    "The football game last night was fun",
    "I love watching basketball and tennis"
  ),
  stringsAsFactors = FALSE
)

library(dplyr)
install.packages(c("tidytext", "tidyr"))
library(tidytext)
library(tidyr)

tokens <- text_data %>% unnest_tokens(word, text) %>% anti_join(stop_words)

word_counts <- tokens %>% count(doc_id, word)
tfidf <- word_counts %>% bind_tf_idf(word, doc_id, n)
tfidf_matrix <- tfidf %>% select(doc_id, word, tf_idf) %>%
  pivot_wider(names_from = word, values_from = tf_idf, values_fill=0)

features <- left_join(tfidf_matrix, text_data[, c("doc_id", "category")],
                      by = "doc_id")

head(features)



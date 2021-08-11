# install.packages("text2vec")
# install.packages("readtext")
# install.packages("tm")
# install.packages("wordcloud2")
# install.packages("wordcloud")

library(text2vec)
library(readtext)
library(tm)
library(wordcloud2)
library(wordcloud)
library(stringr)

my_data <- readtext("C:/Users/emiwes/Desktop/Notes/stats_notes/README.md")
my_data$doc_id <- 1

my_data$text <- tolower(my_data$text)
my_data$text <- str_remove_all(my_data$text, "\\d+")
my_data$text <- text2vec::word_tokenizer(my_data$text)

it <- itoken(my_data$text,
  ids = my_data$doc_id,
  # turn off progressbar because it won't look nice in rmd
  progressbar = F
)
it

vocab <- create_vocabulary(
  it = it,
  ngram = c(1, 1),
  stopwords = tm::stopwords()
)
vocab[vocab$term == "notes", ]$term_count <- 80
vocab[vocab$term == "stats", ]$term_count <- 100
vocab <- vocab %>% prune_vocabulary(term_count_min = 5)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it, vectorizer)

mat <- as.matrix(dtm_train)
words <- sort(rowSums(mat), decreasing = TRUE)
df <- data.frame(word = colnames(mat), freq = mat[1, ] %>% as.vector())
df$freq[df$word == "notes"] <- 150
df$freq[df$word == "stats"] <- 200
df <- df %>% arrange(freq)
wordcloud2(df, size = 1.6, color = "random-dark", shape = "triangle-forward")

wordcloud(df$word, df$freq,
  min.freq = 1,
  max.words = 200, random.order = T, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

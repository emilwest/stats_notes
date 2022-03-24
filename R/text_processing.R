# install.packages("text2vec")
# install.packages("readtext")
# install.packages("tm")
# install.packages("wordcloud2")

library(tidyverse)
library(SnowballC) # for stemming
library(text2vec) # for BoW model
library(tm) # for text preprocessing
library(wordcloud2) # for plotting wordclouds
library(webshot) # for saving wordclouds
library(htmlwidgets) # for saving wordclouds

# stemming 
stem_tokenizer <-  function(x, language = "swedish") {
  tokens = text2vec::word_tokenizer(x)
  lapply(tokens, SnowballC::wordStem, language=language)
}

load_stopwords <- function(.language) {
  stopwords <- tm::stopwords(.language)
  if (.language == "swedish") {
    stopwords <- c(stopwords, 
                   "ska", "ta", "del", "enl", "även", "innehåller",
                   "via", "in", "genom", "annat", "samt")
  }
  stopwords
}

preprocess_text <- function(text, ...) {
  
  args <- list(...)
  
  # default values for empty args 
  if (is.null(args$.stem)) args$.stem <- FALSE
  if (is.null(args$.remNumbers)) args$.remNumbers <- TRUE
  if (is.null(args$.remPunct)) args$.remPunct <- TRUE
  if (is.null(args$.toLower)) args$.toLower <- TRUE
  if (is.null(args$.language)) args$.language <- "swedish"
  
  if (args$.toLower) text <- tolower(text)
  if (args$.remNumbers) text <- tm::removeNumbers(text)
  if (args$.remPunct) text <- tm::removePunctuation(text)
  if (args$.stem) {
    preprocessed <- stem_tokenizer(text, language = .language)
  } else {
    preprocessed <- text2vec::word_tokenizer(text)
  }

  preprocessed
}

# function for summarizing word frequencies over all documents
dtm_to_df <- function(dtm) {
  mat <- as.matrix(dtm)
  colSums(mat) %>% 
    enframe(name = "word", value = "freq") %>% 
    arrange(desc(freq))
}

# plotting wordclouds
plotwordcloud <- function(.df,
                          outname=NULL,
                          outpath_base="./img/"
) {
  
  wctest <- wordcloud2::wordcloud2(.df, 
                                   size = 1, 
                                   color = "random-dark", 
                                   shape = "traingle-forward")
  if (!is.null(outname)) {
    outfile <- str_c(outpath_base, outname, ".png")
    outhtml <- str_c(outpath_base, outname, ".html")
    saveWidget(wctest, outhtml, selfcontained = F)
    webshot(outhtml, outfile, delay =5, vwidth = 600, vheight=600) 
    unlink(outhtml)
    unlink(str_c(outpath_base, outname, "_files"), recursive = TRUE)
  } else {
    wctest
  }
}

analyze_text <- function(.txt,
                         ngrams = c(1,3),
                         term_count_min = 2,
                         ...
                         ) {
  
  args <- list(...)
  if (is.null(args$.language)) args$.language <- "swedish"
  
  txt_tokenized <- preprocess_text(.txt, ...)
  stopwords_preprocessed <- load_stopwords(args$.language) %>%
    preprocess_text(...)
  
  it <- itoken(txt_tokenized,
               ids = seq_len(length(txt_tokenized))
  )
  
  vocab <- create_vocabulary(
    it = it,
    ngram = ngrams,
    stopwords = unlist(stopwords_preprocessed)
  )
  
  vocab <- prune_vocabulary(
    vocab, 
    term_count_min = term_count_min
  )
  
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  dtm_train <- text2vec::create_dtm(it, vectorizer)
  
  # defines tf-idf model. is normalized to L1 by default
  tfidf = TfIdf$new()
  # fit model to train data and transform train data with fitted model
  dtm_train_tfidf = fit_transform(dtm_train, tfidf)
  
  df_freq <- dtm_to_df(dtm_train)
  df_tfidf <- dtm_to_df(dtm_train_tfidf) %>% 
    dplyr::rename("tfidf" = "freq")
  
  df <- df_freq %>% 
    left_join(df_tfidf, by = "word")
  
  return(list(
    "dtm" = dtm_train,
    "dtm_tfidf" = dtm_train_tfidf,
    "vocab" = vocab,
    "df" = df
  ))
}


my_data <- readtext::readtext("C:/Users/emiwes/Desktop/Notes/stats_notes/README.Rmd")
txt <- my_data$text

preprocess_text(txt, .language = "english", .stem = T, .remNumbers = F)
stopwords_preprocessed <- load_stopwords("english") %>% preprocess_text()

res <- analyze_text(txt, ngrams = c(1,1), .language = "english")








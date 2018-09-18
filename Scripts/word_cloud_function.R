library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


my_word_cloud=function(x, MAX.WRDS=10){
  docs = Corpus(VectorSource(x))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("statistics", "statistical", "course", "introduction",'students',"will")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v, stringsAsFactors = FALSE) %>% mutate( relfreq = freq / sum(freq))
  fw = head(d, 5)
  
  # set.seed(1234)
  # wordcloud(words = d$word, freq = d$freq, min.freq = 3,
  #           max.words=MAX.WRDS, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  
}

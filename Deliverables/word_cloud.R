library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(readxl)

#Read Excel file for all years
sim.data <-read_excel("1997-2015 Informs Data.xlsx")

#Use Title column for word cloud and omit NA entries
data <- sim.data$Title
data <- na.omit(data)

#Create corpus
corpus <- Corpus(VectorSource(data))

data.text <- tm_map(corpus, PlainTextDocument)

#Remove punctuation
data.text <- tm_map(data.text, removePunctuation)

#Convert to all lower case
data.text <- tm_map(data.text, content_transformer(tolower))

#Convert words to their stems (e.g. simulating --> simulate)
data.text <- tm_map(data.text, stemDocument)

#Create document term matrix
dtm <- DocumentTermMatrix(data.text)

word.freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
df <- data.frame(word = names(word.freq), freq = word.freq)

#Remove common words
stopwords <- c("the", "and", "for", "with", "can", "use")
df <- df[!rownames(df) %in% stopwords,]

#Plot word cloud
set.seed(100)
wc2 <- wordcloud(df$word, df$freq, min.freq = 100)


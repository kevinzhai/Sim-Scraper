library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)

#Read csv file
sim.data <-read.csv(text=getURL("https://raw.githubusercontent.com/kevinzhai/Sim-Scraper/master/1997%2C%202001-2009%2C%202011-2015%20Informs%20Data.xlsx"), stringsAsFactors = FALSE, header=TRUE)

#Use Abstract column for word cloud and omit NA entries.
data <- sim.data$Abstract
data <- na.omit(data)

#Create corpus
corpus <- Corpus(VectorSource(data))

#This line is trying to fix errors later on in the code, but not working on all errors
corpus <- tm_map(corpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                    mc.cores=1)

data.text <- tm_map(corpus, PlainTextDocument)

#Remove punctuation
data.text <- tm_map(data.text, removePunctuation)

#Convert to all lower case
data.text <- tm_map(data.text, content_transformer(tolower))

#Convert words to their stems (e.g. simulating --> simulate)
data.text <- tm_map(data.text, stemDocument)

#Remove common words
#This line is causing issues so is commented now
#data.text <- tm_map(data.text, removeWords, c("the", "and", "for", "are", "can", "has",
 #                                             "have", "from","not","our", "this"))

data.text <- tm_map(data.text, PlainTextDocument)

#Create document term matrix
dtm <- DocumentTermMatrix(data.text)

freq <- colSums(as.matrix(dtm))
#Create sort order
ord <- order(freq, decreasing = TRUE)
#View top words
freq[head(ord)]

#List of top words for word cloud
top.words <- findFreqTerms(dtm, lowfreq=100)

set.seed(10)
wc <- wordcloud(names(freq), freq, min.freq = 100)




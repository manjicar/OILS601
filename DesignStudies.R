library(qdapTools)
library(tm)
library(RColorBrewer)
library(graph)
library(SnowballC)
library(Rgraphviz)
library(wordcloud)

# Load Word documents into environment
Bias1 <- read_docx("Biased1Clean.docx")
Bias2 <- read_docx("Biased2Clean.docx")
Control <- read_docx("Control3Clean.docx")

#Create corpuses
corpusBias1 <- Corpus(VectorSource(Bias1))
corpusBias2 <- Corpus(VectorSource(Bias2))
corpusControl <- Corpus(VectorSource(Control))

#Convert to lowercase, remove punctuation, remove stopwords, and stem words
corpusBias1 <- tm_map(corpusBias1, tolower)
corpusBias2 <- tm_map(corpusBias2, tolower)
corpusControl <- tm_map(corpusControl, tolower)

corpusBias1 <- tm_map(corpusBias1, PlainTextDocument)
corpusBias2 <- tm_map(corpusBias2, PlainTextDocument)
corpusControl <- tm_map(corpusControl, PlainTextDocument)

corpusBias1 <- tm_map(corpusBias1, removePunctuation)
corpusBias2 <- tm_map(corpusBias2, removePunctuation)
corpusControl <- tm_map(corpusControl, removePunctuation)

corpusBias1 <- tm_map(corpusBias1, removeWords, 
    c("P1", "P2", "P3", "yeah", "okay", "yah", "like", stopwords("english")))
corpusBias2 <- tm_map(corpusBias2, removeWords, 
    c("P1", "P2", "P3", "yeah", "okay", "yah", "one", "like", "just", stopwords("english")))
corpusControl <- tm_map(corpusControl, removeWords, 
    c("P1", "P2", "P3", "yeah", "okay", "yah", "like", stopwords("english")))

corpusBias1 <- tm_map(corpusBias1, stemDocument)
corpusBias2 <- tm_map(corpusBias2, stemDocument)
corpusControl <- tm_map(corpusControl, stemDocument)

#Create document term matrices
dtmBias1 <- DocumentTermMatrix(corpusBias1)
dtmBias2 <- DocumentTermMatrix(corpusBias2)
dtmControl <- DocumentTermMatrix(corpusControl)

#Compute and show main term frequencies
termFreqBias1 = colSums(as.matrix(dtmBias1))
termFreqBias2 = colSums(as.matrix(dtmBias2))
termFreqControl = colSums(as.matrix(dtmControl))

termFreqBias1[tail(order(termFreqBias1))]
termFreqBias2[tail(order(termFreqBias2))]
termFreqControl[tail(order(termFreqControl))]

findFreqTerms(dtmBias1, lowfreq = 5)
findFreqTerms(dtmBias2, lowfreq = 5)
findFreqTerms(dtmControl, lowfreq = 5)
freq <- 5
cT <- 0.15
par(mfrow = c(1, 1))
plot(dtmBias1, terms = findFreqTerms(dtmBias1, lowfreq = (freq)), corThreshold = (cT+.15), weighting = T)
plot(dtmBias2, terms = findFreqTerms(dtmBias2, lowfreq = freq), corThreshold = cT, weighting = T)
plot(dtmControl, terms = findFreqTerms(dtmControl, lowfreq = freq), corThreshold = cT, weighting = T)

par(mfrow = c(1, 3))
wordcloud(corpusBias1, max.words = 20, scale = c(5, .5), colors = brewer.pal(8, "Dark2"), random.order = F)
wordcloud(corpusBias2, max.words = 20, scale = c(5, .5), colors = brewer.pal(8, "Dark2"), random.order = F)
wordcloud(corpusControl, max.words = 20, scale = c(5, .5), colors = brewer.pal(8, "Dark2"), random.order = F)

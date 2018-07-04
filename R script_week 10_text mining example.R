# install.packages("XML")
# install.packages("tm")
# install.packages("RCurl")
library("XML")
library("tm")
library("RCurl")
library("dplyr")

#Use a web file: Note the web location for the speech
sba <- "http://www.historyplace.com/speeches/anthony.htm" %>%
    #encore the string as a URL
    URLencode() %>%
    # get the HTML tree from the URL
    htmlTreeParse(useInternal = TRUE) %>%
    unlist(xpathApply(., '//p' , xmlValue))


# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to 
# create a character vector.
sba = unlist(xpathApply(doc.html, '//p' , xmlValue))
head(sba, 3)

# A vector source interprets each element of the vector x as a document
words.vec <- VectorSource(sba)

# Collection of documents, class in OOP?
words.corpus <- Corpus(words.vec)
words.corpus
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(words.corpus)
tdm

m <- as.matrix(tdm)
wordCounts <- rowSums (m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

library(wordcloud)
CloudFrame <- data.frame (word = names(wordCounts), freq=wordCounts)

wordcloud(CloudFrame$word, CloudFrame$freq)

wordcloud(names(wordCounts), wordCounts, min.freq = 2, max.words = 50, rot.per = 0.35, colors=brewer.pal(8, "Dark2"))



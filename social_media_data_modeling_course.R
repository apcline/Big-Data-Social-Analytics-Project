install.packages("twitteR")
install.packages("rtweet")
install.packages("ggmap")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("tm", dependencies=TRUE)
install.packages("stringi")
library(twitteR)
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(ggmap)
library(ROAuth)
library(RCurl)
library(lubridate)
library(tm)
library(stringi)
library(stringr)
library(wordcloud)

TweetData <- read.csv2("/Users/alexis/Documents/TBS-M2/U2-Big_Data_Analytics/\
                       Social_Media_Data_Modeling/tweets-2.csv")
class(TweetData)
head(TweetData, n=2)

#removing special characters
#(bc we are working in english and will not recognize the french characters)
TweetData$location2<-iconv(TweetData$Tweet.Location, to = "ASCII", sub="")
TweetData$location2[TweetData$location2=="\"\""] <- NA
TweetData$location2[TweetData$location2=="\" \""] <- NA

#counting the number of times each location appears
TweetData %>%count(location2, sort=TRUE) %>%
  mutate(location2=reorder(location2,n)) %>%
  na.omit()%>% top_n(10)%>%ggplot(aes(x=location2,y=n))+
  geom_bar(stat="identity")+geom_col()+coord_flip() +
  labs(x = "Location", y = "Count",
       title = "Twitter users - unique locations ")+
  theme_light()

#hypothesis = many tweets in favor of ukraine (as majority are coming from US)

#time series of tweets counts
TweetData$Tweet.Posted.Time2<-dmy_hms(TweetData$Tweet.Posted.Time)
#plot time series
ts_plot(TweetData, "hours")+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+
  ggplot2::labs(x=NULL,y=NULL,
                title="Frequency of Ukraine war Twitter statuses",
                subtitle="Twitter status counts 1-hour intervals",
                caption="\nSource: Data collected from Twitter's API"
  )

#corpus creation and data cleaning
#removing special characters in non latin language
usableText <- iconv(TweetData$Tweet.Content, to = "ASCII", sub = " ") 
TweetData_corpus<-Corpus(VectorSource(usableText))
TweetData_corpus<-tm_map(TweetData_corpus, tolower)
TweetData_corpus<-tm_map(TweetData_corpus, removePunctuation)
TweetData_corpus<-tm_map(TweetData_corpus, removeNumbers)
TweetData_corpus<-tm_map(TweetData_corpus, function(x)removeWords(x,
                                                                  stopwords()))
TweetData_corpus<-tm_map(TweetData_corpus, function(x)removeWords(x,
                                                                  stopwords("french")))
TweetData_corpus<-tm_map(TweetData_corpus, function(x)removeWords(x,
                                                                  stopwords("italian")))
TweetData_corpus<-tm_map(TweetData_corpus, function(x)removeWords(x,
                                                                  stopwords("spanish")))
TweetData_corpus<-tm_map(TweetData_corpus,removeWords, c("ukrainewar", "ukraine",
                                                         "ukrainerussiawar","russia","ukrainian","russian","will",
                                                         "dont","want","said","may","ukrainekrieg","httpstcophpnpyyp",
                                                         "httpstcoilvjzefti","see","russiaukrainewar","war"))

text_corpus <- tm_map(TweetData_corpus,
                      content_transformer(function(x)
                        iconv(x,to='ASCII',sub='byte')))
# The document-term matrix
TweetData.tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(TweetData.tdm)
m[1:2,1:5]

#most freq terms in the matrix
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 5)

#word frequency
barplot(d[1:20,]$freq, las = 3,
        names.arg = d[1:20,]$word,col ="lightblue",
        main ="Most frequent words",
        ylab = "Word frequencies")

#identify terms used at least 50 times
findFreqTerms(TweetData.tdm, lowfreq=50)[1:10]

#wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 40,
          max.words=100, random.order=FALSE,
          colors=brewer.pal(4,"Dark2"))

#remove sparse terms from the term-document matrix
TweetData.tdm<-removeSparseTerms(TweetData.tdm, sparse=0.95)
#convert the term-document matrix to a data frame
TweetData.df <- as.data.frame(as.matrix(TweetData.tdm))
#scale the data since clustering is sensitive to the scale of the data used
TweetData.df.scale <- scale(TweetData.df)
#create the distance matrix: each cell represents the distance between each
#pair of documents/tweets
TweetData.dist <- dist(TweetData.df.scale, method = "euclidean")

#cluster the data: tweets are grouped into classes
TweetData.fit<-hclust(TweetData.dist, method="ward.D2")
plot(TweetData.fit, main="Cluster-UkraineWar")
#plotting clusters
groups <- cutree(TweetData.fit, k=2)
plot(TweetData.fit, main="Cluster-UkraineWar")
rect.hclust(TweetData.fit, k=2, border="red")

tags<-function(x) toupper(grep("#",strsplit(x, " +")[[1]],value=TRUE))
l <- nrow(TweetData)
taglist <- vector(mode = "list", l)
texts <- vector(mode = "character", length = l)

#extracting the tweet text from each tweet status
for (i in 1:l) texts[i] <- TweetData$Tweet.Content[i]
texts <- iconv(texts, to = "ASCII", sub="") 
# ... and populate it
j<-0
for(i in 1:l){
  if(is.na(str_match(texts[i],"#"))[1,1]==FALSE){
    j <- j+1
    taglist[[j]] <- str_squish(removePunctuation(tags(ifelse(is.na(str_match(texts[i]))))))
  }
}
alltags <- NULL
for (i in 1:l) alltags<-union(alltags,taglist[[i]])

#creating empty graph
library(igraph)
hash.graph <- graph.empty(directed = T)
#populate w nodes
hash.graph <- hash.graph + vertices(alltags)
#populate with edges
for (tags in taglist){ if (length(tags)>1){
  for (pair in combn(length(tags),2,simplify=FALSE, FUN=function(x) sort(tags[x])))
    if (pair[1]!=pair[2]) {
      if (hash.graph[pair[1],pair[2]]==0)
        hash.graph<-hash.graph+edge(pair[1],
                                    pair[2])
    } 
}
} 

#network construction
V(hash.graph)$color <- "black"
E(hash.graph)$color <- "black"
V(hash.graph)$name <- paste("#",V(hash.graph)$name,
                            sep = "")
V(hash.graph)$label.cex = 0.75
V(hash.graph)$size <- 20
V(hash.graph)$size2 <- 2
hash.graph_simple<-delete.vertices(simplify(hash.graph),
                                   degree(hash.graph)<=97)

plot(hash.graph_simple, edge.width = 2, 
     edge.color = "black", vertex.color = "SkyBlue2",
     vertex.frame.color="black", label.color = "black",
     vertex.label.font=2, edge.arrow.size=0.5) 

#sentiment analysis
library(sentimentr)
plain.text<-vector()
for(i in 1:dim(TweetData)[1]){
  plain.text[i]<-TweetData_corpus[[i]][[1]]
}
sentence_sentiment<-sentiment(get_sentences(plain.text))
sentence_sentiment



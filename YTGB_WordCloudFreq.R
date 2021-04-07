getwd()
setwd("~/Desktop/python/Project/YT GB/")

library(stringr)
library(qdap)
library(tm)
library(wordcloud)
library(mgsub)
library(stopwords)
library(ggplot2)

GBYT_df7.tags <- read.csv("GBYT_df7.csv",header=FALSE, fill=TRUE, encoding = "utf-8",quote = "")

#Set Options
options(stringasfactors=F)

#keep a backup version before pre-processing
GBYT_df7.tags.bkup<-GBYT_df7.tags

#replace the words listed in "to.be.replace" by those listed in "replacement"
to.be.replace <-c('next day delivery', 'wish list', 'wishlist')
replace.by <-c('next-day-delivery','wish-list','wish-list')
GBYT_df7.tags<-mgsub(GBYT_df7.tags, to.be.replace, replace.by )

#A function changes all to lower case (and return NA stead of error if it is a special character)
#Return NA instead of tolower error
tryTolower <-function(x){
  #return NA when there is an error
  y=NA
  #tryCatch error
  try_error=tryCatch(tolower(x),error=function(e) e)
  #if not an error 
  if (!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

#create my stop words list
custom.stopwords<-c(stopwords('english'),'lol')

#create a pre-processing function using tm functions and the above two
clean.corpus<-function(corpus){
  corpus<-tm_map(corpus,content_transformer(tryTolower))
  corpus<-tm_map(corpus,removeWords,custom.stopwords)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,stemDocument, language = "english")
  return(corpus)
}

#define the tweets object
the.corpus <- VCorpus(VectorSource(GBYT_df7.tags))

#clean the tweets with the function created earlier
the.corpus<-clean.corpus(the.corpus)

#Create the term document matrix
tdm <- DocumentTermMatrix(the.corpus,control=list(weighting=weightTf))

#remove sparse terms from a doucment if the sparsity is more than 99%
tdm.n<-removeSparseTerms(tdm, 0.99)

#redefine it as matrix for easy to computation
tdm.tags<-as.matrix(tdm.n)

#check dimension of the tweets
dim(tdm.tags)

#check term frequency
term.freq<-colSums(tdm.tags)

#create a dataframe with the term and then the frequency as the second column
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2],decreasing=T),]
freq.df[1:20,]

#Plot word frequencies when frequency is higher than 80
hp <- ggplot(subset(freq.df, freq.df$frequency>50), aes(word, frequency))    
hp <- hp + geom_bar(stat="identity")   
hp <- hp + theme(axis.text.x=element_text(angle=45, hjust=1))   
plot(hp)   

#create a word cloud and maximum number of words are 50
wordcloud(freq.df$word,freq.df$frequency,max.words=50)

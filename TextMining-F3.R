## Text Mining - Sentiment scoring

setwd("C:/Users/vinod.nethilath/OneDrive - Accenture/TextMining/text-mining")

#Set Options
options(stringsAsFactors=F) ## default setting 
Sys.setlocale('LC_ALL','C')

library(stringi)
library(stringr)
library(qdap)
library(tm)
library(ggplot2)
library(ggthemes)
##library(igraph)
library(wordcloud)
##library(plotrix)

## Prepare new pos & neg cases. Extract old pos & neg from key.pol (y = 1 and -1) and bundle all of them together
new.pos<-c('rofl','lol')
old.pos<-subset(as.data.frame(key.pol),key.pol$y==1)
all.pos<-c(new.pos,old.pos[,1])
new.neg<-c('kappa','meh')
old.neg<-subset(as.data.frame(key.pol),key.pol$y==-1)
all.neg<-c(new.neg,old.neg[,1])
all.polarity<-sentiment_frame(all.pos,all.neg,1,-1)

polarity('ROFL, look at that!',polarity.frame =all.polarity)


## Sentiment Word Clouds

bos.airbnb<-read.csv('bos_airbnb_1k.csv')

bos.pol<-polarity(bos.airbnb$comments)

## pol includes $all (data frame with word counts, polarity and other details by document / row in csv; $group has summary details)

ggplot(bos.pol$all, aes(x=polarity, y=..density..)) + theme_gdocs() + geom_histogram(binwidth=.25, fill="darkred",colour="grey60", size=.2) + geom_density(size=.75)

##ggplot(bos.pol$all, aes(x=polarity)) + theme_gdocs() + geom_histogram(binwidth=.25, fill="darkred",colour="grey60", size=.2)

bos.airbnb$polarity<-scale(bos.pol$all$polarity)

## Create seperate docs for neg and positive 
pos.comments<-subset(bos.airbnb$comments, bos.airbnb$polarity>0)
neg.comments<-subset(bos.airbnb$comments, bos.airbnb$polarity<0)

pos.terms<-paste(pos.comments,collapse = " ")
neg.terms<-paste(neg.comments,collapse = " ")
all.terms<-c(pos.terms,neg.terms)

all.corpus<-VCorpus(VectorSource(all.terms))

## TDIF : TF * IDF and TF = number of occurences for the word / unique word occurences ; 
##      IDF = log base 10 (total document in corpus / number of documents with term t in it)

all.tdm<-TermDocumentMatrix(all.corpus,
                            control=list(weighting=weightTfIdf, removePunctuation = TRUE,stopwords=stopwords(kind='en')))
all.tdm.m<-as.matrix(all.tdm)
colnames(all.tdm.m)<-c('positive','negative')

comparison.cloud(all.tdm.m, max.words=100, colors=c('darkgreen','darkred'))


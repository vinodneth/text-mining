## Text Mining - LDA - Latent Dirichlet Allocation

setwd("C:/Users/vinod.nethilath/OneDrive - Accenture/TextMining/text-mining")

#Set Options
options(stringsAsFactors=F) ## default setting 

library(stringi)
library(stringr)
library(qdap)
library(tm)
library(lda)
library(GuardianR)
library(LDAvis)
library(car)
library(treemap)
library(pbapply)
library(data.table)
library(text2vec)

text<-read.csv("Guardian_articles_11_14_2015_12_1_2015.csv")

## Cleansing for LDA

articles <-iconv(text$body,"latin1","ASCII",sub="")
articles <- gsub('http\\S+\\s*','',articles)
articles <- bracketX(articles,bracket = 'all')
articles <- gsub("[[:punct:]]",'',articles)
articles <- removeNumbers(articles)
articles <- tolower(articles)
articles <- removeWords(articles,c(stopwords('en'),'pakistan','gmt','england'))

articles <- pblapply(articles,blank.removal)

## Lexicalize function in LDA 
documents <- lexicalize(articles)

## gives $documents with the position and counts of the word & $vocab which is the distinct list of words
wc<-word.counts(documents$documents,documents$vocab)
doc.length <-document.lengths(documents$documents)

## LDA analysis
k<-4
num.iter<-25
alpha<-0.02
eta <- 0.02
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K=k, vocab = documents$vocab, num.iterations = num.iter, alpha = alpha, eta = eta, initial = NULL, burnin = 0, compute.log.likelihood = TRUE)

## plots over the iterations to show the value stabilizing
plot(fit$log.likelihood[1,])

## provides the top words by topics (for k)
top.topic.words(fit$topics,7,by.score=TRUE)

## provides top documents by the topic
top.topic.documents(fit$document_sums,1)

## LDAvis plots


theta <- t(pbapply(fit$document_sums + alpha, 2, function (x) x/sum(x)))
phi <- t(pbapply(t(fit$topics) + eta, 2, function (x) x/sum(x)))

article.json <- createJSON(phi = phi,theta = theta, doc.length = doc.length, 
                              vocab = documents$vocab, term.frequency = as.vector(wc))



## Treemap!!!

assignments<- unlist(pblapply(fit$assignments,doc.assignment))

assignments<-recode(assignments,"1='Cricket1';2='Paris Attacks';3='Cricket2'; 4='Unknown'")

article.ref<-seq(1:nrow(text))
article.pol<-polarity(articles)[[1]][3]

article.tree.df<-cbind(article.ref,article.pol,doc.length,assignments)

##treemap(article.tree.df,index = c("assignments","article.ref"),vSize = "doc.length", 
##           vColor = "polarity", type = "value", title = "Guardian Articles", pallete = c("red","white","green"))

treemap(article.tree.df,index = c("assignments","article.ref"),vSize = "doc.length", 
        vColor = "polarity", type = "value", title = "Guardian Articles")



## Text2vec 
text<-fread('Airbnb-boston_only.csv')

airbnb<-data.table(review_id=text$review_id,comments=text$comments,review_scores_rating=text$review_scores_rating)

airbnb$comments<-removeWords(airbnb$comments,c(stopwords('en'),'Boston'))
airbnb$comments<-removePunctuation(airbnb$comments)
airbnb$comments<-stripWhitespace(airbnb$comments)
airbnb$comments<-removeNumbers(airbnb$comments)
airbnb$comments<-tolower(airbnb$comments)

tokens <-strsplit(airbnb$comments,split = " ", fixed = T)

vocab <-create_vocabulary(itoken(tokens),ngram = c(1,1))

vocab <-prune_vocabulary(vocab,term_count_min = 5)

iter <- itoken(tokens)
vectorizer<-vocab_vectorizer(vocab) ##,grow_dtm = F, skip_grams_Window = 5)
tcm<-create_tcm(iter,vectorizer)


## General 

ex.text<- c("Text mining is a     good time","Text mining is a good time")

char.vec<- unlist(strsplit(ex.text[1]," "))

char.vec<-subset(char.vec,nchar(char.vec)>0)

ex.text<- c("this is a text document")

ex.text.lex<-lexicalize(ex.text)

##library(ggplot2)
##library(ggthemes)
##library(igraph)
##library(wordcloud)
##library(plotrix)
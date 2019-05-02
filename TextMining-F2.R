## Text Mining - Visualization scripts

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
library(igraph)
library(wordcloud)
library(plotrix)



## load commmon functions
source("./common/commonfunc.R")

## From TextMining-F1.R
text.df<-read.csv('oct_delta.csv')
tweets<-data.frame(doc_id=seq(1:nrow(text.df)),text=text.df$text)
ds<-DataframeSource(tweets)
corpus <- Corpus(ds)
corpus <-clean.corpus(corpus)
tdm<-TermDocumentMatrix(corpus,control=list(weighting=weightTf))
tdm.tweets.m<-as.matrix(tdm)
term.freq<-rowSums(tdm.tweets.m)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

## words as factors
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))
## plot bar graph - axis flipped
ggplot(freq.df[1:20,], aes(x=word, y=frequency))+geom_bar(stat="identity", fill='darkred')+coord_flip()+theme_gdocs()+ geom_text(aes(label=frequency))


## Word Associations - associations for the top works to understand how they are used
associations<-findAssocs(tdm, 'apologies', 0.11)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels=associations$terms)

ggplot(associations, aes(y=terms)) + geom_point(aes(x=apologies), data=associations, size=5)+
  theme_gdocs()+ geom_text(aes(x=apologies,label=apologies), colour="darkred",hjust=-.25,size=8)+
  theme(text=element_text(size=20),axis.title.y=element_blank())

## Word Network
## use grep to find the select the subset of tweets, prepare TDM and work on the network

refund<-tweets[grep("refund", tweets$text, ignore.case=T), ]

refund.ds<-DataframeSource(refund)
refund.corpus <- Corpus(refund.ds)
refund.corpus<-clean.corpus(refund.corpus)
refund.tdm<-TermDocumentMatrix(refund.corpus, control=list(weighting=weightTf))

refund.m<-as.matrix(refund.tdm)
refund.adj<-refund.m %*% t(refund.m)
refund.adj<-graph.adjacency(refund.adj, weighted=TRUE, mode="undirected", diag=T)
refund.adj<-simplify(refund.adj)

plot.igraph(refund.adj, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred",
            vertex.label.cex=.7, edge.color="gray85")
title(main='@DeltaAssist Refund Word Network')

##use of qdap without creating the adjacency matrix etc....
## Option one where subset with grep refund Output passed
word_network_plot(refund$text[1:3])
title(main='@DeltaAssist Refund Word Network')
## Option two where full tweet passed with match string
word_associate(tweets$text,match.string=c('refund'), stopwords=Top200Words,network.plot = T, cloud.colors=c('gray85','darkred'))
title(main='@DeltaAssist Refund Word Network')



##dendograms
tdm2 <- removeSparseTerms(tdm, sparse=0.975)

hc <- hclust(dist(tdm2, method="euclidean"), method="complete")

plot(hc,yaxt='n', main='@DeltaAssist Dendrogram')

## using dend change - 
hcd <- as.dendrogram(hc)
clusMember <- cutree(hc,4)
clusDendro <- dendrapply(hcd, dend.change)
plot(clusDendro, main = "@DeltaAssist Dendrogram", type = "triangle",yaxt='n')

## circular dendogram

library(dendextend)
library(circlize)
hcd<-color_labels(hcd,4, col = c('#bada55','darkgrey', "black", 'darkred'))
hcd<-color_branches(hcd,4, col = c('#bada55','darkgrey', "black", 'darkred'))
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4)


## Word cloud examples and usages
# One corpus word cloud
wordcloud(freq.df$word,freq.df$frequency, max.words = 100, colors=c('black','darkred'))

# Compare / contrast corpora using word cloud
## Get tweets of amazon and Delta
amzn<-read.csv('amzn_cs.csv')
delta<-read.csv('oct_delta.csv')

amzn.vec<-clean.vec(amzn$text)
delta.vec<-clean.vec(delta$text)
## collape the vector to single string add them in corpus as two documents. create TDM of that corpus
amzn.vec <- paste(amzn.vec, collapse=" ")
delta.vec <- paste(delta.vec, collapse=" ")
all <- c(amzn.vec, delta.vec)
corpus <- VCorpus(VectorSource(all))
tdm <- TermDocumentMatrix(corpus)
tdm.m <- as.matrix(tdm)
colnames(tdm.m) = c("Amazon", "delta")
## select pallete from RCBPallet and excude last 2 coors
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:4)]

commonality.cloud(tdm.m, max.words=200, random.order=FALSE,colors=pal)

comparison.cloud(tdm.m, max.words=200, random.order=FALSE,title.size=1.0, colors=brewer.pal(ncol(tdm.m),"Dark2"))


## Polarized tag plot - view the difference in count for a term - can do only for 2 corpus
#pick none zeros of amazaon and delta
common.words <- subset(tdm.m, tdm.m[, 1] > 0 & tdm.m[, 2] > 0)

## identify absolute difference in count add as a new column and sort to get top values first
difference <- abs(common.words[, 1] - common.words[, 2])

common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[, 3], decreasing = TRUE), ]

top25.df <- data.frame(x = common.words[1:25, 1],y = common.words[1:25, 2],
                       labels = rownames(common.words[1:25, ]))

pyramid.plot(top25.df$x, top25.df$y, labels = top25.df$labels,
             gap = 14, top.labels = c("Amazon", "Words", "delta"),
             main = "Words in Common", laxlab = NULL, raxlab = NULL, unit = NULL)






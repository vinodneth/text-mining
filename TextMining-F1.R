## Text Mining Scripts

setwd("C:/Users/vinod.nethilath/OneDrive - Accenture/TextMining/text-mining")

#Set Options
options(stringsAsFactors=F) ## default setting 
Sys.setlocale('LC_ALL','C')

library(stringi)
library(stringr)
library(qdap)
library(tm)

## load commmon functions
source("./common/commonfunc.R")

## load file
text.df<-read.csv('oct_delta.csv')
## counts for the initial entries in df
nchar(head(text.df$text))

## mean char size for the entries
mean(nchar(text.df$text))

## below uses subset to exclude docs with ZERO size
subset.doc<-subset(text.df,nchar(text.df$text)>0)

## code to see usages of mgsub
fake.text<-'R text mining is good but text mining in python is also'

patterns<-c('good','also','text mining')
replacements<-c('great','just as suitable','tm')
## takes patterns and replacement as vector applies on the provided text iteratively
mgsub(patterns,replacements,fake.text)

## paste function usage to get te timestamp as a combined field
patterns<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
replacements<-seq(1:12)
text.df$month<-mgsub(patterns,replacements,text.df$month)
text.df$combined<-paste(text.df$month,text.df$date,text.df$year, sep='-')

library(lubridate)
text.df$combined<-mdy(text.df$combined)

## agent wise counts
weekdays<-subset(text.df,text.df$combined >= mdy('10-05-2015') & text.df$combined <= mdy('10-09-2015'))
table(as.factor(last.chars(weekdays$text,2)))

## grep (gives the locatin of the first instance) and grepl (providesTRUE / FALSE based on existence of each instance...)
sum(grepl(c('sorry|apologize'),text.df$text,ignore.case=T))/nrow(text.df)
sum(grepl('http', text.df$text, ignore.case = T))/nrow(text.df)
sum(grepl('[0-9]{3})|[0-9]{4}', text.df$text))/nrow(text.df)

## stringi and str_count
library(stringi)
stri_count(text.df$text, fixed='http')

## stringr library
library(stringr)
str_detect(text.df$text,'http')

patterns <- with(text.df, str_detect(text.df$text, 'http') |
                   str_detect(text.df$text, 'DM'))
text.df[patterns,5]


## Pre processing
## modified to work with Datasource - latest version of tm does not support readTabular
tweets<-data.frame(doc_id=seq(1:nrow(text.df)),text=text.df$text)

## meta data for tweet
## - not there in latest tm ! meta.data.reader <- readTabular(mapping=list(content='text', id='ID'))
##corpus <- VCorpus(DataframeSource(tweets), readerControl=list(reader=meta.data.reader))

ds<-DataframeSource(tweets)
corpus <- Corpus(ds)


corpus <-clean.corpus(corpus)

as.list(corpus)[1]


## spell checks in text mining

tm.definition<-'Txt mining is the process of distilling actionable insyghts from text.'

## provides the word no and word which are suspected to be wrong spelling
which_misspelled(tm.definition)


## option to interactively chose from auto corrections proposed
check_spelling_interactive(tm.definition)


## TDM - prep and analysis

## with default weightage
tdm<-TermDocumentMatrix(corpus,control=list(weighting=weightTf))
## tdm as matrix
tdm.tweets.m<-as.matrix(tdm)
## view matrix dim
dim(tdm.tweets.m)
## view part of the matrix
tdm.tweets.m[2250:2255,1340:1342]

##
term.freq<-rowSums(tdm.tweets.m)

freq.df<-data.frame(word=names(term.freq),frequency=term.freq)

## sort freq and pick first 10
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df[1:10,]






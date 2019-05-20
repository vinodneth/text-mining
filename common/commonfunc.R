## Collation of functions written as part of Text Mining exercise

## function which takes text and number of last chars to return the same
last.chars<-function(text,num){
  last<-substr(text, nchar(text)-num+1,nchar(text))
  return(last)
}


# Return NA instead of tolower error when special chars are found in text passed
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

## Generic clean function for corpus 
clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  ##custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')
  ##corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

## leverage checkspelling to replace with suggested word in the string passed to function 
fix.text <- function(myStr) {
  check <- check_spelling(myStr)
  splitted <- strsplit(myStr, split=' ')
  for (i in 1:length(check$row)) {
    splitted[[check$row[i]]][as.numeric(check$word.no[i])] = check$suggestion[i]
  }
  df <- unlist(lapply(splitted, function(x) paste(x, collapse = ' ')))
  return(df)
}

## function for dendogram - color coding etc...
dend.change <- function(n) {
  labelColors <- c('darkgrey', 'darkred', 'black', '#bada55')
  
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(
      names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col =
                              labCol)
  }
  n
}

##pre process text vectors
clean.vec<-function(text.vec){
  custom.stopwords <- c(stopwords('english'), 'sorry', 'amp', 'delta', 'amazon')
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

## function for blank removal 
blank.removal<-function(x) {
  x<-unlist(strsplit(x,' '))
  x<-subset(x,nchar(x)>0)
  x<-paste(x,collapse = ' ')
}


## function for use with LDA - to get best topic by article from LDA model
doc.assignment <- function (x) {
  x<- table(x)
  x<-as.matrix(x)
  x<-t(x)
  x<-max.col(x)
}


mydata <- readRDS("data_use.RDS")
library(dplyr)
library(tidyr)
library(wordcloud)
library(tm)
library(httr)
library(rvest)

mydata <- mydata %>% 
        separate(review_helpfulness, c("helpful.v", "total.v"), sep = "/", remove=FALSE)
mydata$helpful.v <- as.numeric(mydata$helpful.v)
mydata$total.v <- as.numeric(mydata$total.v)
mydata <- mydata %>% 
        mutate(review_h=helpful.v/total.v)
mydata$review_date <- as.Date(as.POSIXct(mydata$review_time, origin="1970-01-01"))

product <- as.data.frame(table(mydata$product_productid))
product$Var1 <- as.character(product$Var1)
product <- arrange(product, Freq)

####################################################################
####################################################################
dataset <- readRDS("data_use.RDS")
con <- load("connoi.RData")
ext <- load("extreme.RData")
exp <- connoi[1:100,]
ext <- extreme[1:100,]
pal2 <- brewer.pal(8,"Dark2")
set1 <- connoi$review_userid
for (i in 1:500) {
  if (i == 1){
    show <- mydata[which(dataset$review_userid==set1[i]),]
  }
  else {
    temp <- mydata[which(dataset$review_userid==set1[i]),]
    show <- rbind(show,temp)
  }
}
wordsset <- NULL
for (i in 1:nrow(show)) {
  words <- strsplit(show$review_summary[i], "\\W")[[1]]
  words <- words[!tolower(words)%in%c(stopwords(),"")]
  wordsset <- c(wordsset, words)
}
temp <- VCorpus(VectorSource(wordsset))
wordcloud(temp, scale=c(5,.2),min.freq=250,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)

set2 <- extreme$review_userid
for (i in 1:500) {
  if (i == 1){
    show1 <- mydata[which(dataset$review_userid==set2[i]),]
  }
  else {
    temp1 <- mydata[which(dataset$review_userid==set2[i]),]
    show1 <- rbind(show1,temp1)
  }
}
wordsset1 <- NULL
for (i in 1:nrow(show1)) {
  words1 <- strsplit(show1$review_summary[i], "\\W")[[1]]
  words1 <- words1[!tolower(words1)%in%c(stopwords(),"")]
  wordsset1 <- c(wordsset1, words1)
}
temp1 <- VCorpus(VectorSource(wordsset1))
wordcloud(temp1, scale=c(5,.2),min.freq=200,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)


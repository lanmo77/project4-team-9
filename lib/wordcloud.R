start<- Sys.time()
movie <- readRDS("data_use.RDS")
end  <- Sys.time()
end-start
library(dplyr)
library(tidyr)
library(wordcloud)
library(tm)
library(httr)
library(rvest)

#movie$product_productid <- as.character(movie$product_productid)
#movie$review_userid <- as.character(movie$review_userid)
#movie$review_helpfulness <- as.character(movie$review_helpfulness)
#movie$review_summary <- as.character(movie$review_summary)
#movie$review_text <- as.character(movie$review_text)

movie <- movie %>% 
        separate(review_helpfulness, c("helpful.v", "total.v"), sep = "/", remove=FALSE)
movie$helpful.v <- as.numeric(movie$helpful.v)
movie$total.v <- as.numeric(movie$total.v)
movie <- movie %>% 
        mutate(review_h=helpful.v/total.v)
movie$review_date <- as.Date(as.POSIXct(movie$review_time, origin="1970-01-01"))

product <- as.data.frame(table(movie$product_productid))
product$Var1 <- as.character(product$Var1)
product <- arrange(product, Freq)
#product <- filter(product, Freq>100)


opar <- par()

# Change the index of product id to see different plots.
input <- product$Var1[4]

showset <- movie %>%
        filter(product_productid==input) %>%
        select(product_productid, review_date, review_score, review_summary, review_text) %>%
        arrange(review_date)

par(mfrow=c(1,2))
plot(showset$review_date, showset$review_score, "l",xlab = "date", ylab = "score")
abline(h=mean(showset$review_score))
title("score records")
plot(showset$review_date, cummean(showset$review_score), "l", xlab = "date", ylab = "score")
abline(h=mean(showset$review_score))
title("cumulative mean of score")
opar

# Word Cloud
wordsset <- NULL
for (i in 1:nrow(showset)) {
        words <- strsplit(showset$review_summary[i], "\\W")[[1]]
        words <- words[!tolower(words)%in%c(stopwords(),"")]
        wordsset <- c(wordsset, words)
}
temp <- VCorpus(VectorSource(wordsset))
pal2 <- brewer.pal(8,"Dark2")
wordcloud(temp, scale=c(5,.2),min.freq=3,max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)


##    READ DATA
##________________________________________________________________________________________
##________________________________________________________________________________________

library(plyr)
library(reshape)
library(mice)
# base <- readRDS("/Users/JPC/Documents/Columbia/2nd Semester/1. Applied Data Science/2. Homeworks/Project 4/data/data_use.RDS")
load("/Users/JPC/Documents/Columbia/2nd Semester/1. Applied Data Science/2. Homeworks/Project 4/project4-team-8/data/train.RData")
load("/Users/JPC/Documents/Columbia/2nd Semester/1. Applied Data Science/2. Homeworks/Project 4/project4-team-8/data/test.RData")

training <- train[,c("product_productid","review_userid","review_score","product_name")]
test <- test[,c("product_productid","review_userid","review_score","product_name")]

# c("product_productid","review_userid","review_score")
# index.train <- sample(1:nrow(base),10000)
# index.test <- sample(setdiff(1:nrow(base),index.train),100000)
# training <- base[index.train,]
# test <- base[index.test,]
# matrix <- cast(data = base.red,review_userid~product_productid,fun.aggregate = mean,value = "review_score",fill=0)

movie.names <- unique(training[,c("product_productid","product_name")])
# nrow(movie.names)

products <- movie.names$product_productid
users <- names(table(training$review_userid))
# length(users)

##    AUXILIARY FUNCTIONS
##________________________________________________________________________________________
##________________________________________________________________________________________

return.name <- function(x){
  movie.names[movie.names$product_productid==x,]$product_name
}

mutual.reviewer.f <- function(data.base,movie1, movie2) {
  reviews1 <- subset(data.base, product_productid==movie1)
  reviews2 <- subset(data.base, product_productid==movie2)
  reviewers_sameset <- intersect(reviews1[,'review_userid'],
                                 reviews2[,'review_userid'])
  if (length(reviewers_sameset)==0) {
    NA
  } else {
    reviewers_sameset
  }
}

extract.review <- function(data.base,movie,user){
  aa <- subset(data.base,product_productid==movie)
  aaa <- subset(aa,review_userid==user)
  mean(aaa$review_score)
}

similarity <- function(data.base,movie1,movie2){
  mutual.reviewers <- mutual.reviewer.f(data.base,movie1,movie2)
  if(is.na(mutual.reviewers)){
    NA
  } else {
    rev1 <- sapply(mutual.reviewers, function(x) extract.review(data.base,movie1,x))
    rev2 <- sapply(mutual.reviewers, function(x) extract.review(data.base,movie2,x))
    pp <- sum(rev1==5&rev1==rev2)
    pn <- sum(rev1==5&rev1!=rev2)
    np <- sum(rev2==5&rev1!=rev2)
    nn <- sum(rev2!=5&rev1!=5)
    movies.track <<- c(movie1,movie2)
    if ((pp+nn)>(np+pn)){
    # length(mutual.reviewers)-pp-pn-np-nn 
    contingency.table <- data.frame(p=c(pp,np),n=c(pn,nn))
    rownames(contingency.table) <- c("p","n")
    chi <- chisq.test(contingency.table)$statistic
    chi
    } else {
      NA
    }
  }
}

bridge <- function(data.base,x) {
  b1 <- x$movie1
  b2 <- x$movie2
  similarity(data.base,b1, b2)
}

chi.for.all <- function(movie.pairs,data.base){
  ddply(movie.pairs.train, .(movie1, movie2),function(x) bridge(data.base,x) , .progress="text")
}

test.average <- function(data.base,movie1,movie2){
   mutual.reviewers <- mutual.reviewer.f(data.base,movie1,movie2)
   if(is.na(mutual.reviewers[1])){
     NA
   } else {
     rev1 <- sapply(mutual.reviewers, function(x) extract.review(data.base,movie1,x))
     rev2 <- sapply(mutual.reviewers, function(x) extract.review(data.base,movie2,x))
     names.like <-   names(subset(rev1,rev1==5))
     if (length(names.like)<2){
       NA
     } else{
       other.average <- mean(subset(rev2,names(rev2)%in%names.like))
       other.average
     }
     
   }   
}

bridge.test <- function(data.base,x){
  b1 <- x$movie1
  b2 <- x$movie2
  test.average(data.base,b1, b2)
}

average.for.all <- function(movie.pairs,data.base){
  
}

##    COMPUTE CHI-SQ FOR EVERY PAIR OF MOVIES
##________________________________________________________________________________________
##________________________________________________________________________________________

movie.pairs.train <- expand.grid(movie1=products,movie2=products,stringsAsFactors = F)
movie.pairs.train <- subset(movie.pairs.train,movie1!=movie2)

# movie.pairs.test <- expand.grid(movie1=products,movie2=products,stringsAsFactors = F)
# movie.pairs.test <- subset(movie.pairs.test,movie1!=movie2)

# class(movie.pairs.train)
# dim(movie.pairs.train)

# movie1 <- "079070546X"
# movie2 <- "B00000JGEK"
# movie1 <- movie.pairs.train[1,]$movie1
# movie2 <- movie.pairs.train[1,]$movie2
# movie <- movie2
# user <- mutual.reviewers[1]
# data.base <- training
# movie.pairs <- movie.pairs.train
# rm(data.base,movie1,movie2,mutual.reviewers,rev1,rev2,movie,user)

# similarity(training,movie1,movie2)
# bridge(training,movie.pairs.train[4,])
# extract.review(training,movie1,mutual.reviewers[1])

t0 <- proc.time()
chi.raw <- chi.for.all(movie.pairs.train,training)
proc.time()-t0

names(chi.raw)[3] <- "chi"
chi <- chi.raw [!is.na(chi.raw$chi),]
max.chi <- max(chi$chi)
chi$distance <- max.chi-chi$chi
# chi[order(chi$chi,decreasing = T),]

distances.names <- chi[,-4]
distances.names <- distances.names[order(distances.names$chi,decreasing = T),]
temp.names <- movie.names
names(temp.names)[1] <- "movie1"
distances.names <- merge(distances.names,temp.names,by.x = "movie1")
names(distances.names)[4] <- "name1"
temp.names <- movie.names
names(temp.names)[1] <- "movie2"
distances.names <- merge(distances.names,temp.names,by.x = "movie2")
names(distances.names)[5] <- "name2"
distances.names <- distances.names[,c("name1","name2","movie1","movie2","chi")]

##    COMPUTE TEST METRICS 
##________________________________________________________________________________________
##________________________________________________________________________________________


#train.averages <- ddply(.data = movie.pairs.train,.variables = .(movie1,movie2),.fun = function(x) bridge.test(training,x),.progress="text")

t0 <- proc.time()
test.averages <- ddply(.data = movie.pairs.train,.variables = .(movie1,movie2),.fun = function(x) bridge.test(test,x),.progress="text")
proc.time()-t0


##    RECOMMENDATION TOOL
##________________________________________________________________________________________
##________________________________________________________________________________________

top.similar <- function(movie,number){
  temp.distance <- distances.names[distances.names$movie1==movie,]
  temp.distance <- temp.distance[order(temp.distance$chi,decreasing = T),c("name2","chi")]
  return(head(temp.distance,number))
}

return.name("B00004TX12")
top.similar(products[1],5)

return.name(products[2])
top.similar(products[2],5)

return.name(products[3])
top.similar(products[3],5)




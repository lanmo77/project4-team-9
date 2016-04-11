#Data set up
movie <- readRDS("data_use.RDS")
movie$x <- rep(1, dim(movie)[1])
dir_num <- aggregate(movie$x,list(movie$product_director),sum)[which(count_dir$x > 4000),]
dir_num <- dir_num[-9,]
dir_score <- aggregate(movie$review_score,list(movie$product_director),mean)
dir_score <- dir_score[which(dir_score$Group.1 == "Ang Lee"|dir_score$Group.1 == "Bryan Singer"|
                  dir_score$Group.1 == "Clint Eastwood"|dir_score$Group.1 == "David Fincher"|
                  dir_score$Group.1 == "Doug Liman"|dir_score$Group.1 == "James Cameron"|
                  dir_score$Group.1 == "John McTiernan"|dir_score$Group.1 == "Martin Scorsese"|
                  dir_score$Group.1 == "Quentin Tarantino"|dir_score$Group.1 == "Ridley Scott"|
                  dir_score$Group.1 == "Robert Zemeckis"|dir_score$Group.1 == "Roland Emmerich"|
                  dir_score$Group.1 == "Ron Howard"|
                  dir_score$Group.1 == "Sam Raimi"|dir_score$Group.1 == "Stanley Kubrick"|
                  dir_score$Group.1 == "Stephen Sommers"|dir_score$Group.1 == "Steven Spielberg"|
                  dir_score$Group.1 == "Tim Burton"|dir_score$Group.1 == "Zack Snyder"),]
dir_20 <- cbind(dir_num, dir_score)[,-3]
colnames(dir_20) <- c("name","review_num","review_score")
dir_20$review_score <- (dir_20$review_score - min(dir_20$review_score) + 0.1)*10
dir_20 <- cbind(id <- seq(1,19,1), dir_20)

from <- c(seq(1,18,1), seq(1,17,1), seq(1,16,1), seq(1,15,1), seq(1,14,1))
to <- c(seq(2,19,1), seq(3,19,1), seq(4,19,1), seq(5,19,1), seq(6,19,1))
type <- rep(3,80)
x <- c(lag_1, lag_2, lag_3, lag_4, lag_5)
link <- data.frame(from,to,type,x)
#link <- aggregate(link$x, list(link$from, link$to, link$type), mean)
colnames(link) <- c("from", "to", "type", "weight")
#link <- link[order(link$from, link$to),]
rownames(link) <- NULL

############################################################################################
############################################################################################
# Getting edge measurement
#lag 1
lag_1 <- vector()
for (k in 1:18) {
  one <- unique(movie[which(movie$product_director == dir_20$name[k]),2])
  two <- unique(movie[which(movie$product_director == dir_20$name[k+1]),2])
  mutual = 0
  for (i in 1:length(one)){
    for (j in 1:length(two)) {
      if (one[i] == two[j]) {
        mutual = mutual + 1
      }
    }
    lag_1[k] = mutual
  }
}

#lag 2
lag_2 <- vector()
for (k in 1:17) {
  one <- unique(movie[which(movie$product_director == dir_20$name[k]),2])
  two <- unique(movie[which(movie$product_director == dir_20$name[k+2]),2])
  mutual = 0
  for (i in 1:length(one)){
    for (j in 1:length(two)) {
      if (one[i] == two[j]) {
        mutual = mutual + 1
      }
    }
    lag_2[k] = mutual
  }
}

#lag 3
lag_3 <- vector()
for (k in 1:16) {
  one <- unique(movie[which(movie$product_director == dir_20$name[k]),2])
  two <- unique(movie[which(movie$product_director == dir_20$name[k+3]),2])
  mutual = 0
  for (i in 1:length(one)){
    for (j in 1:length(two)) {
      if (one[i] == two[j]) {
        mutual = mutual + 1
      }
    }
    lag_3[k] = mutual
  }
}

#lag 4
lag_4 <- vector()
for (k in 1:15) {
  one <- unique(movie[which(movie$product_director == dir_20$name[k]),2])
  two <- unique(movie[which(movie$product_director == dir_20$name[k+4]),2])
  mutual = 0
  for (i in 1:length(one)){
    for (j in 1:length(two)) {
      if (one[i] == two[j]) {
        mutual = mutual + 1
      }
    }
    lag_4[k] = mutual
  }
}

#lag 5
lag_5 <- vector()
for (k in 1:14) {
  one <- unique(movie[which(movie$product_director == dir_20$name[k]),2])
  two <- unique(movie[which(movie$product_director == dir_20$name[k+5]),2])
  mutual = 0
  for (i in 1:length(one)){
    for (j in 1:length(two)) {
      if (one[i] == two[j]) {
        mutual = mutual + 1
      }
    }
    lag_5[k] = mutual
  }
}

############################################################################################
############################################################################################




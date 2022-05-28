#Text mining in R
#Project- Mary Kay 

Sys.setlocale('LC_ALL','C') #English(US)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-16.jdk/Contents/Home")
setwd("/Users/maryannakay/Desktop")

alltopics <- read.csv("alltopics.csv")
alltopics<-data.frame(alltopics)

res<- c()
for(i in 1:ncol(alltopics)){
  res <- c(res,alltopics[,i])
}

unique_words<- unique(tolower(res))


df <- data.frame()
for(i in 1:ncol(alltopics)){
  topic_words <- alltopics[,i]
  arr <- rep(0, 506)
  for(j in 1:length(unique_words)){
    word = unique_words[j]
    indx <-  which(topic_words == word)
    if(length(indx)==0){
      indx <- 0
    }
    arr[j] <- indx
  }
  df <- rbind(df,arr)
}

df<-data.frame(df)

row.names(df) <- names(alltopics)
names(df)<-unique_words



# ---- k-means ----------
library(NbClust)

number_of_clusters<-NbClust(df, distance = "euclidean", method = "kmeans", index = "tau")
number_of_clusters$All.index
number_of_clusters$Best.nc
number_of_clusters$Best.partition

# Loading package
library(ClusterR)
library(cluster)

set.seed(240)
fit.km <- kmeans(df, centers=4,  nstart=100)
fit.km$size
df$km<- fit.km$cluster
fit.km$centers
fit.km 

#library(tidyverse)  # data manipulation
#library(cluster)    # clustering algorithms
#library(factoextra)
#fviz_cluster(fit.km, data = df)

#t.test between each two centroids
dfcenters<-data.frame(fit.km$centers)
dfcenters<-scale(dfcenters)
t.test(dfcenters[1,],dfcenters[2,], conf.level = 0.95)
t.test(dfcenters[1,],dfcenters[3,], conf.level = 0.95)
t.test(dfcenters[1,],dfcenters[4,], conf.level = 0.95)
t.test(dfcenters[2,],dfcenters[3,], conf.level = 0.95)
t.test(dfcenters[2,],dfcenters[4,], conf.level = 0.95)
t.test(dfcenters[3,],dfcenters[4,], conf.level = 0.95)

t.test(dfcenters[1,],dfcenters[1,], conf.level = 0.95)
t.test(dfcenters[2,],dfcenters[2,], conf.level = 0.95)
t.test(dfcenters[3,],dfcenters[3,], conf.level = 0.95)
t.test(dfcenters[4,],dfcenters[4,], conf.level = 0.95)
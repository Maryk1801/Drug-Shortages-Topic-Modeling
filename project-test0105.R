#Text mining in R
#Project- Mary Kay 

Sys.setlocale('LC_ALL','C') #English(US)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-16.jdk/Contents/Home")
setwd("/Users/maryannakay/Desktop/test1")

#libraries
library(stringi)
library(tm)
library(textstem)
library(ggplot2)
library(ggthemes)
#library(sentimentr)
library(topicmodels)
library(ldatuning)
library(LDAvis)
library(plyr)
library(dplyr)
library(tidytext)

#library(SnowballC) 
#library(httpuv)
library(qdap)
#library(lexicon)
#library(reshape2)
#library(rlang)
#library(skmeans)
#library(textclean)
#library(cluster)
#library(bench)
#library(data.table)
library(tidyverse)
#library(gsl)
library(gmp)
options(stringsAsFactors = F)

# tweets file
drugs1 <- read.csv("drugs shortage 010521.csv")
drugs.all<-rbind(drugs1)
#create df of tweets
drugs.all<-data.frame(doc_id=seq(1:nrow(drugs.all)),text=drugs.all$text,date=drugs.all$created_at)

################topic modeling##########
#remove links
drugs.all$text <- gsub("http\\S*", "", drugs.all$text)
drugs.all$text <- gsub("https\\S*", "", drugs.all$text)
drugs.all$text <- gsub("\n", "", drugs.all$text)
drugs.all$text <- gsub("@\\S*", "",drugs.all$text)
drugs.all$text <- gsub("&amp", "", drugs.all$text)
drugs.all$text <- gsub("(\\.+|[[:punct:]])", " \\1 ", drugs.all$text)
drugs.all$text <- iconv(drugs.all$text, "latin1", "ASCII", sub="") #convert Latin to ASCII

# build a corpus
myCorpus <- VCorpus(VectorSource(drugs.all$text))
#clean the corpus
myCorpus <- tm_map(myCorpus, content_transformer(tolower))# convert to lower case
custom.stopwords <- c(stopwords("english"),'the','will','for','are','shortage','shortages','drug','drugs')#remove stopwords
myCorpus <- tm_map(myCorpus, removeWords, custom.stopwords)
myCorpus <- tm_map(myCorpus, removePunctuation)# remove punctuation
myCorpus <- tm_map(myCorpus, removeNumbers)# remove numbers
myCorpus <- tm_map(myCorpus, stripWhitespace)# remove extra whitespace
myCorpus <- tm_map(myCorpus, stemDocument)

write.csv(drugs.all, "drugs.all.csv")

tweetCorpus.df <- as.data.frame(myCorpus)

## extract unique elements
x<-drugs.all$text
which(duplicated(x))
noduplicate <- drugs.all[!duplicated(x),]

drugs.all$cleantext<- tweetCorpus.df$text
merged.data <- drugs.all[order(drugs.all$date),]

library(plyr)
merged.data <- arrange(merged.data, text, date)
merged.data.no.dupe <- merged.data[!duplicated(merged.data$text),1:4 ]

colnames(merged.data.no.dupe)[2]<- "text"
colnames(merged.data.no.dupe)[3]<- "created_at"
colnames(merged.data.no.dupe)[4]<- "text_after_cleaning"
merged.data.no.dupe <- subset(merged.data.no.dupe, nchar(merged.data.no.dupe$text_after_cleaning)>0)
summary(merged.data.no.dupe$text_after_cleaning)
View(merged.data.no.dupe)
write.csv(merged.data.no.dupe, "merged.data.no.dupe.csv") #?????? ?????

# build a corpus
BCorpus<-data.frame(doc_id=seq(1:nrow(merged.data.no.dupe)),text=merged.data.no.dupe$text_after_cleaning)

myCorpus <- VCorpus(VectorSource(BCorpus$text))

dtm <- DocumentTermMatrix(myCorpus,control = list(weighting = weightTf))
dtm 

terms_in_doc <- apply(dtm,1,sum) #number of terms in a document
dtm <- dtm[terms_in_doc>0,] #remove from dtm
empty_docs <- Docs(dtm[terms_in_doc==0,]) #empty docs
text_df_filtered <- dtm[-as.numeric(as.character(empty_docs)),] #original text without empty docs

# find top terms
# TDM, *without* tfXidf weighting
term.freq <- colSums(as.matrix(dtm)) #summarize each row (number of appearances of each term)
top.terms <- term.freq[order(term.freq, decreasing = T)][1:100]
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
head(freq.df,10)


#number of topics
library(ldatuning)

result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77,alpha=0.1),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

#LDA-9-----------------------------
lda9 <- LDA(dtm, k = 9, control=list(seed=77,alpha=0.1),method = "Gibbs") #lower alpha, we assume documents contain fewer topics
termsByTopic <- terms(lda9, 30)
termsByTopic
posterior(lda9)$topics[1:30,] #topic distribution for each document (ten first documents)

topics <- topics(lda9)
topics.labeled <- recode(topics, '1'='Topic 1','2'='Topic 2','3'='Topic 3','4'='Topic 4', '5'='Topic 5','6'='Topic 6', '7'='Topic 7', '8'='Topic 8', '9'='Topic 9')
table(topics) #number of terms in each topic 

colnames(termsByTopic) <- c('1'='Topic 1','2'='Topic 2','3'='Topic 3','4'='Topic 4', '5'='Topic 5','6'='Topic 6', '7'='Topic 7', '8'='Topic 8' , '9'='Topic 9')


#most common term within each topic
ap_topics<- tidy(lda9,matrix="beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic))) + 
  geom_col(show.legend = TRUE ) +
  scale_fill_manual(values = c( "#F44336", "#F06292", "#7B1FA2", "#80DEEA", "#AED581", "#FFF176", "#FFB74D", "#A1887F", "#212121")
                    , name = "Topics", labels = c("1","2","3","4","5","6", "7", "8", "9")) +
  facet_wrap(~topic, scales="free") + 
  labs(title ="Topic Modeling- Top Terms By Topic (betas) For 23-30 April 2021") + 
  scale_y_reordered()

# plot topic over time
topics.by.date <- data.frame(date=as.Date(merged.data.no.dupe$created_at), topic=factor(topics.labeled))

library(ggplot2)

#density plot
ggplot(topics.by.date, aes(date, fill = topics.labeled)) + geom_density(alpha = 0.5) +   # alpha is a transparency param
  scale_fill_manual(values = c( "#F44336","#F06292", "#7B1FA2", "#80DEEA", "#AED581", "#FFF176", "#FFB74D", "#A1887F", "#212121")) +
  labs(title ="Density Plot For 23-30 April 2021") 

# bar plot
ggplot(topics.by.date, aes(date, fill = topics.labeled)) + 
  geom_bar(position = "stack")+scale_x_date(date_breaks = "week") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c( "#F44336","#F06292", "#7B1FA2", "#80DEEA", "#AED581", "#FFF176", "#FFB74D", "#A1887F", "#212121")) +
  labs(title ="Bar Plot For 23-30 April 2021") 


tweets.with.topic <- data.frame(text = merged.data.no.dupe$text_after_cleaning, created = as.character(merged.data.no.dupe$created_at), 
                                topic = as.character(topics.by.date$topic))

write.csv(tweets.with.topic, 'tweets_with_topic_pop.csv')



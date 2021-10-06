#Text mining in R
#Project- Mary Kay

Sys.setlocale('LC_ALL','C') #English(US)
Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-16.jdk/Contents/Home")
setwd("/Users/maryannakay/Desktop/tweets")
vignette("auth")

# Connect to tweeter
library(rtweet)
token <- create_token(
  app = "rtweet_token",
  consumer_key = "xxx",
  consumer_secret = "xxx")

# drug shortage tweets
drugs <- search_tweets(q = "(drug shortage) OR (drugs shortage) OR (drug shortages) OR (drugs shortages) OR #drugshortage OR #drugshortages",
                       n = 10000, since = "2021-04-23",until = Sys.Date(),lang = "en", include_rts = FALSE, retryonratelimit=FALSE, type = "recent")

drugs$text <- iconv(drugs$text, "latin1", "ASCII", sub="") #remove Latin characters and emojis

save_as_csv(drugs, file_name = "drugs shortage 010521.csv", prepend_ids = TRUE, na = "",fileEncoding = "UTF-8")

#read the csv file of the tweets 
drugs <-read.csv("drugs shortage 010521.csv")
class(drugs)
head(drugs$text,10)



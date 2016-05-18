
install_github("twitteR", username="")
install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("dplyr","tidyr" )
install.packages("plyr")
install.packages("sentiment", "ggplot2", "wordcloud", "RColorBrewer")
install.packages("qdap")
library(twitteR)
library(devtools)
library(dplyr)
library(plyr)
library(qdap)

#Access Twitter/enable R to pull tweets
api_key<-"mmslwYmAIDkn9YjKEIWDg402N"
api_secret<-"UEqJf66Dzj2c3pO406Sq5XG8fZjZrkAWldE99wYbTUmRRM0F8n"
access_token<-"724654344138706944-z0HN3ZevUI3VMqGMNEr1zhmXTGzULPg"
access_token_secret<-"f3gLnwkK3JAUdYPBa3z76FibZfBFAdwWDttY5N18y1JTs"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Try to collect Tweets from Denver area--0 results returned:
zikaGeo=searchTwitter("zika", n=100, geocode="39.742043, -104.991531,2000mi")

#Collect zika tweets in english (4 batches from different days, 1500 per batch):
zika.tweets2=searchTwitter("zika", n=1500, lang="en", since='2016-05-12', until='2016-05-13')
zika.tweets3=searchTwitter("zika", n=1500, lang="en", since='2016-05-13', until='2016-05-14')
zika.tweets4=searchTwitter("zika", n=1500, lang="en", since='2016-05-14', until='2016-05-15')
zika.tweets5=searchTwitter("zika", n=1500, lang="en", since='2016-05-15', until='2016-05-16')

zika.test=searchTwitter("zika", n=10, lang="en")
zika.test
#Combine 4 smaller (n=1500) lists into larger list (n=6000):
zika.tweets_all=as.list(c(zika.tweets2, zika.tweets3,zika.tweets4, zika.tweets5))
                        
#Examine list:
tweet_all=zika.tweets_all[[1]]
class(tweet_all)
tweet_all$getScreenName()
tweet_all$getText()

#Function to get all tweets (all text)
zika.text_all=laply(zika.tweets_all, function(t_all) t_all$getText())
#How many tweets (texts)
length(zika.text_all)
#See the first (5) texts
head(zika.text_all, 5)
#Download Hu and Lui's opinion lexicon and Separate into positive vs negative:
positive = scan('C://Users/Jenni/Desktop/BIOS6640/Project_Zika/positive.txt', what='character', comment.char=';')
negative = scan('C://Users/Jenni/Desktop/BIOS6640/Project_Zika/negative.txt', what='character', comment.char=';')
#Add pos/negative words specific to disease/zika (that might not be included in the lists):
pos.words= c(positive, 'cure', 'treatable', 'vaccine', 'treatment')
neg.words= c(negative, 'death', 'outbreak', 'untreatable', 'contagious', 'defect')

#Create score.sample function that "cleans" text and scores number of positive vs negative words per tweet:
score.sentiment= function(sentences, pos.words, neg.words, .progress='none'){
  require(plyr)
  require(stringr)
  scores=laply(sentences, function(sentence, pos.words, neg.words){
    sentence=gsub('[[:punct:]]', '', sentence)
    sentence=gsub('[[:cntrl:]]', '', sentence)
    sentence=gsub('\\d+', '', sentence)
    sentence=tolower(sentence)
    word.list=str_split(sentence, '\\s+')
    words=unlist(word.list)
    pos.matches=match(words, pos.words)
    neg.matches=match(words, neg.words)
    pos.matches=!is.na(pos.matches)
    neg.matches=!is.na(neg.matches)
    score=sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df=data.frame(score=scores, text=sentences)
  return(scores.df)
}

#SENTIMENT SCORE Test: Test zika list data:
#Make text usable--remove odd characters:
usableText_all=str_replace_all(zika.text_all, "[^[:graph:]]", " ")
#Perform sentiment anaysis on cleaned text:
zika.scores_all = score.sentiment(usableText_all, pos.words, neg.words, .progress='text')
zika.scores_all
class(zika.scores_all)
zika.scores_all$score
score_summary<-summary(zika.scores_all$score)
View(score_summary)
#Count the number of positive, negative, and neutral results from sentiment analysis:
count_pos_all <-length(which(zika.scores_all$score>0))
count_pos_all
count_neg_all <- length(which(zika.scores_all$score<0))
count_neg_all
count_neutral_all<-length(which(zika.scores_all$score==0))
count_neutral_all
summary(zika.scores_all$score)
#Create HISTOGRAM of sentiment scores:
hist(zika.scores_all$score, xlim=c(-5, 5), breaks=20)
?hist
##########################################################################
#Count the number of times certain location words appear in the tweets:
china_count<-length(grep("China", zika.text_all, ignore.case=TRUE))
china_count
brazil_count<-length(grep("Brazil", zika.text_all, ignore.case=TRUE))
brazil_count
PuertoRico_count<-length(grep("Puerto Rico", zika.text_all, ignore.case=TRUE))
PuertoRico_count
EasterIsland_count<-length(grep("Easter Island", zika.text_all, ignore.case=TRUE))
EasterIsland_count
US_count<-length(grep("United States", zika.text_all, ignore.case=TRUE))
US_count
US_count2<-length(grep("US", zika.text_all, ignore.case=FALSE))
US_count2
US_count3<-length(grep("U.S.", zika.text_all, ignore.case=FALSE))
US_count3
US_count_tot<-sum(US_count, US_count2, US_count3)
US_count_tot
Mexico_count<-length(grep("Mexico", zika.text_all, ignore.case=TRUE))
Mexico_count
SouthAmer_count<-length(grep("South America", zika.text_all, ignore.case=TRUE))
SouthAmer_count
FL_count<-length(grep("florida", zika.text_all, ignore.case=TRUE))
FL_count
NY_count<-length(grep("NY", zika.text_all, ignore.case=FALSE))
NY_count
TX_count<-length(grep("texas", zika.text_all, ignore.case=TRUE))
TX_count
VA_count<-length(grep("virginia", zika.text_all, ignore.case=TRUE))
VA_count

#Make text usable in tm package:
library(stringr)
#Repeat from above:
#usableText_all=str_replace_all(zika.text_all, "[^[:graph:]]", " ")
library(tm)
zika_txt_all<-usableText_all
zika_corp_all<-Corpus(VectorSource(zika_txt_all))
zika_dtm_all<-DocumentTermMatrix(zika_corp_all)



#Remove Stopwords:
zika_no_stop_all<-tm_map(zika_corp_all, removeWords, stopwords("english"))
library(SnowballC)
#Stem words:
zika_stem<-tm_map(zika_no_stop_all, stemDocument)
#Remove punctuation:
zika_no_punc<-tm_map(zika_stem, removePunctuation)
#Turn to dtm:
zika_dtm_clean<-DocumentTermMatrix(zika_no_punc)


#Find words that appear at least 200 times (no stop words, and stemmed):
zika_freq_terms<-findFreqTerms(zika_dtm_clean, lowfreq=200, highfreq = Inf)
zika_freq_terms

#Find most common words using tm package:
#Corr search With stopwords &punctuation removed and words stemmed (15% correlation)
zika_assocs_nostop<-findAssocs(zika_dtm_clean, c("zika"), 0.2)
zika_assocs_nostop

library(wordcloud)
zika_plaintext<-tm_map(zika_no_punc, PlainTextDocument)
wordcloud(zika_plaintext, max.words=50, random.order=FALSE)



##################################################################
#Look at other factors of interest: retweets, usernames, etc
isretweet<-laply(zika.tweets_all, function(x) x$getIsRetweet())
summary(isretweet)
retweet_count<-laply(zika.tweets_all, function(x) x$getRetweetCount())
summary(retweet_count)
favorite<-laply(zika.tweets_all, function(x) x$favorited())
summary(favorite)
favoritecount<-laply(zika.tweets_all, function(x) x$favoriteCount())

#Find names of top 10 most frequent tweeters
names<-laply(zika.tweets_all, function(x) x$screenName)
sorted_names<-sort(table(names), decreasing=TRUE)[1:10]
sorted_names
class(sorted_names)
namedf<-data.frame(sorted_names)
colnames(namedf)<-'number of zika tweets'
View(namedf, "Top_10_Tweeters")
?View

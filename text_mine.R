library(XML)
library(tm)
library(SnowballC)
library(wordcloud)

setwd("~/workspace/Pubmed_mining/")
pubmed<-xmlTreeParse("../../Downloads/pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract", xmlValue)
abstr.df<-do.call("rbind", abstr)

abstrCorpus<-Corpus(DataframeSource(abstr.df))

abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
abstrCorpus<-tm_map(abstrCorpus, removeNumbers)

myStopwords<-c(stopwords('english'), "available", "via")

#a specific set of stopwords will need to be developed for this analysis

##"r" does not exist in the stopwords vector...
#myStopwords<-myStopwords[-which(myStopwords=="r")]

abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
abstrCorpus<-tm_map(abstrCorpus, stemDocument)
dictCorpus<-abstrCorpus
abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
inspect(abstrCorpus[1:3])

##stemCompletion breaks corpus...
#abstrCorpus<-tm_map(abstrCorpus, stemCompletion, dictionary=dictCorpus)
#inspect(abstrCorpus[1:3])

tdm<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[1:10,1:10])

################
##Some basic analyses
################

findFreqTerms(tdm,lowfreq = 500)
findAssocs(tdm,terms = c("human", "risk", "exposur"), corlimit = 0.25)

tdm.m<-as.matrix(tdm)
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
myNames<-names(tdm.s)

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png("wordCloud.png", height=800, width=800, units="px")
wordcloud(tdm.df$word, tdm.df$freq, min.freq = 500)
dev.off()

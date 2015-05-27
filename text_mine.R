library(XML)
library(tm)
library(SnowballC)

setwd("~/workspace/Pubmed_mining/")
pubmed<-xmlTreeParse("../../Downloads/pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract", xmlValue)
abstr.df<-do.call("rbind", abstr)

abstrCorpus<-Corpus(VectorSource(abstr.df))

abstrCorpus<-tm_map(abstrCorpus, tolower)
abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
abstrCorpus<-tm_map(abstrCorpus, removeNumbers)

myStopwords<-c(stopwords('english'), "available", "via")
##"r" does not exist in the stopwords vector...
#myStopwords<-myStopwords[-which(myStopwords=="r")]

abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
dictCorpus<-abstrCorpus
abstrCorpus<-tm_map(abstrCorpus, stemDocument)
inspect(abstrCorpus[1:3])

##stemCompletion func fails for unknown reason
##abstrCorpus<-tm_map(abstrCorpus, stemCompletion, type="first", dictionary=dictCorpus)

##Term Document matrix func call fails.
myDtm<-TermDocumentMatrix(abstrCorpus)

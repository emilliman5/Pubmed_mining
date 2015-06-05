library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)
library(topicmodels)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

files<-list.files(pattern = "SP_")
files<-files[grep(".txt", files)]

docs<-do.call("rbind", lapply(files, function(x) paste(readLines(file(x, open="r")), collapse=" ")))
docs<-cbind(gsub(".txt", "",files), docs)

SP<-VCorpus(VectorSource(docs[,2]))
SP<-tm_map(SP, content_transformer(tolower))
SP<-tm_map(SP, removePunctuation)
SP<-tm_map(SP, removeNumbers)

stopWords<-read.table("stopwords.txt")
myStopwords<-c(stopwords('english'), stopWords$V1)

SP<-tm_map(SP, removeWords, myStopwords)
dictCorpus<-SP
SP<-tm_map(SP, stemDocument)
SP<-tm_map(SP, stripWhitespace)

SP<-mclapply(SP, stemCompletion2, dictionary=dictCorpus, mc.cores=24)
SP<-Corpus(VectorSource(SP))

tdm<-TermDocumentMatrix(SP)
inspect(tdm)

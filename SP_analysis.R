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
files<-files[-grep(".csv", files)]

SP<-do.call(rbind, lapply(files, function(x) read.table(x, header=F, quote="", sep="")))

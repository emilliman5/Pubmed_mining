library(tm)
library(topicmodels)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

abstrCorpus<-Corpus(DirSource("data/Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("data/CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
for (x in colnames(metaData)) {
    meta(abstrCorpus, x)<-metaData[,x]
}
rm(metaData)

tdm<-TermDocumentMatrix(abstrCorpus)
load("data/LDA_models_current.rda")
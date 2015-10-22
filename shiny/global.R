library(tm)
library(topicmodels)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

abstrCorpus<-Corpus(DirSource("../Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("../CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
for (x in c("PMID","GrantID","Date", "FY", "FY.Q")) {
    meta(abstrCorpus, x)<-metaData[,x]
}
rm(metaData)

tdm<-TermDocumentMatrix(abstrCorpus)
load("../LDA_models_current.rda")
library(tm)
library(topicmodels)
library(proxy)

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
load("data/LDA_FY_models_current.rda")

betaDist<-lapply(models, function(x){
    z<-as.matrix(x@beta)
    rownames(z)<-apply(terms(x,4),2,function(y) paste(y,collapse=","))
    simil(z, method="cosine") 
})

gammaDist<-lapply(models, function(x){
    fys<-getFactorIdx("FY",meta(abstrCorpus))
    lapply(fys, function(y){
        simil(t(x@gamma)[,y], method="cosine")
    })    
})

betatrees<-lapply(betaDist, function(x){
    hclust(x,method = "ward.D2")
})

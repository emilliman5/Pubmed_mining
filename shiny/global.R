library(tm)
library(topicmodels)
library(proxy)

extraFunFile<-"shiny_textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
    source("arcplot.r", keep.source=TRUE)
}

abstrCorpus<-Corpus(DirSource("data/Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("data/CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
for (x in colnames(metaData)) {
    meta(abstrCorpus, x)<-metaData[,x]
}
rm(metaData)

tdm<-TermDocumentMatrix(abstrCorpus)

load("data/LDA_models_current.rda")
#load("data/LDA_FY_models_current.rda")
load("data/beta.tree.rda")

# beta.tree<-lapply(models, function(x) hclust(dist(x@beta, "Hellinger")))
# beta.tree<-lapply(seq_along(beta.tree), function(x) {beta.tree[[x]]$labels<-getTopicNames(x)
#                                                     beta.tree[[x]]})
# save(beta.tree, file = "data/beta.tree.rda")

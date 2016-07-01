library(topicmodels)
library(proxy)
library(ape)
library(arcdiagram)
library(slam)
library(reshape2)

extraFunFile<-"shiny_textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
    source("arcplot.r", keep.source=TRUE)
}

metaData<-read.csv("data/models/modelMetaData.txt", colClasses=c('character','character','Date','character',
                                           'character','numeric','integer'))

load("data/models/LDA_models_current.rda")
load("data/models/beta.tree.rda")
load("data/models/termAssoc.rda")
load("data/Corpus_TDM.rda")
load("data/models/betaTreeEdgeList.rda")

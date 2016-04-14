library(topicmodels)
library(proxy)
library(ape)
library(arcdiagram)
library(slam)

extraFunFile<-"shiny_textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
    source("arcplot.r", keep.source=TRUE)
}

metaData<-read.csv("data/ModelMetaData.txt",colClasses=c('character','character','Date','character','numeric'))

load("data/LDA_models_current.rda")
load("data/beta.tree.rda")
load("data/term.assoc.rda")
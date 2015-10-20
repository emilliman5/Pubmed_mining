library(shiny)
library(tm)
library(reshape2)
library(igraph)
library(ape)
library(arcdiagram)
library(RColorBrewer)
library(riverplot)
library(rCharts)
library(topicmodels)
library(dendextend)
library(wordcloud)

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

#sankey<-function(){}

#network<-function(){}
    
shinyServer(function(input,output) {
    
    currentIds<-reactive({which(meta(abstrCorpus)$FY %in% input$fy)})
    topicNames<-reactive({apply(terms(models[[as.integer(input$topicK)]],4),2,function(z) paste(z,collapse=","))})
    output$text<-renderText({str(models)})
    output$wordcloud<-renderPlot({
     terms<-rowSums(as.matrix(tdm[currentIds(),]))
     wordcloud(names(terms), freq=terms,max.words = 50, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    })
    output$topics<-renderPlot(height=800, width=1400, {
        par(mar=c(20,8,5,2))
        barplot(colSums(models[[as.integer(input$topicK)]]@gamma[currentIds(),]), las=2,names.arg = topicNames(), col=rainbow(10), ylab="Sum of Topic Probability")
    })
})

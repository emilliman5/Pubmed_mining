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
    
    currentIds<-reactive({
        lapply(input$fy, function(x)
            which(meta(abstrCorpus)$FY == x))})
    topicNames<-reactive({apply(terms(models[[as.integer(input$topicK)]],4),2,function(z) paste(z,collapse=","))})    
    output$wordcloud<-renderPlot({
        terms<-rowSums(as.matrix(tdm[,unlist(currentIds())]))
        terms<-terms[order(terms, decreasing = T)]
        wordcloud(names(terms), freq=terms,max.words = input$slider, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    })
    output$topics<-renderPlot(height=800, width=1400, {
        #par(mar=c(20,8,5,2))
        #barplot(colSums(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]), las=2,names.arg = topicNames(), col=rainbow(10), ylab="Sum of Topic Probability")
        gamma<-data.frame(topic=topicNames(), 
                          sum=unlist(lapply(currentIds(), 
                                            function(x) colSums(models[[as.integer(input$topicK)]]@gamma[x,]) )), fy=rep(input$fy, each=models[[as.integer(input$topicK)]]@k))
        p1<-nPlot(sum~topic, group="fy", data=gamma, type="multiBarChart")
        p1$addParams(dom="topics")
        return(p1) 
        })
})

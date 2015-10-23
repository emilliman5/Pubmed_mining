library(shiny)
library(reshape2)
library(RColorBrewer)
library(rCharts)
library(wordcloud)
#library(networkD3)
library(proxy)
library(htmlwidgets)

#sankey<-function(){}

#network<-function(){}
    
shinyServer(function(input,output) {
    
    currentIds<-reactive({
        lapply(input$fy, function(x)
            which(meta(abstrCorpus)$FY == x))})
    topicNames<-reactive({apply(terms(models[[as.integer(input$topicK)]],4),2,function(z) paste(z,collapse=","))})    
    words<-reactive({unlist(strsplit(input$words, "\\s|,|;|\\t"))})
    output$wordcloud<-renderPlot({
        terms<-rowSums(as.matrix(tdm[,unlist(currentIds())]))
        terms<-terms[order(terms, decreasing = T)]
        wordcloud(names(terms), freq=terms,max.words = input$slider, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    })
    output$topics<-renderChart({
        #par(mar=c(20,8,5,2))
        #barplot(colSums(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]), las=2,names.arg = topicNames(), col=rainbow(10), ylab="Sum of Topic Probability")
        gamma<-data.frame(topic=rep(topicNames(),length(input$fy)), 
                          sum=unlist(lapply(currentIds(), 
                                            function(x) colSums(models[[as.integer(input$topicK)]]@gamma[x,]) )), 
                          fy=rep(input$fy, each=models[[as.integer(input$topicK)]]@k))
        p1<-nPlot(sum~topic, group="fy", data=gamma, type="multiBarChart")
        p1$addParams(dom="topics")
        p1$chart(reduceXTicks = FALSE)
        p1$xAxis(rotateLabels=-45)
        return(p1) 
        })
    output$assoc<-renderText({
        findAssocs(tdm[,unlist(currentIds())], input$words, input$corr)
       })
#     output$force<-renderForceNetwork({
#         gamma2<-dist(t(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]), method = "correlation")
#         edges<-melt(as.matrix(gamma2))
#         edges<-edges[edges$Var2>edges$Var1,]
#         colnames(edges)<-c("source","target","value")
#         nodes<-data.frame(name=unique(c(edges$source, edges$target)), size=colSums(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]))
#         forceNetwork(Links = edges, Nodes = nodes, Source = "source", 
#                      Target = "target",Nodesize = "size", Value = "value", 
#                      NodeID = "name", Group = "name", opacity=0.8)
#     })
})

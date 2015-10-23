library(shiny)
library(reshape2)
library(RColorBrewer)
library(rCharts)
library(wordcloud)
library(visNetwork)
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
    
    nodes<-reactive({
      nodes<-data.frame(id=seq(models[[as.integer(input$topicK)]]@k), group=1, size=colSums(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]))
      nodes<-nodes[order(nodes$id),]
      nodes$label<-topicNames()
      })
    
    edges<-reactive({
      edges<-melt(as.matrix(gamma2))
      edges<-edges[edges$Var2>edges$Var1,]
      colnames(edges)<-c("to","from","width")
      edges$length<-100
      edges[edges$value>quantile(edges$value,input$dist),]
    })
    output$force<-renderVisNetwork({
        visNetwork(nodes = nodes(), edges = edges(), width="100%")
    })
})

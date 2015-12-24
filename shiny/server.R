library(shiny)
library(reshape2)
library(RColorBrewer)
library(rCharts)
library(wordcloud)
library(visNetwork)
library(proxy)
library(htmlwidgets)

#sankey<-function(){}

createLink <- function(url, val) {
  sprintf('<a href="%s%s" target="_blank">%s</a>',url, val, val)
}

getTopicNames<-function(K){ apply(terms(models[[as.integer(K)]],4),2,
                                  function(z) paste(z,collapse=","))}    

shinyServer(function(input,output) {
    
    output$pubs<-renderPlot({
        pub<-tapply(meta(abstrCorpus)$FY,meta(abstrCorpus)$FY, length)
        barplot(pub, col="red3", main="Number of Publications by FY")}, height=400, width=800)
    
    output$pubs.q<-renderPlot({
        pub.Q<-tapply(meta(abstrCorpus)$FY.Q,meta(abstrCorpus)$FY.Q, length)
        barplot(pub.Q, col="darkgreen", las=2, main="Number of Publications by FY quarter")}, height=400, width=800)
    
    output$gamma<-renderPlot({
        
    })
    
    fys<-reactive({
        if("ALL" %in% input$fy){
            x<-c(2009,2010,2011,2012,2013,2014,2015)   
        } else{
            x<-input$fy
        }
        x
    })
    currentIds<-reactive({
        if("ALL" %in% input$fy){
            lapply(fys(), function(x)
                which(meta(abstrCorpus)[fileIds(),"FY"] == x))
        }else{
            lapply(fys(), function(x)
                which(meta(abstrCorpus)[fileIds(),"FY"] == x))
        }
    })
    
    fileIds<-reactive({
        inFile<-input$file
        ids<-1:length(abstrCorpus)
        if(is.null(inFile)){
            return(ids)
        } else {
        x<-read.table(inFile$datapath, header=F)
        if(grepl("ES", x[1,])){
            ids<-lapply(x[,1], function(x)
               grep(x, meta(abstrCorpus)[,"GrantID"])) 
        } else{
            ids<-lapply(x[,1], function(x)
                which(meta(abstrCorpus)[,"PMID"] == x)) 
            }
        }
        unlist(ids)
    })
    
    #topicNames<-reactive({apply(terms(models[[as.integer(input$topicK)]],4),2,function(z) paste(z,collapse=","))})    
    words<-reactive({keyword<-tolower(unlist(strsplit(input$words, "\\s|,|;|:|\\t")))
                     keyword[keyword %in% models[[as.integer(input$K)]]@terms]
                    })
    
    output$wordcloud<-renderPlot({
        terms<-rowSums(as.matrix(tdm[,unlist(currentIds())]))
        terms<-terms[order(terms, decreasing = T)]
        wordcloud(names(terms), freq=terms,max.words = input$slider, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    })
    
    output$topics<-renderChart({
        gamma<-data.frame(topic=rep(getTopicNames(input$topicK),length(fys())), 
                          sum=unlist(lapply(currentIds(), 
                                            function(x) colSums(models[[as.integer(input$topicK)]]@gamma[x,]) )), 
                          fy=rep(fys(), each=models[[as.integer(input$topicK)]]@k))
        p1<-nPlot(sum~topic, group="fy", data=gamma, type="multiBarChart")
        p1$addParams(dom="topics")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Sum of Topic Proportion across Corpus")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(bottom=200))
        p1$params$height<-600
        p1$params$width<-1200
        return(p1) 
        })
    
    findassoc<-reactive({
        assoc<-findAssocs(tdm, words(), 0.00001)
        if(length(words())==1){
          data.frame(Source=rep(words()), Target=names(assoc[[1]]), Correlation=assoc[[1]])
          } else{
            do.call(rbind, lapply(words(), function(x) data.frame(Source=x, Target=names(assoc[[x]]), Correlation=assoc[[x]])))
            }
        })
    
    wordAssoc<-reactive({
      findassoc()[findassoc()$Correlation>input$corr,]
      })
    
    output$assoc<-renderDataTable({    
        wordAssoc()
        }, escape=F)
    
    output$keywordTopic <-renderChart({
        betad<-data.frame(topic=rep(getTopicNames(input$K),length(words())), 
                          beta=unlist(lapply(words(), 
                            function(x) models[[as.integer(input$K)]]@beta[,models[[as.integer(input$K)]]@terms==x])), 
                          Term=rep(words(), each=models[[as.integer(input$K)]]@k))
        p1<-nPlot(beta~topic, group="Term", data=betad, type="multiBarChart")
        p1$addParams(dom="keywordTopic")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Beta weight of term per Topic")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(bottom=200))
        p1$params$height<-600
        p1$params$width<-1200
        return(p1) 
    })
       
    nodes<-reactive({
      nodes<-data.frame(id=seq(models[[as.integer(input$topicK)]]@k), group=rep(1, models[[as.integer(input$topicK)]]@k), 
                        value=as.integer(cut(colSums(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]),5)), label=getTopicNames(input$topicK))
      })
    
    makeEdges<-reactive({
      gamma2<-dist(t(models[[as.integer(input$topicK)]]@gamma[unlist(currentIds()),]), method = "cosine")
      edges<-melt(as.matrix(gamma2))
      edges<-edges[edges$Var2>edges$Var1,]
      colnames(edges)<-c("to","from","value")
      edges$length<-50
      edges
      })
    
    edges<-reactive({makeEdges()[makeEdges()$value<quantile(makeEdges()$value,input$dist),]
      })
    
    output$force<-renderVisNetwork({
        visNetwork(nodes = nodes(), edges = edges()) %>% visOptions(highlightNearest = TRUE)
    })
    
    output$papers<-renderDataTable({
      df<-meta(abstrCorpus)[unlist(currentIds()),]
      df$PMID<-createLink("http://www.ncbi.nlm.nih.gov/pubmed/",df$PMID)
      df$Journal<-createLink("http://www.issn.cc/",df$Journal)
      df
    }, escape=FALSE)
    
#     output$dendroarc<-renderPlot({
#         
#         
#     })
#     output$sankey<-renderChart({
#       sankeyPlot<-rCharts$new()
#       sankeyPlot$setLib("./d3/rCharts_d3_sankey-gh-pages/")
#       sankeyPlot$setTemplate(script="./d3/rCharts_d3_sankey-gh-pages/layouts/chart.html")
#       sankeyPlot$set(
#         data=edgelist,
#         nodewidth=10,
#         nodePadding=10,
#         layout=32,
#         width=1200,
#         height=1200)
#       
#       sankeyPlot$print(chartId="sankey1")
#       sankeyPlots
#     })
})

library(shiny)
library(reshape2)
library(RColorBrewer)
library(rCharts)
library(wordcloud)
library(visNetwork)
library(proxy)

createLink <- function(url, val) {
  sprintf('<a href="%s%s" target="_blank">%s</a>',url, val, val)
}

topicLength<-function(K){ models[[as.integer(K)]]@k }
getTopicNames<-function(K){ apply(terms(models[[as.integer(K)]],4),2,
                                  function(z) paste(z,collapse=","))}    
dict<-rownames(tdm)

shinyServer(function(input,output) {
         
    text<-reactive({
        input$submit
        x<-isolate(input$abstract)
        makeCorpus(x)        
    })
    
    dtm<-reactive({
        DocumentTermMatrix(text())
    })
    
    posteriors<-reactive({
        posterior(models[[as.integer(input$Ktopic)]], newdata=dtm())
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
        ids<-lapply(fys(), function(x)
                which(meta(abstrCorpus)[,"FY"] == x))
        lapply(ids, function(x) x[x %in% fileIds()])
    })
    
    fileIds<-reactive({
        inFile<-input$file
        ids<-1:length(abstrCorpus)
        if(is.null(inFile)){
            return(ids)
        } else {
        x<-read.table(inFile$datapath, header=F)
        if(grepl("ES", x[1,])){
            ids<-lapply(x[,1], function(z)
               grep(z, meta(abstrCorpus)[,"GrantID"])) 
        } else{
            ids<-lapply(x[,1], function(x)
                which(meta(abstrCorpus)[,"PMID"] == x)) 
            }
        }
        unlist(ids)
    })
    
    words<-reactive({keyword<-tolower(unlist(strsplit(input$words, "\\s|,|;|:|\\t")))
                     keyword[keyword %in% dict]
                    })
    
    terms<-reactive({
        terms<-rowSums(as.matrix(tdm[,unlist(currentIds())]))
        terms[order(terms, decreasing = T)]
    })
    
    output$pubs<-renderPlot({
        pub<-tapply(meta(abstrCorpus)$FY,meta(abstrCorpus)$FY, length)
        barplot(pub, col="red3", main="Number of Publications by FY")}, height=400, width=800)
    
    output$pubs.q<-renderPlot({
        pub.Q<-tapply(meta(abstrCorpus)$FY.Q,meta(abstrCorpus)$FY.Q, length)
        barplot(pub.Q, col="darkgreen", las=2, main="Number of Publications by FY quarter")}, height=400, width=800)
    
    output$wordcloud<-renderPlot({
        wordcloud(names(terms()), freq=terms(),max.words = input$slider, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    },height = 600)
    
    output$topics<-renderChart({
        gamma<-data.frame(topic=rep(getTopicNames(input$topicK),length(fys())), 
                          sum=unlist(lapply(currentIds(), 
                                            function(x) 100*(colSums(models[[as.integer(input$topicK)]]@gamma[x,])/length(x)) )), 
                          fy=rep(fys(), each=models[[as.integer(input$topicK)]]@k))
        p1<-nPlot(sum~topic, group="fy", data=gamma, type="multiBarChart")
        p1$addParams(dom="topics")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Sum of Topic Proportion across Corpus")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=125,bottom=240))
        p1$params$height<-600
        p1$params$width<-22*topicLength(input$topicK)
        return(p1) 
        })
    
    findassoc<-reactive({
        assoc<-findAssocs(tdm, words(), 0.000001)
        if(length(words())==1){
          data.frame(Source=rep(words()), Target=names(assoc[[1]]), Correlation=assoc[[1]])
          } else{
            do.call(rbind, lapply(words(), function(x) data.frame(Source=x, Target=names(assoc[[x]]), Correlation=assoc[[x]])))
            }
        })
    
    wordAssoc<-reactive({
      x<-findassoc()[findassoc()$Correlation>input$corr,]
      if(length(words())>1){
        rbind(x, do.call(rbind, lapply(words(), function(z) findassoc()[(findassoc()$Target==z && findassoc()$Source!=z), ])))
      }
      return(x)
      })
    
    output$assoc<-renderDataTable({    
        wordAssoc()
        },escape=F)
    
    output$keywordTopic <-renderChart({
        betad<-data.frame(topic=rep(getTopicNames(input$K),length(words())), 
                          beta=10**unlist(lapply(words(), 
                            function(x) models[[as.integer(input$K)]]@beta[,models[[as.integer(input$K)]]@terms==x])), 
                          Term=rep(words(), each=models[[as.integer(input$K)]]@k))
        p1<-nPlot(beta~topic, group="Term", data=betad, type="multiBarChart")
        p1$addParams(dom="keywordTopic")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Beta weight of term per Topic",tickFormat = "#! function(d) {return d3.format(',.5f')(d)} !#")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=125, bottom=240))
        p1$params$height<-600
        p1$params$width<-22*topicLength(input$K)
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
    
    edges<-reactive({
        makeEdges()[makeEdges()$value<edgeThresh(),]
      })
    
    edgeThresh<-reactive({
        quantile(makeEdges()$value,input$dist)
    })
    
    output$text<-renderText({
        x<-edgeThresh()
        paste("Absolute distance threshold: ",x, sep = "")
    })
    
    output$classify<-renderChart({
        p<-data.frame(topicProb=as.vector(posteriors()[["topics"]]), topics=getTopicNames(input$Ktopic), color=1)
        p<-p[order(p$topicProb,decreasing = T),]
        p1<-nPlot(topicProb~topics, data=p, type="multiBarChart", color="color")
        p1$addParams(dom="classify")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Topic Probabilty (gamma)",tickFormat = "#! function(d) {return d3.format(',.5f')(d)} !#")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=125, bottom=240))
        p1$params$height<-600
        p1$params$width<-18*topicLength(input$Ktopic)
        return(p1) 
    })
    
    output$force<-renderVisNetwork({
        visNetwork(nodes = nodes(), edges = edges()) %>% visOptions(highlightNearest = TRUE)
    })
    
    output$papers<-renderDataTable({
      df<-meta(abstrCorpus)[unlist(currentIds()),]
      df$PMID<-createLink("http://www.ncbi.nlm.nih.gov/pubmed/",df$PMID)
      df$Journal<-createLink("http://www.issn.cc/",df$Journal)
      df
    },options = list(autoWidth = FALSE,
                columnDefs = list(list(width = '25px', targets = "_all")
                    )
        ), escape=FALSE)
})

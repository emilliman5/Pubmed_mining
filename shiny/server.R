library(shiny)
library(reshape2)
library(RColorBrewer)
library(rCharts)
library(wordcloud)
library(visNetwork)
library(proxy)
library(ape)
 
dict<-rownames(tdm)
limits<-list(c(0.02,0.99),
             c(0.01,0.99),
             c(0.005,0.995),
             c(0.0025,0.9975),
             c(0.00125,0.99875),
             c(0.0006,0.9994))

shinyServer(function(input,output, session) {
         
    text<-reactive({
        if(input$submit==0){
            return()
        }
        x<-isolate(input$abstract)
        z<-makeCorpus(x)
        DocumentTermMatrix(z)
    })
    
    posteriors<-reactive({
        input$submit
        isolate(posterior(models[[as.integer(input$Ktopic)]], newdata=text()))
    })
    
    currentIds<-reactive({
        ids<-lapply(fys(input$fy), function(x)
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
    
    output$pubs<-renderChart({
        pub.Q<-tapply(meta(abstrCorpus)$FY.Q,meta(abstrCorpus)$FY.Q, length)
        pubs<-data.frame(FY=floor(as.numeric(names(pub.Q))), 
                         Quarter=paste0("Q",as.numeric(unlist(lapply(strsplit(as.character(names(pub.Q)), "\\."), 
                                                   function(x) x[2])))), 
                          count=as.integer(pub.Q))
        pubs<-pubs[pubs$FY>2008,]
        p1<-nPlot(count~FY, group="Quarter", data=pubs, type="multiBarChart")
        p1$addParams(dom="pubs")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Number of Publications",tickFormat = "#! function(d) {return d3.format(',.0f')(d)} !#")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=125))
        p1$params$height<-500
        p1$params$width<-1000
        return(p1) 
        })
    
    output$wordcloud<-renderPlot({
        wordcloud(names(terms()), freq=terms(),max.words = input$slider, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order = F)
    },height = 600)
    
    output$topics<-renderChart({
        gamma<-data.frame(topic=rep(getTopicNames(input$topicK),length(fys(input$fy))), 
                          sum=unlist(lapply(currentIds(), 
                                            function(x) colSums(t(apply(models[[as.integer(input$topicK)]]@gamma[x,], 1, 
                                                                      function(z) z*(z>=z[order(z,decreasing = T)][5])) )))), 
                          fy=rep(fys(input$fy), each=models[[as.integer(input$topicK)]]@k))
        p1<-nPlot(sum~topic, group="fy", data=gamma, type="multiBarChart")
        p1$addParams(dom="topics")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Topic Probability",tickFormat = "#! function(d) {return d3.format(',.0f')(d)} !#")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=100,bottom=240))
        p1$params$height<-600
        p1$params$width<-22*topicLength(input$topicK)
        return(p1) 
        })
    
    findassoc<-reactive({
        assoc<-findAssocs(tdm, words(), 0.01)
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
    
    posterior.dist<-reactive({
      x<-dist(models[[as.integer(input$Ktopic)]]@gamma, matrix(posteriors()[["topics"]], nrow=1,byrow = T), method="cosine")
      cbind(meta(abstrCorpus)[order(x),], Distance=x[order(x)])
    })
    
    output$closestPubs<-renderDataTable({
      posterior.dist()
    })
    
    output$classify<-renderChart({
        p<-data.frame(topicProb=as.vector(posteriors()[["topics"]]), topics=getTopicNames(input$Ktopic), color=1)
        p<-p[order(p$topicProb,decreasing = T),]
        p1<-nPlot(topicProb~topics, data=p, type="multiBarChart", color="color")
        p1$addParams(dom="classify")
        p1$chart(reduceXTicks = FALSE)
        p1$yAxis(axisLabel="Topic Probabilty (gamma)",tickFormat = "#! function(d) {return d3.format(',.3f')(d)} !#")
        p1$xAxis(rotateLabels=-45)
        p1$chart(margin=list(left=125, bottom=240))
        p1$params$height<-600
        p1$params$width<-20*topicLength(input$Ktopic)
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
                    ), 
    escape=FALSE)
    
    output$dendroArc<-renderPlot({   
        dendroArc(FYs = fys(input$fy), modelK = as.integer(input$treeK),
                  distThresh = input$treeDist, ids=currentIds(), 
                  betaTree = beta.tree[[as.integer(input$treeK)]][[as.integer(input$topicTree)]],
                  y_lim=limits[[as.integer(input$treeK)]],topicN = as.integer(input$topicN), 
                  distFun = input$proxy, gamma = 0.15)
    })
    
    output$dendroArc.ui<-renderUI({
      plotOutput("dendroArc", height=paste0(topicLength(input$treeK)*18, "px"))
    })
    
    topicChoices<-reactive({
      z<-lapply(1:topicLength(input$treeK), function(x) x)
      names(z)<-getTopicNames(input$treeK)
      return(z)
      })
    
    observe({
      # This will change the value of input$partnerName to searchResult()[,1]
      updateSelectInput(session, "topicN", 
                      label = "Anchor Topic:", 
                      choices = topicChoices())
    })
    
    gammaDistRange<-reactive({
      ids=currentIds()
      topicGamma<-as.matrix(models[[as.integer(input$treeK)]]@gamma)
      values<-unlist(lapply(ids, function(x) dist(x=t(topicGamma[x,-as.integer(input$topicN)]),y=t(topicGamma[x,as.integer(input$topicN)]) , method=input$proxy)))
      return(c(range(values), as.numeric(quantile(values, 0.9))))
      })

    observe({
      updateSliderInput(session, "treeDist",
                      min=gammaDistRange()[1],
                      max=gammaDistRange()[2],
                      value=gammaDistRange()[3])
      })
})

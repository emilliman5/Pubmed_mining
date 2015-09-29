getDate<-function()
{
  ## Purpose is to define a data in the format
  ## 2015may05_1631 (YYYYmmDD_HHmm)
  tolower(format(Sys.time(), "%Y%b%d_%H%M"));
}

timeStamp<-function()
{
    gsub("-| |:", "",Sys.time())
}

stemCompletion2 <- function(x, dictionary) 
{
  
  x <- unlist(strsplit(as.character(x), " "))  
  x <- x[x != ""]  
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ") 
  PlainTextDocument(stripWhitespace(x))
  
}

toSpace<-content_transformer(function(x, pattern, perl=F) gsub(pattern, " ",perl=perl, x))

NgramTokenizer <-function(x,n=2)
{
  unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
}

stemCompletionF<-function (x, dictionary, 
          type = c("prevalent", "first", "longest", "none", "random", "shortest")) 
{
 
  if (inherits(dictionary, "Corpus")) 
    dictionary <- unique(unlist(lapply(dictionary, words)))
  type <- match.arg(type)
  y<-unique(x)
  possibleCompletions <- lapply(y, function(w) grep(sprintf("^%s", 
                                                            w), dictionary, value = TRUE))
  switch(type, first = {
    setNames(sapply(possibleCompletions, "[", 1), x)
  }, longest = {
    ordering <- lapply(possibleCompletions, function(x) order(nchar(x), 
                                                              decreasing = TRUE))
    possibleCompletions <- mapply(function(x, id) x[id], 
                                  possibleCompletions, ordering, SIMPLIFY = FALSE)
    setNames(sapply(possibleCompletions, "[", 1), x)
  }, none = {
    setNames(x, x)
  }, prevalent = {
    possibleCompletions <- lapply(possibleCompletions, function(x) sort(table(x), 
                                                                        decreasing = TRUE))
    n <- structure(names(sapply(possibleCompletions, "[", 1)))
    n[match(x,names(n))]
    #setNames(if (length(n)) n else rep(NA, length(x)), x)
  }, random = {
    setNames(sapply(possibleCompletions, function(x) {
      if (length(x)) sample(x, 1) else NA
    }), x)
  }, 
  shortest = {
    ordering <- lapply(possibleCompletions, function(x) order(nchar(x)))
    possibleCompletions <- mapply(function(x, id) x[id], 
                                  possibleCompletions, ordering, SIMPLIFY = FALSE)
    setNames(sapply(possibleCompletions, "[", 1), x)
  })
}

wordCloudMontage<-function(tdm, file="wordcloud.png", path=resultsPath, dim=c(4,6), f=1)
{  
    png(paste(resultsPath, file,sep="/"), height=3500, width=6000, units="px")
    layout(matrix(seq(1,prod(dim)), nrow=dim[1]), heights=c(1,8,1,8))
    par(mar=rep(0,4))
    sapply(colnames(as.matrix(tdm)), function(x){ 
      plot.new()
      text(x=0.5, y=0.5, x, cex=8)
      wordcloud(row.names(as.matrix(tdm)),min.freq=f, as.matrix(tdm)[,x], scale=c(10,0.5), 
                colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
          }
      )
    dev.off()
}

hclustgraph<-function(tdm=tdm, file="hierarchicalCluster.png", path=resultsPath, s=0.9)
{
  tdm2<-removeSparseTerms(tdm, sparse =s)
  tdm2.m<-as.matrix(tdm2)
  dtm2<-t(as.matrix(tdm2))
  
  row.names(dtm2)<-docs[,1]
  dist.w<-dist(scale(tdm2))
  dist.d<-dist(scale(dtm2))
  fit.w<-hclust(dist.w, method = "ward.D")
  fit.d<-hclust(dist.d, method = "ward.D")
  
  png(paste0(resultsPath, file), height=800, width=1200, units="px")
  par(mfrow=c(2,1))
  plot(fit.d)
  plot(fit.w, cex=0.75)
  dev.off()
}

tfidfHisto<-function(tdm, fact, fun)
  {
  f<-unique(meta(abstrCorpus)[,fact])
  x<-lapply(f, function(x) which(meta(abstrCorpus)[,fact]==x) )
  
  lapply(x, function(y){
    f<-meta(abstrCorpus)[y[1],fact]
    m<-as.matrix(tdm)[rowSums(as.matrix(tdm)[,y]),y]
    s<-apply(m, 1, fun)
    m<-as.matrix(tdm[,y])
    png(file.path(resultsPath,paste0(fact,"_",f, "_TermFreqxIDF_Distributions.png"), fsep = "/"), 
        height=800, width=1200, units="px")
    par(mfrow=c(2,1), cex=2)
    hist(as.vector(m)[as.vector(m)>0], breaks=100, col="blue4", main=paste(fact, f, "Distribution of tf-idf Scores in Corpus", collpase= " "))
    hist(s, breaks=100, col="red", main=paste0(fact, " ", f, "Distribution of ",fun, " tf-idf scores per term in Corpus"))  
    dev.off()
  })
}

tfHisto<-function(tdm, fact)
{
  f<-unique(meta(abstrCorpus)[,fact])
  x<-lapply(f, function(x) which(meta(abstrCorpus)[,fact]==x) )
  
  lapply(x, function(y){
    f<-meta(abstrCorpus)[y[1],fact]
    s<-rowMeans(as.matrix(tdm[,y]))
    s<-s[s>0]
    m<-rowSums(as.matrix(tdm[,y]))
    m<-m[m>0]
    png(file.path(resultsPath,paste(fact,f, "TermFreq_Distributions.png", sep="_"), fsep = "/"), 
        height=800, width=1200, units="px")
    par(mfrow=c(2,1), cex=2)
    hist(m, breaks=100, col="blue4", main=paste(fact, f,"Distribution of term Freq in Corpus", collapse=" "), xlab="Number of Occurences")
    hist(s, breaks=100, col="red", main=paste(fact, f, "Distribution of Avg. term freq. per Document in Corpus", collapse=" "))  
    dev.off()
  })
}

getFactorIdx<-function(col, df){
    f<-unique(df[,col])
    x<-lapply(f, function(x) which(df[,col]==x) )
    names(x)<-f
    x
}

wordCloud<-function(tdm, fact, maxWords, fun="sum", pre="", scale=c(10,0.5))
{
  fun<-tolower(fun)
  f<-unique(meta(abstrCorpus)[,fact])
  x<-lapply(f, function(x) which(meta(abstrCorpus)[,fact]==x) )
  
  lapply(x, function(y) {
    f<-meta(abstrCorpus)[y[1],fact]
    tdm.df<-data.frame(word=rownames(tdm), freq=apply(as.matrix(tdm)[,y], 1, fun))
    tdm.df<-tdm.df[order(tdm.df$freq,decreasing = T),]
    if(tdm.df[maxWords,"freq"]==0){
      low=1
    } else{
      low=tdm.df[maxWords,"freq"]
    }
#     low<-quantile(rowSums(as.matrix(tdm[,y])), probs = 0.99)
    
    png(paste0(resultsPath,"/",paste(pre,fact,f,"wordCloud.png", sep="_")), height=1600, width=1600, units="px")
    layout(matrix(c(1,2), nrow=2), heights=c(0.25,3))
    par(mar=rep(0,4))
    plot.new()
    text(0.5, 0.5, paste(pre, fact,f, collapse = " "), cex=4)
    wordcloud(tdm.df$word, tdm.df$freq, scale=scale, min.freq = low, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
    dev.off()
  })
}

getTopicAssign<-function(ids, model, corpus){
    idx<-do.call(c, lapply(ids, function(x){
        which(meta(corpus)[-484,]$PMID==x)
    }))
    t<-as.data.frame(model@gamma[idx,])
        colnames(t)<-apply(terms(model,4),2,function(z) paste(z,collapse=","))
        rownames(t)<-ids
    t
}

dendroArc<-function(FYs=c(2009,2012,2015), model, topicN, distFun="cosine",
                    gamma=0.15, distThresh=0.94)
{
    library(ape)
    library(arcdiagram)
    ##FYs = the two fiscal years to compare
    ##model = the topic model object to use
    ##topicN = the topic to center the analysis on, i.e. only show arcs that are connected to this topic
    
    ##1: Make a dendrogram of topic-topic relationships using terms
    topicTerms<-as.matrix(model@beta)
    colnames(topicTerms)<-model@terms
    rownames(topicTerms)<-apply(terms(model,4),2,function(z) paste(z,collapse=","))
    topicTermsTree<-hclust(dist(topicTerms, method=distFun))
    
    topic<-rownames(topicTerms)[topicN]
    ##1: Calculate distances between topics by document assigment
    fyIdx<-lapply(FYs, function(x) meta(abstrCorpus)[,"FY"]==x)
    topicGamma<-as.matrix(model@gamma)
    colnames(topicGamma)<-apply(terms(model,4),2,function(z) paste(z,collapse=","))
    topicGammaDist<-lapply(fyIdx, function(x) {
        dist(x=t(topicGamma[x,-topicN]),y=t(topicGamma[x,topicN]) , method=distFun)
    })
    
    edges<-lapply(topicGammaDist, function(x){
        e<-x[x<=distThresh,]
        cbind(names(e),topic)
    })
    
    degrees<-lapply(fyIdx, function(x){
        colSums(topicGamma[x,]>=gamma)        
    })
    order<-rownames(topicTerms)[as.numeric(gsub("Topic ", "", names(topicTermsTree$labels[topicTermsTree$order])))]
    sizes<-as.numeric(cut(rowSums(do.call(cbind, degrees)),10))[topicTermsTree$order]
    lab<-order
    pal<-rainbow(7)[c(1,3,6)]
    edge.col<-do.call(c, lapply(seq_along(edges), function(x) rep(pal[x], length(edges[[x]][,1]))))
    edge.weight<-do.call(c, lapply(rev(seq_along(edges)), function(x) rep(x/2+.5, length(edges[[x]][,1]))))
    edges<-do.call(rbind, edges)
    png(paste0(resultsPath, "/DendroArcs_Topic",topicN,"_",paste(FYs, collapse="and"),"_",gsub("-| |:", "",Sys.time()),".png"),height=1200, width=800, units="px")
    par(mfcol=c(1,2))
    plot(as.phylo(topicTermsTree),show.tip.label=F, main="Topic-Topic relationship by Terms")
    arcplot(edges,vertices = lab, pch=21,cex.labels=0.75,
            col.arcs=edge.col,main=paste("FY",paste(FYs, collapse=" and ")), cex.nodes = sizes,
            ylim=c(0.01,.99),col.labels="black",lwd.arcs=edge.weight, ordering=order, 
            horizontal=F,col.nodes="black", font=0)
    legend("topright", lty=1,lwd=2,cex=1.25, col=pal, legend = FYs, bty="n")
    dev.off()   
}

dist2Table<-function(x){
    library(reshape2)
    t<-melt(x[1:dim(x)[1],1:dim(x)[2]], varnames=c("col","row"))   
    t   
}

findTerms<-function(corpus,tdm, terms){
    ###Finds terms and reports freq by FY
    library(tm)
    f<-getFactorIdx(5, meta(corpus))
    x<-lapply(terms, function(x,z=as.matrix(tdm)[rownames(tdm)==x,]) 
        lapply(f, function(y) sum(z[y])))
    names(x)<-terms
    x
}
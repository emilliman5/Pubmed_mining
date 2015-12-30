library(tm)
library(parallel)
makeCorpus<-function(text, cores=4){
           
    df.Corpus<-Corpus(VectorSource(text))
    
    df.Corpus<-tm_map(df.Corpus, content_transformer(tolower), mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, toSpace, "/|@|\\||-|_|\\\\", mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, removePunctuation, mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, removeNumbers, mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, toSpace, "[^[:alnum:] ]", perl=T, mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, toSpace, "[\\s\\t][A-z]{1,2}[\\s\\t]", perl=T, mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, stemDocument, mc.cores=cores)
    df.Corpus<-tm_map(df.Corpus, stripWhitespace, mc.cores=cores)
    df.Corpus<-mclapply(df.Corpus, stemCompletion2, dictionary=abstrCorpus, mc.cores=cores)
    df.Corpus<-Corpus(VectorSource(df.Corpus))
    return(df.Corpus)
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
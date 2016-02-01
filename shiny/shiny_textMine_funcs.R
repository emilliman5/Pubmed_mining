library(tm)
library(parallel)

createLink <- function(url, val) {
    sprintf('<a href="%s%s" target="_blank">%s</a>',url, val, val)
}

fys<-function(z)
  {
  if("ALL" %in% z){
    x<-c(2009,2010,2011,2012,2013,2014,2015)   
  } else{
    x<-z
  }
  x
}

topicLength<-function(K){ models[[as.integer(K)]]@k }

getTopicNames<-function(K){ apply(terms(models[[as.integer(K)]],4),2,
                                  function(z) paste(z,collapse=","))}   

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

getTopicAssign<-function(ids, model, corpus){
    idx<-do.call(c, lapply(ids, function(x){
        which(meta(corpus)[-484,]$PMID==x)
    }))
    t<-as.data.frame(model@gamma[idx,])
        colnames(t)<-apply(terms(model,4),2,function(z) paste(z,collapse=","))
        rownames(t)<-ids
    t
}

dendroArc<-function(FYs, modelK, topicN, ids, distFunc, gamma=0.15, distThresh, betaTree, y_lim)
{
    ##FYs = the two fiscal years to compare
    ##model = the topic model object to use
    ##topicN = the topic to center the analysis on, i.e. only show arcs that are connected to this topic

    ##1: Calculate distances between topics by document assigment
    topicGamma<-as.matrix(models[[as.integer(modelK)]]@gamma)
    colnames(topicGamma)<-getTopicNames(modelK)
    topicGammaDist<-lapply(ids, function(x) {
        dist(x=t(topicGamma[x,-topicN]),y=t(topicGamma[x,topicN]) , method=distFunc)
    })
    
    edges<-lapply(topicGammaDist, function(x){
        e<-x[x<=distThresh,]
        cbind(names(e), getTopicNames(modelK)[topicN])
    })
    
    degrees<-lapply(ids, function(x){
        colSums(topicGamma[x,]>=gamma)        
    })
    
    order<-getTopicNames(modelK)[betaTree$order]
    sizes<-as.numeric(cut(rowSums(do.call(cbind, degrees)),10))[betaTree$order]
    pal<-rainbow(7)[1:length(FYs)]
    edge.col<-do.call(c, lapply(seq_along(edges), function(x) rep(pal[x], length(edges[[x]][,1]))))
    edge.weight<-do.call(c, lapply(rev(seq_along(edges)), function(x) rep(x/2+.5, length(edges[[x]][,1]))))
    edges<-do.call(rbind, edges)
    
    par(mfcol=c(1,2))
    plot(as.phylo(betaTree),show.tip.label=F)
    arcplot(edges, vertices = order, pch=21, cex.labels=1,
            col.arcs=edge.col, main=paste("FY",paste(FYs, collapse=" and ")), cex.nodes = sizes,
            ylim=y_lim, col.labels="black",lwd.arcs=edge.weight, ordering=order, 
            horizontal=F,col.nodes="black")
    legend("topright", lty=1,lwd=2,cex=1.25, col=pal, legend = FYs, bty="n")  
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
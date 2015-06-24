getDate<-function()
{
  ## Purpose is to define a data in the format
  ## 2015may05_1631 (YYYYmmDD_HHmm)
  tolower(format(Sys.time(), "%Y%b%d_%H%M"));
}

stemCompletion2 <- function(x, dictionary) 
{
  
  x <- unlist(strsplit(as.character(x), " "))  
  x <- x[x != ""]  
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ") 
  PlainTextDocument(stripWhitespace(x))
  
}

toSpace<-content_transformer(function(x, pattern) gsub(pattern, " ", x))

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
    png(paste0(resultsPath, file), height=3500, width=6000, units="px")
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
    
    png(paste0(resultsPath,"/",paste(pre,fact,f,"_wordCloud.png", sep="_")), height=1600, width=1600, units="px")
    layout(matrix(c(1,2), nrow=2), heights=c(0.25,3))
    par(mar=rep(0,4))
    plot.new()
    text(0.5, 0.5, paste(pre, fact,f, collapse = " "), cex=4)
    wordcloud(tdm.df$word, tdm.df$freq, scale=scale, min.freq = low, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
    dev.off()
  })
}





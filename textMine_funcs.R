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

hclustgraph<-function(tdm=tdm, file="hierarchicalCluster.png", path=resultsPath, s=0.9){
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
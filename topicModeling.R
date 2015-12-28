library(parallel)
library(proxy)

idx<-getFactorIdx("FY",meta(abstrCorpus))
idx<-do.call(c, lapply(idx[-8], function(x) sample(x, 500)))

test.corpus<-abstrCorpus[idx]
test.dtm<-DocumentTermMatrix(test.corpus)

dtm<-t(t(as.matrix(test.dtm))[as.vector(apply(t(as.matrix(test.dtm)), 1, sum)>10),])

seq.k<-seq(2,1000,25)

best.model<-mclapply(seq.k,mc.cores = 8, function(k) LDA(test.dtm, k) )
best.model.lglk<-as.data.frame(as.matrix(lapply(best.model, logLik)))
LogLik.df<-data.frame("topics"=seq.k, 
                    "LL"=as.numeric(as.matrix(best.model.lglk)))

png(paste(resultsPath,"LDA_topicNumber_optimziation.png", sep="/"), height=1200, width=1200, units="px")
plot(LogLik.df$LL~LogLik.df$topics, pch=19, col="red", main="LDA Simulation with 10 docs per FY")
dev.off()

topTermBeta<-lapply(best.model, function(x){
    y<-as.matrix(x@beta)
    colnames(y)<-x@terms
    rownames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topTermsDist<-lapply( topTermBeta, function(x) {
    dist(x,method = "cosine")
})

names(topTermsDist)<-lapply(best.model, function(x) x@k)
lapply(names(topTermsDist)[-1], function(x){
    png(paste0(resultsPath,"/","TopicClustering_byTerms_TopicNumber_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topTermsDist[[x]]), cex=1)
    dev.off()
})

topDocGamma<-lapply(best.model, function(x) {
    y<-as.matrix(x@gamma)
    colnames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topDocDist<-lapply(topDocGamma, function(x){
    dist(t(x),method="cosine")
})
names(topDocDist)<-lapply(best.model, function(x) x@k)

lapply(names(topDocDist)[-1], function(x){
    png(paste0(resultsPath,"/","TopicClustering_byDoc_TopicNumber_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topDocDist[[x]]), cex=1)
    dev.off()
})

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

topTermBeta<-as.matrix(best.model[[7]]@beta)
colnames(topTermBeta)<-best.model[[7]]@terms
rownames(topTermBeta)<-apply(terms(best.model[[7]],3),2,function(x) paste(x,collapse=","))

topTermsDist<-dist(topTermBeta,method = "cosine")

png(paste(resultsPath,"TopicClustering_byTerms.png", sep="/"), height=1200, width=2400, units="px")
plot(hclust(topTermsDist), cex=1)
dev.off()

topDocGamma<-as.matrix(best.model[[7]]@gamma)
colnames(topDocGamma)<-rownames(topTermBeta)

topDocDist<-dist(t(topDocGamma),method="cosine")

png(paste(resultsPath,"TopicClustering_byDoc.png", sep="/"), height=1200, width=2400, units="px")
plot(hclust(topDocDist), cex=1)
dev.off()


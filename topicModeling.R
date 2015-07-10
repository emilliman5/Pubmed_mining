library(parallel)

idx<-getFactorIdx("FY",meta(abstrCorpus))
idx<-do.call(c, lapply(idx, function(x) sample(x, 100)))

test.corpus<-abstrCorpus[idx]
test.dtm<-DocumentTermMatrix(test.corpus)

dtm<-t(t(as.matrix(test.dtm))[as.vector(apply(t(as.matrix(test.dtm)), 1, sum)>10),])

seq.k<-seq(2,200,10)

best.model<-mclapply(seq.k,mc.cores = 10, function(k) LDA(test.dtm, k) )
best.model.lglk<-as.data.frame(as.matrix(lapply(best.model, logLik)))
LogLik.df<-data.frame("topics"=seq.k, 
                    "LL"=as.numeric(as.matrix(best.model.lglk)))

png(paste(resultsPath,"LDA_topicNumber_optimziation.png", sep="/"), height=1200, width=1200, units="px")
plot(LogLik.df$LL~LogLik.df$topics, pch=19, col="red", main="LDA Simulation with 10 docs per FY")
dev.off()

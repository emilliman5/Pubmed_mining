library(parallel)

idx<-getFactorIdx("FY",meta(abstrCorpus))
idx<-do.call(c, lapply(idx, function(x) sample(x, 10)))

test.corpus<-abstrCorpus[idx]
test.dtm<-DocumentTermMatrix(test.corpus)

best.model<-mclapply(seq(2,200,2),mc.cores = 30, function(k) LDA(test.dtm, k) )
best.model.lglk<-as.data.frame(as.matrix(lapply(best.model, logLik)))
LogLik.df<-data.frame("topics"=seq(2,200,2), 
                    "LL"=as.numeric(as.matrix(best.model.lglk)))



sp.dtm<- DocumentTermMatrix(spCorpus)
lda.sp<-mclapply(seq(2,30,1), function(k) LDA(sp.dtm, k), mc.cores = 30)
best.lda<-as.data.frame(as.matrix(lapply(lda.sp, logLik)))
best.lda<-data.frame(topics=seq(2,30,1), LL=as.numeric(as.matrix(best.lda)))

png(paste(resultsPath,"LDA_topicNumber_optimziation.png", sep="/"), height=1200, width=1200, units="px")
par(mfrow=c(2,1))
plot(best.lda$LL~best.lda$topics,pch=19, col="blue", main="LDA Simulation with Strategic Plan")
plot(LogLik.df$LL~LogLik.df$topics, pch=19, col="red", main="LDA Simulation with 10 docs per FY")
dev.off()

harmonicMean <- function(logLikelihoods, precision=2000L) {
    library("Rmpfr")
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
}

fitted<-mclapply(seq(2,200,1), mc.cores = 30, function(x) LDA(test.dtm, 
    k=x,method="Gibbs", control=list(iter=100,burnin=100, keep=10)))
logLiks_many<-lapply(fitted, function(L) L@logLiks)


sp.dtm<- DocumentTermMatrix(spCorpus)
fitted<-mclapply(seq(2,50,1), mc.cores = 30, 
                 function(x) LDA(sp.dtm, 
                             k=x,method="Gibbs", control=list(iter=100,burnin=100, keep=10)))
logLiks_many<-lapply(fitted, function(L) L@logLiks)

sp.hm<-sapply(logLiks_many, function(h) harmonicMean(h))

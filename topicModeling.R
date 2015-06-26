library(parallel)

harmonicMean <- function(logLikelihoods, precision=2000L) {
    library("Rmpfr")
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
}


idx<-getFactorIdx("FY",meta(abstrCorpus))
idx<-do.call(c, lapply(idx, function(x) sample(x, 10)))

test.corpus<-abstrCorpus[idx]
test.dtm<-DocumentTermMatrix(test.corpus)

best.model<-mclapply(seq(2,200,2), function(k) LDA(test.dtm, k) )


fitted<-mclapply(seq(2,200,1), mc.cores = 30, function(x) LDA(test.dtm, 
    k=x,method="Gibbs", control=list(iter=100,burnin=100, keep=10)))
logLiks_many<-lapply(fitted, function(L) L@logLiks)


sp.dtm<- DocumentTermMatrix(spCorpus)
fitted<-mclapply(seq(2,50,1), mc.cores = 30, 
                 function(x) LDA(sp.dtm, 
                             k=x,method="Gibbs", control=list(iter=100,burnin=100, keep=10)))
logLiks_many<-lapply(fitted, function(L) L@logLiks)

sp.hm<-sapply(logLiks_many, function(h) harmonicMean(h))

getTopicAssign<-function(ids, model, corpus){
    idx<-do.call(c, lapply(ids, function(x){
        which(meta(corpus)[-484,]$PMID==x)
    }))
    t<-model@gamma[idx,]
    colnames(t)<-apply(terms(model,4),2,function(z) paste(z,collapse=","))
    t
}
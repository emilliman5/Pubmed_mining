library(tm)
library(topicmodels)
library(proxy)
library(docopt)
library(Matrix)

load("data/LDA_FY_models_current.rda")
load("data/LDA_models_current.rda")

abstrCorpus<-Corpus(DirSource("data/Corpus/"), readerControl = list(language="english"))
tdm<-TermDocumentMatrix(abstrCorpus)
write_stm_CLUTO(tdm, "data/Corpus_tdm.cluto")

beta.tree<-lapply(models, function(x) lapply( c("cosine", "Hellinger"),function (z) hclust(dist(x@beta, z))))
beta.tree<-lapply(models, function(x) hclust(dist(x@beta, "cosine")))
save(beta.tree, file = "data/beta.tree.rda")

metaData<-read.csv("data/CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
grantIDs<-strsplit(metaData$GrantID, "\\|")
grants.table<-data.frame(PMID=rep(metaData$PMID, 
                                   sapply(grantIDs, length)), 
                                   grantID=unlist(grantIDs))
write.table(grants.table, "data/PMIDs_to_grants.txt",col.names = T,sep="\t",quote=T)


wordAssoc<-mclapply(split(rownames(tdm), cut(seq_along(rownames(tdm)[1:5000]), 50)),
                                           function(x) findAssocs(tdm,x, 0.01), mc.cores=4)



##FY model-model distance measures.
edges<-lapply(2:(length(models.fy)-1), function(x){
    d<-dist(exp(models.fy[[x]][[2]]@beta), exp(models.fy[[(x+1)]][[2]]@beta), method="bhjattacharyya")
    e<-dist2Table(d)
    e$col<-paste0("FY",substr(names(models.fy)[x],3,4),"_",e$col)
    e$row<-paste0("FY",substr(names(models.fy)[(x+1)],3,4),"_",e$row)
    colnames(e)<-c("N1","N2","Value")
    e
})

nodes<-do.call(rbind, lapply(seq_along(edges), function(x) {
    n<-data.frame(ID=unique(edges[[x]]$N1), x=x, y=seq_along(unique(edges[[x]]$N1)))
}))

nodes<-rbind(nodes, data.frame(data.frame(ID=unique(edges[[6]]$N2), x=7, y=seq_along(unique(edges[[6]]$N2)))))

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


wordAssoc<-mclapply(split(rownames(tdm), cut(seq_along(rownames(tdm))), 50),
                                           findAssocs(tdm,x, 0.01), mc.cores=4)


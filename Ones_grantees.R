library(tm)
library(topicmodels)
library(reshape2)
library(proxy)
library(ape)
library(igraph)
library(gplots)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

##load Corpus.
abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
for (x in c("PMID","GrantID","Date", "FY", "FY.Q")) {
    meta(abstrCorpus, x)<-metaData[,x]
}

spCorpus<-Corpus(DirSource("Corpus/SP/"), readerControl = list(language="english"))

##load Topic Models
load("LDA_models_current.rda")
load("LDA_FY_models_current.rda")

ones<-read.table("data/ONES_grants_2012_present.txt")
ones$V2<-substr(ones$V1, 5,12)

pmids<-lapply(ones$V2, function(x) grep(x,meta(abstrCorpus)[,"GrantID"]))
names(pmids)<-ones$V2

png("~/Desktop/Ones_pub_number.png", height=800, width=1400, units="px")
barplot(unlist(lapply(pmids, length)), names=names(pmids), las=3)
dev.off()

models[[2]]@gamma[1:5,1:5]
lapply(seq_along(pmids), function(x){
    if(length(pmids[[x]])>0){
        png(paste0(resultsPath,"/",names(pmids)[x], "Ones_grantees_topics.png"), height=600, width=1000, units="px")
        par(mar=c(15,4,2,1))
        barplot(colSums(models[[2]]@gamma[which(models[[2]]@documents %in% meta(abstrCorpus)[pmids[[x]],1]),])
            , names=apply(terms(models[[2]],3),2,function(z) paste(z,collapse=",")), las=3, main=names(pmids)[x],cex.names=1)
        dev.off()
    }
})

x<-do.call(rbind, lapply(seq_along(pmids), function(x){
    colSums(models[[2]]@gamma[which(models[[2]]@documents %in% meta(abstrCorpus)[pmids[[x]],1]),])
}))

png(paste0(resultsPath, "/Ones_grantees_topicsSum.png"), height=800, width=1400, units="px")
par(mar=c(15,4,2,1))
barplot(x, las=3, 
        names=apply(terms(models[[2]],3),2,function(z) paste(z,collapse=",")), 
        col=palette(rainbow(18)),ylab="Sum Topic Probility" )
dev.off()

rownames(x)<-ones$V2
colnames(x)<-apply(terms(models[[2]],3),2, function(z) paste(z, collapse=","))

x.dist<-dist(t(x), method="cosine")
png(paste0(resultsPath, "/Ones_grantees_topicMap.png"), height=1600, width=1600, units="px")
par(mar=c(15,15,15,15))
heatmap.2(as.matrix(x.dist),lmat=matrix(c(4,2,3,1), nrow = 2), lhei=c(1,4.5), lwid=c(1,4), trace="none", mar=c(18,18), cexRow=1.25, cexCol=1.25, symkey=F)
dev.off()

edges<-melt(x)
edges<-edges[edges$value>0.01,]
nodes<-as.data.frame(c(unlist(lapply(pmids, length))/3, colSums(x)))
colnames(nodes)<-"size"
nodes$color<-"green"
nodes[grep("ES",rownames(nodes)),]$color<-"red"
nodes<-nodes[nodes$size>0,]

g<-graph.data.frame(edges, directed=F)
E(g)$width<-edges$value
E(g)$Weight<-edges$value
V(g)$size<-nodes$size
V(g)$color<-nodes$color

png(paste0(resultsPath, "/Ones_grantees_topicNetwork.png"), height=1600, width=1600, units="px")
plot(g, layout=layout_with_fr)
dev.off()

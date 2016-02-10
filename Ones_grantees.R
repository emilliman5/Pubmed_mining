library(tm)
library(topicmodels)
library(reshape2)
library(proxy)
library(ape)
library(igraph)
library(gplots)
library(rCharts)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

##load Corpus.
abstrCorpus<-Corpus(DirSource("data/Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("data/CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric','character','character','logical'))
for (x in c("PMID","GrantID","Date","FY.Q","FY","Journal","Title", "InModel")) {
    meta(abstrCorpus, x)<-metaData[,x]
}

spCorpus<-Corpus(DirSource("data/Corpus/SP/"), readerControl = list(language="english"))

##load Topic Models
load("data/LDA_models_current.rda")
load("data/LDA_FY_models_current.rda")

ones<-read.table("data/ONES_grants_ALL_from_RFAs.txt")
ones$V2<-substr(ones$V1, 5,12)

pmids<-lapply(unique(ones$V2), function(x) grep(x,meta(abstrCorpus)[,"GrantID"]))
names(pmids)<-unique(ones$V2)

##this is a workaround until new models are made
docRemove<-which(meta(abstrCorpus)$InModel)
models[[2]]@documents<-c(meta(abstrCorpus)[-docRemove,1], names(spCorpus))
rownames(models[[2]]@gamma)<-c(meta(abstrCorpus)[-docRemove,1], names(spCorpus))


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
rownames(x)<-unique(ones$V2)
colnames(x)<-apply(terms(models[[2]],3),2, function(z) paste(z, collapse=","))


png(paste0(resultsPath, "/Ones_grantees_topicsSum.png"), height=800, width=1400, units="px")
par(mar=c(15,4,2,1))
barplot(x, las=3, 
        names=apply(terms(models[[2]],3),2,function(z) paste(z,collapse=",")), 
        col=palette(rainbow(18)),ylab="Sum Topic Probility" )
dev.off()

x.dist<-dist(t(x), method="cosine")
png(paste0(resultsPath, "/Ones_grantees_topicMap.png"), height=1600, width=1600, units="px")
par(mar=c(15,15,15,15))
heatmap.2(as.matrix(x.dist),lmat=matrix(c(4,2,3,1), nrow = 2), lhei=c(1,4.5), lwid=c(1,4), trace="none", mar=c(18,18), cexRow=1.25, cexCol=1.25, symkey=F)
dev.off()

edges<-melt(x)
edges<-edges[edges$value>0.01,]
nodes<-data.frame(c(unlist(lapply(pmids, length))/3, colSums(x)))
#rownames(nodes)<-c(names(pmids),colnames(x))
colnames(nodes)<-"size"
nodes$color<-"green"
nodes[grep("ES",rownames(nodes)),]$color<-"red"
nodes<-nodes[nodes$size>0,]

g<-graph.data.frame(edges, directed=F)
E(g)$width<-edges$value
E(g)$weight<-edges$value
V(g)$size<-nodes$size
V(g)$color<-nodes$color

png(paste0(resultsPath, "/Ones_grantees_topicNetwork.png"), height=1600, width=1600, units="px")
plot(g, layout=layout_with_fr)
dev.off()

edgelist<-get.data.frame(g)
edgelist<-edgelist[,c("to","from","weight")]
colnames(edgelist)<-c("source","target","value")
edgelist[,"source"]<-as.character(edgelist[,"source"])
edgelist[,"target"]<-as.character(edgelist[,"target"])

sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')
sankeyPlot$set(
    data = edgelist,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 1500,
    height = 1000
    #labelFormat = ".1%"
)

#sankeyPlot$print(chartId="sankey1")
sankeyPlot

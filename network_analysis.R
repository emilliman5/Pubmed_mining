library(proxy)
library(reshape2)
library(igraph)
library(ape)
library(arcdiagram)
library(RColorBrewer)
library(riverplot)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

abstrCorpus<-Corpus(DirSource("data/Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("data/CorpusMetaData.txt",colClasses=c('character','character','Date','numeric','integer','character', 'factor'))
for (x in colnames(metaData)) {
        meta(abstrCorpus, x)<-metaData[,x]
}

load("data/LDA_models_current.rda")
dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

###############
##EDA of LDA assignments and data reduction methods
###############

#to determine a reasonable cutoff for "significant" assignment of docs to topics
#I would expect most docs to truely contain 1-3 topics with reviews containing no more than 10ish...
hist(rowSums(topDocGamma[[2]]>=0.15),6)
summary(rowSums(topDocGamma[[2]]>=0.15))
table(cut(rowSums(topDocGamma[[2]]>=0.15),6))

plot(density(as.vector(topDocGamma[[3]])), ylim=c(0,5000))
lines(density(as.vector(topDocGamma[[2]])), col="blue")
lines(density(as.vector(topDocGamma[[1]])),col="red")

boxplot(log10(as.vector(topDocGamma[[1]])),range=0 ,
        log10(as.vector(topDocGamma[[2]])),
        log10(as.vector(topDocGamma[[3]])), names = c("25 topics","50 topics","100 topics"))


####Riverplots using FY-LDA models.
load("data/LDA_FY_models_current.rda")
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

edges<-do.call(rbind, edges)
topEdges<-do.call(rbind, by(edges,edges$N1, function(x) head(x[order(x$Value),],n=3)))

png(paste0(resultsPath, "/FY_LDA_modeldist_score_distr.png"), height=600, width=800, units="px")
hist(edges$Value, breaks=1000)
dev.off()

edges<-edges[edges$Value<1.0,]

# edges$Value<-edges$Value/1000

palette<-paste0(brewer.pal(12, "Paired"), "80")
styles<-lapply(nodes$y, function(n){
    list(col=palette[ceiling(n/6)+1], lty=0, textcol="black")
})
names(styles)<-nodes$ID

rp<-list(nodes=nodes, edges=edges, styles=styles)
class(rp)<-c(class(rp), "riverplot")
png(paste0(resultsPath,"/RiverPlot_",timeStamp(),".png"), height=2400, width=4800, units="px")
par(cex.lab = 0.25)
plot(rp, plot_area=0.95, yscale=0.06, srt=T)
dev.off()

rp1<-list(nodes=nodes, edges=topEdges, styles=styles)
class(rp1)<-c(class(rp1), "riverplot")
png(paste0(resultsPath,"/RiverPlot_top5edges_perNode",timeStamp(),".png"), height=1600, width=3500, units="px")
par(cex.lab = 0.25)
plot(rp1, plot_area=0.95, yscale=0.06, srt=T)
dev.off()

#############
##Riverplot 2009->2012->2015
#############
library(topicmodels)
library(proxy)
load("data/LDA_FY_models_current.rda")
fy.topics<-lapply(seq_along(models.fy), function(f) apply(terms(models.fy[[f]][[2]],4),2,
                                                          function(z) paste(z,collapse=",")))

edges<-lapply(list(c(3,6),c(6,9)), function(x){
    d<-dist(exp(models.fy[[x[1]]][[2]]@beta), exp(models.fy[[x[2]]][[2]]@beta), method="bhjattacharyya")
    e<-dist2Table(d)
    e$col<-fy.topics[[x[1]]][e$col]
    e$row<-fy.topics[[x[2]]][e$row]
    colnames(e)<-c("N1","N2","Value")
    e
})

nodes<-do.call(rbind, lapply(seq_along(edges), function(x) {
    n<-data.frame(ID=unique(edges[[x]]$N1), x=x, y=seq_along(unique(edges[[x]]$N1)), stringsAsFactors=FALSE)
}))

nodes<-rbind(nodes, data.frame(data.frame(ID=unique(edges[[2]]$N2), x=3, y=seq_along(unique(edges[[2]]$N2))), stringsAsFactors=FALSE))

edges<-do.call(rbind, edges)
topEdges<-do.call(rbind, by(edges,edges$N1, function(x) head(x[order(x$Value),],n=3)))
edges.1<-edges
edges.1[[1]]<-edges.1[[1]][grep(nodes[46,1],edges.1[[1]]$N1),]
edges.1[[1]]<-edges.1[[1]][edges.1[[1]]$Value<1.2,]
edges.1[[2]]<-do.call(rbind, lapply(edges.1[[1]]$N2, function(x) 
    edges.1[[2]][grep(x,edges.1[[2]]$N1),]
    ))
edges.1[[2]]<-edges.1[[2]][edges.1[[2]]$Value<1.1,]
edges.1<-do.call(rbind, edges.1)

png(paste0(resultsPath, "/FY_LDA_modeldist_score_distr.png"), height=600, width=800, units="px")
hist(edges$Value, breaks=1000)
dev.off()

edges<-edges[edges$Value<1.1,]

# edges$Value<-edges$Value/1000

palette<-paste0(brewer.pal(12, "Paired"), "80")
styles<-lapply(nodes$y, function(n){
    list(col=palette[ceiling(n/6)+1], lty=0, textcol="black")
})
names(styles)<-nodes$ID


rp<-list(nodes=nodes, edges=edges, styles=styles)
class(rp)<-c(class(rp), "riverplot")
png(paste0(resultsPath,"/RiverPlot_",timeStamp(),".png"), height=1600, width=2100, units="px")
op<-par(cex=1.5)
plot(rp, plot_area=0.95, srt=T)
par(op)
dev.off()

rp1<-list(nodes=nodes, edges=topEdges, styles=styles)
class(rp1)<-c(class(rp1), "riverplot")
png(paste0(resultsPath,"/RiverPlot_top5edges_perNode",timeStamp(),".png"), height=1600, width=2100, units="px")
op<-par(cex=1.5)
plot(rp1, plot_area=0.95, srt=T)
par(op)
dev.off()

rp2<-list(nodes=nodes, edges=edges.1, styles=styles)
class(rp2)<-c(class(rp2), "riverplot")
png(paste0(resultsPath,"/RiverPlot_Topic46_evo",timeStamp(),".png"), height=1600, width=2100, units="px")
op<-par(cex=1.5)
plot(rp2, plot_area=0.95, srt=T)
par(op)
dev.off()


############
##Static Network Output
############
fys<-as.integer(c(levels(as.factor(DocNodes$FY))))
fys<-fys[-1]
fyqs<-as.numeric(c(levels(as.factor(DocNodes$FY.Q))))
fyqs<-fyqs[-c(1,2,3,4)]

TopicNodes<-read.csv("results//2015aug05_0846//Network//Topics50//TopicNodes.csv", stringsAsFactors=F)
DocTopEdges<-read.csv("results//2015aug05_0846//Network//Topics50/TopicDocumentProbEdges.csv", stringsAsFactors=F)         
DocNodes<-read.csv("results/2015aug05_0846/Network/AbstrNodeAttrs.csv", stringsAsFactors=F)
SpNodes<-read.csv("results/2015aug05_0846/Network/SpNodeAttrs.csv", stringsAsFactors=F)

TopicNodes[,c("FY","FY.Q")]<-rep(NA)

SpNodes[,c("FY","FY.Q")]<-NA
SpNodes[,"TopicWords"]<-SpNodes[,"ID"]
DocNodes[,c("TopicWords")]<-NA

TopicNodes<-TopicNodes[,c("ID","TopicWords","Type","FY","FY.Q")]
DocNodes<-DocNodes[,c("ID","TopicWords","Type","FY","FY.Q")]
SpNodes<-SpNodes[,c("ID","TopicWords","Type","FY","FY.Q")]

nodes<-rbind(TopicNodes,DocNodes,SpNodes)
colnames(DocTopEdges)<-c("from","to","Weight","Type")          

lapply(fyqs, function(x){
    nodes<-rbind(TopicNodes,DocNodes[DocNodes$FY.Q==x,],SpNodes)
    edges<-DocTopEdges[DocTopEdges$from %in% nodes$ID,]
    edges<-do.call(rbind, by(edges, edges$from, function(x) x[order(x$Weight, decreasing=T)[1:3],]))
    edges<-edges[!is.na(edges$Weight),]
    
    net<-graph.data.frame(edges, nodes, directed=F)
    V(net)$TopicWords<-gsub("\\|", ",", V(net)$TopicWords)
    
    l<-layout.fruchterman.reingold(net, weight=(1-E(net)$Weight)*10)
    shape<-factor(nodes$Type,labels = c("circle","square","csquare"))
    color<-factor(nodes$Type,labels = c("blue","chartreuse4","red"))
    
    png(paste0(resultsPath, "/TopicDoc_network_FYQ",x,"_similWeights_",timeStamp(),".png"), height=2500, width=2500, units="px")
    plot(net, 
         vertex.size=1+(degree(net, V(net), mode="all", normalized=T)*40),
         vertex.label.cex=1.5, 
         vertex.label.color="black",
         vertex.label=V(net)$TopicWords,vertex.color=as.character(color), 
         vertex.shape=as.character(shape), 
         edge.width=(edges$Weight*4.26)+0.957, edge.curved=0.2, 
         layout=l, rescale=T)
    dev.off()
})

#############
##Topic-Topic Distance by FY
##############

topDocDist<-lapply(c(2009,2010,2011,2012,2013,2014,2015), function(x){
        ids<-meta(abstrCorpus)[,"FY"]==x
        d<-dist(t(models[[2]]@gamma[ids,]), method="cosine")
        t<-melt(as.matrix(d), varnames=c("col","row"))   
        t<-t[t$row>t$col,]
        t
})

plot(density(do.call(rbind, topDocDist)$value), main="Topic-Topic Distance Distributions", bty="n")
lines(density(topDocDist[[1]]$value), col="red")
lines(density(topDocDist[[7]]$value), col="blue")
legend("topleft",bty="n", lty=c(1,1,1), lwd=c(2,2,2), col=c("black","red","blue"), legend=c("All FYs","FY 2009","FY 2015"))

connections<-lapply(topDocDist, function(z) {
        x<-z[z$value<=0.9,]
        tapply(x$col, x$col, length)
        })
plot(density(unlist(connections)), main="Number of Connections per Topic", bty="n")
lines(density(connections[[1]]), col="red")
lines(density(connections[[7]]), col="blue")
legend("topright",bty="n", lty=c(1,1,1), lwd=c(2,2,2), col=c("black","red","blue"), legend=c("All FYs","FY 2009","FY 2015"))

png(paste0(resultsPath,"/Topic-Topic_connections.png"),height=800, width=900, units="px")
par(mfrow=c(2,1))
plot(density(do.call(rbind, topDocDist)$value), main="Topic-Topic Distance Distributions", bty="n")
lines(density(topDocDist[[1]]$value), col="red")
lines(density(topDocDist[[7]]$value), col="blue")
abline(v=0.9)
legend("topleft",bty="n", lty=c(1,1,1), lwd=c(2,2,2), col=c("black","red","blue"), legend=c("All FYs","FY 2009","FY 2015"))

plot(density(unlist(connections)), main="Number of Connections per Topic", bty="n")
lines(density(connections[[1]]), col="red")
lines(density(connections[[7]]), col="blue")
legend("topright",bty="n", lty=c(1,1,1), lwd=c(2,2,2), col=c("black","red","blue"), legend=c("All FYs","FY 2009","FY 2015"))
dev.off()

connections.1<-do.call(c, connections)
connec<-lapply(1:10000, function(x) {
        mean(sample(connections.1, 37, replace = T))
        })
hist(unlist(connec))
sum(unlist(connec)>4.027)/10000

topDocDistFYtable<-do.call(cbind, lapply(topDocDist.fy[[2]], function(x){
  x<-as.matrix(x)
  rownames(x)<-seq(1,nrow(x))
  colnames(x)<-seq(1,ncol(x))
  t<-melt(as.matrix(x), varnames=c("col","row"))   
  t<-t[t$row>t$col,]
  t
}) )

topDocDistFYtable<-topDocDistFYtable[,c(1,2,3,6,9,12,15,18,21,24)]
topDocDistFYtable[,1]<-paste0("Topic",topDocDistFYtable[,1])
topDocDistFYtable[,2]<-paste0("Topic",topDocDistFYtable[,2])
colnames(topDocDistFYtable)<-c("to","from", "2008","2009","2010","2011","2012","2013","2014","2015")
rownames(topDocDistFYtable)<-paste(topDocDistFYtable[,1],topDocDistFYtable[,2], sep = "-")

png(paste0(resultsPath, "/Topic-TopicSimilarityHeatmap_overTime.png"), height=1200, width=1200, units="px")
heatmap.2(as.matrix(topDocDistFYtable[,3:10]),labRow = NA, trace = "none")
dev.off()
png(paste0(resultsPath, "/Topic-TopicSimilarityDistribution_overTime.png"), height=600, width=1200, units="px")
hist(as.matrix(topDocDistFYtable[,3:10]), breaks=100)
dev.off()

k<-kmeans(topDocDistFYtable[,3:10],centers = 5, iter.max = 1000)
png(paste0(resultsPath, "/Topic-TopicKmeans_overTime.png"), height=1200, width=1200, units="px")
par(mfrow=c(2,1))
matplot(x=colnames(topDocDistFYtable)[4:10], t(topDocDistFYtable[,4:10]), type="l", col=1:length(topDocDistFYtable))
matplot(x=colnames(k$centers), t(k$centers), type="l")
legend("bottomleft", lty=1, legend=paste("Cluster", 1:length(k$size), sep=" "), col=1:length(k$size), bty="n")
dev.off()

###Write network data to files...
path<-paste0(resultsPath,"/Network/")
dir.create(path)

meta(abstrCorpus, "Type")<-"Abstract"
write.table(file =paste0(path,"/AbstrNodeAttrs.csv"), x=meta(abstrCorpus)[,c("PMID","FY","FY.Q","Type")],sep=",",
            quote=F, row.names=F, col.names=T)

spMeta<-as.data.frame(cbind(names(spCorpus), "StrategicPlan"))
colnames(spMeta)<-c("ID","Type")
write.table(spMeta, paste0(path,"/SpNodeAttrs.csv"),sep=",",
            quote=F, row.names=F, col.names=T)

lapply(models, function(x) {
    p<-paste0(path,"/Topics", x@k)
    dir.create(p)    
    topicNodes<-as.data.frame(apply(terms(x,4),2,function(z) paste(z,collapse="|")))
    colnames(topicNodes)<-"TopicWords"
    topicNodes$ID<-rownames(topicNodes)
    topicNodes$ID<-gsub(" ", "", topicNodes$ID)
    topicNodes$Type<-"Topic"
    write.table(topicNodes, paste0(p,"/TopicNodes.csv"), row.names=F,sep=",", quote=F)
})

mclapply(topDocGamma, mc.cores=4,function(x){
    p<-paste0(path,"/Topics", ncol(x))
    colnames(x)<-paste(rep("Topic",ncol(x)),seq(1,ncol(x)), sep="")
    x<-as.data.frame(x)
    x$id<-c(meta(abstrCorpus)[-docRemove,"PMID"], names(spCorpus))
    t<-reshape(x, times = colnames(x)[-ncol(x)], varying = colnames(x)[-ncol(x)], direction="long",v.names ="Weight",idvar = "id" ,ids = rownames(x),timevar = "Topic")
    t<-t[t$Weight>0.01,]
    t$Weight<--t$Weight
    t$Type<-rep("Undirected")
    colnames(t)<-c("Source","Target","Weight","Type")
    write.table(t, paste0(p, "/TopicDocumentProbEdges.csv"), sep=",",
                row.names=F, col.names=T,quote=F)
})

mclapply(topTermBeta,mc.cores=2, function(x){
    p<-paste0(path,"/Topics", nrow(x))
    x<-as.data.frame(x)
    x$Topics<-paste(rep("Topic",nrow(x)),seq(1,nrow(x)), sep="")
    t<-reshape(x, times = colnames(x)[-ncol(x)], varying = colnames(x)[-ncol(x)], direction="long",v.names ="Weight",idvar = "Topics" ,ids = rownames(x),timevar = "Term")
    t<-t[t$Weight>-250,]
    t$Type<-rep("Undirected")
    colnames(t)<-c("Source","Target","Weight","Type")
    write.table(t, paste0(p, "/TopicTermWeightEdges.csv"), sep=",",
                row.names=F, col.names=T,quote=F)
})

library(reshape2)

lapply(topDocDist, function(x){
    p<-paste0(path,"/Topics", nrow(x))
    x<-as.matrix(x)
    rownames(x)<-seq(1,nrow(x))
    colnames(x)<-seq(1,ncol(x))
    t<-melt(as.matrix(x), varnames=c("col","row"))   
    t<-t[t$row>t$col,]
    t$col<-paste(rep("Topic", nrow(x)), t$col, sep="")
    t$row<-paste(rep("Topic", nrow(x)), t$row, sep="")
    t$Type<-rep("Undirected")
    colnames(t)<-c("Source", "Target","Weight", "Type")
    write.table(t,paste0(p,"/TopicTopicbyDocSimilarity.csv"), sep=",", row.names=F, quote=F, col.names=T)    
})

lapply(topTermsDist, function(x){
    p<-paste0(path,"/Topics", nrow(x))
    x<-as.matrix(x)
    rownames(x)<-seq(1,nrow(x))
    colnames(x)<-seq(1,ncol(x))
    t<-melt(as.matrix(x), varnames=c("col","row"))   
    t<-t[t$row>t$col,]
    t$col<-paste(rep("Topic", nrow(x)), t$col, sep="")
    t$row<-paste(rep("Topic", nrow(x)), t$row, sep="")
    colnames(t)<-c("Source", "Target","Weight")
    t$Type<-rep("Undirected")
    write.table(t,paste0(p,"/TopicTopicbyTermSimilarity.csv"), sep=",", row.names=F, quote=F, col.names=T)        
})

##########
##Grant-Grant Co-occurence
##########

library(igraph)

co.occ<-table(grants.table[grants.table$year==2015,1:2])
co.occ<-co.occ[rowSums(co.occ)>0, colSums(co.occ)>0]

adj<-t(co.occ) %*% co.occ

g<-graph.adjacency(adj, mode="Undirected")
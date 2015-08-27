############
##Co-Occurence analysis
############

pmid<-meta(abstrCorpus)[,"FY.Q"]=="2009.4"

dtm.sub<-as.matrix(dtm[pmid,])
dtm.sub<-dtm.sub[,colSums(dtm.sub)>0]
dtm.sub<-apply(dtm.sub, 2, function(x) as.numeric(x>0))
terms<-colnames(dtm.sub)
com<-dtm.sub %*% t(dtm.sub)
diag(com)<-0

comDist<-dist(com, "jaccard")
comDendro<-as.dendrogram(hclust(comDist))
plot(cut(comDendro, h=0.6)$upper, cex=0.1)
plot(comDendro)


#####################
###Topic-Topic Co Occurences
#####################
lapply(fys, function(x){
    DocTopEdge.m<-dcast(DocTopEdges[DocTopEdges$from %in% DocNodes[DocNodes$FY==x,"ID"],], to ~ from, value.var="Weight", fill=0)
    rownames(DocTopEdge.m)<-DocTopEdge.m[,1]
    DocTopEdge.m<-as.matrix(DocTopEdge.m[,-1])
    DocTopEdge.m<-apply(DocTopEdge.m, 2, function(x) as.numeric(x>0.1))
    TopCoOc<-DocTopEdge.m %*% t(DocTopEdge.m)
    diag(TopCoOc)<-0
    
    png(paste0(resultsPath, "/TopicCoOccurence_heatmapFY",x,"_",timeStamp(),".png"), height=1200, width=1200, units="px")
    heatmap.2(TopCoOc, trace="none",cexRow = 0.75, cexCol=0.75)
    dev.off()
})

lapply(fyqs, function(x){
    DocTopEdge.m<-dcast(DocTopEdges[DocTopEdges$from %in% DocNodes[DocNodes$FY.Q==x,"ID"],], to ~ from, value.var="Weight", fill=0)
    rownames(DocTopEdge.m)<-DocTopEdge.m[,1]
    DocTopEdge.m<-as.matrix(DocTopEdge.m[,-1])
    DocTopEdge.m<-apply(DocTopEdge.m, 2, function(x) as.numeric(x<0.15))
    TopCoOc<-DocTopEdge.m %*% t(DocTopEdge.m)
    diag(TopCoOc)<-0
    
    png(paste0(resultsPath, "/TopicCoOccurence_heatmapFY",x,"_",timeStamp(),".png"), height=1200, width=1200, units="px")
    heatmap.2(TopCoOc, trace="none",cexRow = 0.75, cexCol=0.75)
    dev.off()
})

DocTopEdge.m<-dcast(DocTopEdges[DocTopEdges$from %in% DocNodes[DocNodes$FY.Q==2009.1,"ID"],], to ~ from, value.var="Weight", fill=0)
rownames(DocTopEdge.m)<-DocTopEdge.m[,1]
DocTopEdge.m<-as.matrix(DocTopEdge.m[,-1])
DocTopEdge.m<-apply(DocTopEdge.m, 2, function(x) as.numeric(x>0.2))
TopCoOc<-DocTopEdge.m %*% t(DocTopEdge.m)
diag(TopCoOc)<-0

g<-graph.adjacency(TopCoOc, weighted =TRUE,mode = "undirected")
g<-simplify(g)
l<-layout_with_fr(g, weights = E(g)$weight)
plot(g)

g<-graph.adjacency(com, weight=T, mode="undirected")
g<-simplify(g)
V(g)$label<-V(g)$name
V(g)$degree<-degree(g)
png(paste0(resultsPath,"/CoOccurenceGraph",gsub("-| |:", "",Sys.time()),".png"),height=1200, width=1200, units="px")
plot(g)
dev.off()

cm<-com
cm<-com[rowSums(com)>150,colSums(com)>150]
s<-sample(1:dim(com)[1], 500)
png("heatmap.png", height=10000, width=10000,units="px")
heatmap.2(cm, Rowv=NA, Colv=NA,dendrogram='none',trace='none',labRow = NA, labCol=NA, key = F)
dev.off()
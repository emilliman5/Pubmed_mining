############
##Co-Occurence analysis
############

pmid<-meta(abstrCorpus)[,"FY.Q"]=="2009.4"

dtm.sub<-dtm[pmid,]
dtm.sub<-dtm.sub[,colSums(dtm.sub)>0]
dtm.sub<-apply(dtm.sub, 2, function(x) as.numeric(x>0))
terms<-colnames(dtm.sub)
com<-t(dtm.sub) %*% dtm.sub
diag(com)<-0

comDist<-dist(com, "cosine")
comDendro<-as.dendrogram(hclust(comDist))
plot(cut(comDendro, h=0.6)$upper, cex=0.1)



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
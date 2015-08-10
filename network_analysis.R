library(igraph)
library(ape)
#setwd("results/2015aug06_0840/")

dends1<-lapply(names(topDocDist.fy), function(x){
    lapply(seq(1,length(topDocDist.fy[[x]])), function(y){
        hclust(topDocDist.fy[[x]][[y]])
    })
})

x<-dends1[[1]][[2]]
z<-hclust(topTermsDist[[1]])

phyloTree<-as.phylo(x)
phyloEdges<-phyloTree$edge    
net<-graph.edgelist(phyloEdges,directed = F)     
plot(net)

graphLayout<-layout.auto(net)
plot(net, layout = graphLayout*0.2, rescale=F)
n.obs<-length(x$labels)
png(paste0(resultsPath, "/topic_phylo.png"), height=2400, width=2400, units="px")
plot(graphLayout[,1],graphLayout[,2], type="n", axes=F, xlab="", ylab="")
segments(
    x0 = graphLayout[phyloEdges[,1],1], 
    y0 = graphLayout[phyloEdges[,1],2],
    x1 = graphLayout[phyloEdges[,2],1],
    y1 = graphLayout[phyloEdges[,2],2],
    col = "darkgray", lwd = 3
)

text(graphLayout[1:n.obs,1],graphLayout[1:n.obs,2],
    phyloTree$tip.label, cex=2, xpd=T, font=1)
dev.off()

############
##Static Network Output
############
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

nodes<-rbind(TopicNodes,DocNodes[DocNodes$FY.Q==2009.1,],SpNodes)
edges<-DocTopEdges[DocTopEdges$from %in% nodes$ID,]
edges<-do.call(rbind, by(edges, edges$from, function(x) x[order(x$Weight, decreasing=T)[1:3],]))
edges<-edges[!is.na(edges$Weight),]


net<-graph.data.frame(edges, nodes, directed=F)
V(net)$TopicWords<-gsub("\\|", ",", V(net)$TopicWords)

l<-layout.fruchterman.reingold(net, weight=E(net)$Weight*10,repulserad=vcount(net)^4)
shape<-factor(nodes$Type,labels = c("circle","square","csquare"))
color<-factor(nodes$Type,labels = c("blue","chartreuse4","red"))

png(paste0(resultsPath, "/TopicDoc_network",getDate(),".png"), height=2500, width=2500, units="px")
plot(net, 
     vertex.size=1+(degree(net, V(net), mode="all", normalized=T)*40),
     vertex.label.cex=1.5, 
     vertex.label.color="black",
     vertex.label=V(net)$TopicWords,vertex.color=as.character(color), 
     vertex.shape=as.character(shape), 
     edge.width=(edges$Weight*4.26)+0.957, edge.curved=0.2, 
     layout=l, rescale=T)
dev.off()

##############
##Topic-Topic Distance by FY
##############
library(reshape2)

topDocDistFYtable<-do.call(cbind, lapply(topDocDist.fy[[1]], function(x){
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

heatmap.2(as.matrix(topDocDistFYtable[,3:10]), trace = "none")

hist(as.matrix(topDocDistFYtable[,3:10]), breaks=100)

matplot(x=colnames(topDocDistFYtable)[4:10], t(topDocDistFYtable[,4:10]), type="l", col=1:length(topDocDistFYtable))

k<-kmeans(topDocDistFYtable[,3:10],centers = 5, iter.max = 1000)
matplot(x=colnames(k$centers), t(k$centers), type="l")
legend("bottomleft", lty=1, legend=paste("Cluster", 1:length(k$size), sep=" "), col=1:length(k$size), bty="n")

##############
##DendroArcs
##############

x<-dends1[[1]][[2]]
z<-hclust(topTermsDist[[1]])


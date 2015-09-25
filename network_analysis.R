library(reshape2)
library(igraph)
library(ape)
library(arcdiagram)
library(RColorBrewer)
library(riverplot)

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

##############
##DendroArc plots
##############
dends1<-lapply(names(topDocDist.fy), function(x){
    lapply(seq(1,length(topDocDist.fy[[x]])), function(y){
        hclust(topDocDist.fy[[x]][[y]])
    })
})

dendTop50<-dends[[2]]
z<-hclust(topTermsDist[[2]])
d<-lapply(levels(as.factor(meta(abstrCorpus)$FY)), function(x){
    topDocDistFYtable[topDocDistFYtable[,x]<=0.94,c("to","from",as.character(x))]
})

degree<-lapply(levels(as.factor(meta(abstrCorpus)$FY)), function(x){
    pmids<-meta(abstrCorpus)[-docRemove,"FY"]==x
    colSums(topDocGamma[[2]][pmids,]>=0.15)
})
names(degree)<-levels(as.factor(meta(abstrCorpus)$FY))
names(d)<-levels(as.factor(meta(abstrCorpus)$FY))
topDocDegree<-do.call(rbind, degree)[-1,]

png(paste0(resultsPath, "/TopicUsagebyDocumentbyFY.png"), height=1600, width=1600, units="px")
par(mfrow=c(2,1),oma=c(4,1,1,1), mar=c(15,4,2,2))
barplot(topDocDegree, las=2, col=2:7, ylab="Number of Citations Assigned to Topic")
barplot(t(t(topDocDegree)/colSums(topDocDegree)), las=2, col=2:7, ylab="Percentage of Citations Assigned to Topic")
dev.off()
barplot(t(topDocDegree/rowSums(topDocDegree)))

png(paste0(resultsPath, "/TopicUsagebyDocumentbyFY_lineplot.png"), height=1600, width=1600, units="px")
par(mfrow=c(2,1),mar=c(6,8,2,2))
matplot(x=c(2009:2015),topDocDegree/rowSums(topDocDegree),cex.lab=2, cex.axis=2, type="l", xlab="Fiscal Year", ylab="Proportion of Citations in Topic")
matplot(xlim=c(1,50), xlab="Topic Number",cex.lab=2, cex.axis=2, ylab="Proportion of Citations Assigned to Topic",t(topDocDegree/rowSums(topDocDegree)), type="l")
dev.off()

order<-gsub("Topic ", "", names(z$labels[z$order]))
edges<-lapply(d, function(x) as.matrix(x[,1:2]))
edges<-lapply(edges, function(x) gsub("Topic","",x))
lab<-gsub("Topic ","", names(z$labels))
sizes<-lapply(degree, function(x) as.integer(cut(x,10)))
colors<-cutree(z, k=6)
edge.widths<-lapply(d, function(x) as.integer(cut(1-x[,3],5))*2)

lapply(seq(2,8), function(x){
    png(paste0(resultsPath, "/ArcDiagram",x,"_",gsub("-| |:", "",Sys.time()),".png"),height=600, width=1200, units="px")
    arcplot(edges[[x]],vertices = lab, col.labels = "black", pch=21,
            main=names(degree)[x],cex.nodes = sizes[[x]],ordering=order, col.nodes="black", bg.nodes= 1+colors)
    dev.off()
})

lapply(seq(2,8), function(x){
    png(paste0(resultsPath, "/DendroArcs",x,"_",gsub("-| |:", "",Sys.time()),".png"),height=1200, width=800, units="px")
    par(mfcol=c(1,2))
    plot(as.phylo(z), main="Topic-Topic relationship by Terms")
    arcplot(edges[[x]],vertices = lab, col.labels = "black", pch=21,lwd.arcs = edge.widths[[x]],
            main=paste("FY",names(degree)[x]), cex.nodes = sizes[[x]],ylim=c(0.01,.99),ordering=order, horizontal=F,col.nodes="black", bg.nodes= 1+colors)
        dev.off()
})

###dendroarcs for one topic between fiscal years
lapply(seq(1,models[[2]]@k), function(x) {
        dendroArc(model = models[[2]],topicN = x)
        dev.off()
})

####Riverplots using FY-LDA models.

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

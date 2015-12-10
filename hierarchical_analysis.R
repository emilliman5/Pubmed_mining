library(proxy)
library(dendextend)
library(ape)
library(reshape2)
library(arcdiagram)
library(RColorBrewer)

###Do not enter unless text_analysis.R and topic_model.R 
##have been run to completion

distFun<-"cosine"
gammaThresh<-0.15
distThresh<-0.94


extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

##load Corpus.
abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','character','Date','numeric','integer','character', 'factor'))
for (x in colnames(metaData)) {
    meta(abstrCorpus, x)<-metaData[,x]
}

spCorpus<-Corpus(DirSource("Corpus/SP/"), readerControl = list(language="english"))

##load Topic Models
load("LDA_models_current.rda")
load("LDA_FY_models_current.rda")

###Analysis of Topic Modeling on entire

topTermBeta<-lapply(models, function(x){
    y<-as.matrix(x@beta)
    colnames(y)<-x@terms
    rownames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topTermsDist<-lapply( topTermBeta, function(x) {
    dist(x,method = distFun)
})
names(topTermsDist)<-lapply(models, function(x) x@k)

lapply(names(topTermsDist), function(x){
    png(paste0(resultsPath,"/","TopicClustering_byTerms_NumberofTopics_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topTermsDist[[x]]), cex=1)
    dev.off()
})

topDocGamma<-lapply(models, function(x) {
    y<-as.matrix(x@gamma)
    colnames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topDocDist<-lapply(topDocGamma, function(x){
    dist(t(x),method=distFun)
})
names(topDocDist)<-lapply(models, function(x) x@k)

lapply(names(topDocDist), function(x){
    png(paste0(resultsPath,"/","TopicClustering_byDoc_NumberofTopics_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topDocDist[[x]]), cex=1)
    dev.off()
})

######
#FY tangled dendrogram comparisons
######

dends<-lapply(names(topDocDist.fy), function(x){
    lapply(seq(1,length(topDocDist.fy[[x]])), function(y){
        as.dendrogram(hclust(topDocDist.fy[[x]][[y]]))
    })
})

lapply(dends, function(x){
    lapply(seq(2,length(x)-1), function(y){
        d<-dendlist(x[[y]],x[[(y+1)]])
        l<-length(labels(x[[y]]))
        png(paste0(resultsPath,"/","DendroCompare",l,"FY",y,".png"), height=1200, width=2400, units="px")
        d %>% untangle(method= "DendSer") %>% 
            tanglegram(common_subtrees_color_branches=TRUE, hang=T,lab.cex=2)
        dev.off()
    })
    d<-dendlist(x[[2]],x[[8]])
    l<-length(labels(x[[2]]))
    png(paste0(resultsPath,"/","DendroCompare",l,"FY2009-2015",".png"), height=1200, width=2400, units="px")
    d %>% untangle(method= "DendSer") %>% 
        tanglegram(common_subtrees_color_branches=TRUE, hang=T,lab.cex=2)
    dev.off()
})


ent<-unlist(lapply(seq(2,length(dends[[1]])-1), function(x){
    dendlist(dends[[1]][[x]],dends[[1]][[x+1]]) %>% untangle(method="random") %>% entanglement
}))

ent<-c(ent,dendlist(dends[[1]][[2]],dends[[1]][[8]]) %>% untangle(method="random") %>% entanglement)

##############
##DendroArc plots
##############
topDocDist.fy<-lapply(topDocGamma, function(x){
    f<-unique(meta(abstrCorpus)[,"FY"])
    idx<-lapply(f, function(x) which(meta(abstrCorpus)[,"FY"]==x) )
    lapply(idx, function(y) {
        f<-meta(abstrCorpus)[y[1],"FY"]
        dist(t(x[y,]),method=distFun)
    })
})
names(topDocDist.fy)<-lapply(models, function(x) x@k)

topDocDistFYtable<-do.call(cbind, lapply(topDocDist.fy[[2]], function(x){
    x<-as.matrix(x)
    rownames(x)<-seq(1,nrow(x))
    colnames(x)<-seq(1,ncol(x))
    t<-melt(as.matrix(x), varnames=c("col","row"))   
    t<-t[t$row>t$col,]
    t
}) )

lapply(names(topDocDist.fy), function(x){
    lapply(seq(1,length(topDocDist.fy[[x]])), function(y){
        png(paste0(resultsPath,"/","TopicClustering_byDoc_TopicNumber_",x,"FY",y,".png"), height=1200, width=2400, units="px")
        plot(hclust(topDocDist.fy[[x]][[y]]), cex=1)
        dev.off()        
    }) 
})

top50dends<-hclust(topTermsDist[[2]])
top50edges<-lapply(levels(as.factor(meta(abstrCorpus)$FY)), function(x){
    topDocDistFYtable[topDocDistFYtable[,x]<=distThresh,c("to","from",as.character(x))]
})

degree<-lapply(levels(as.factor(meta(abstrCorpus)$FY)), function(x){
    pmids<-meta(abstrCorpus)[-docRemove,"FY"]==x
    colSums(topDocGamma[[2]][pmids,]>=gammaThresh)
})
names(degree)<-levels(as.factor(meta(abstrCorpus)$FY))
names(top50edges)<-levels(as.factor(meta(abstrCorpus)$FY))
topDocDegree<-do.call(rbind, degree)[-1,]

png(paste0(resultsPath, "/TopicUsagebyDocumentbyFY.png"), height=1600, width=1600, units="px")
    par(mfrow=c(2,1),oma=c(4,1,1,1), mar=c(15,4,2,2))
    barplot(topDocDegree, las=2, col=2:7, ylab="Number of Citations Assigned to Topic")
    barplot(t(t(topDocDegree)/colSums(topDocDegree)), las=2, col=2:7, ylab="Percentage of Citations Assigned to Topic")
dev.off()


png(paste0(resultsPath, "/TopicUsagebyDocumentbyFY_lineplot.png"), height=1600, width=1600, units="px")
    par(mfrow=c(2,1),mar=c(6,8,2,2))
    matplot(x=c(2009:2015),topDocDegree/rowSums(topDocDegree),cex.lab=2, cex.axis=2, type="l", xlab="Fiscal Year", ylab="Proportion of Citations in Topic")
    matplot(xlim=c(1,50), xlab="Topic Number",cex.lab=2, cex.axis=2, ylab="Proportion of Citations Assigned to Topic",t(topDocDegree/rowSums(topDocDegree)), type="l")
dev.off()

order<-gsub("Topic ", "", names(z$labels[z$order]))
edges<-lapply(top50edges, function(x) as.matrix(x[,1:2]))
edges<-lapply(edges, function(x) gsub("Topic","",x))
lab<-gsub("Topic ","", names(top50dends$labels))
sizes<-lapply(degree, function(x) as.integer(cut(x,10)))
colors<-cutree(top50dends, k=6)
edge.widths<-lapply(d, function(x) as.integer(cut(1-x[,3],5)))

lapply(seq(2,8), function(x){
    png(paste0(resultsPath, "/ArcDiagram",x,"_",gsub("-| |:", "",Sys.time()),".png"),height=600, width=1200, units="px")
        arcplot(edges[[x]],vertices = lab, col.labels = "black", pch=21,
            main=names(degree)[x],cex.nodes = sizes[[x]],ordering=order, col.nodes="black", bg.nodes= 1+colors)
    dev.off()
})

lapply(seq(2,8), function(x){
    png(paste0(resultsPath, "/DendroArcs",x,"_",gsub("-| |:", "",Sys.time()),".png"),height=1200, width=800, units="px")
    par(mfcol=c(1,2))
    plot(as.phylo(top50dends), main="Topic-Topic relationship by Terms")
    arcplot(edges[[x]],vertices = lab, col.labels = "black", pch=21,lwd.arcs = edge.widths[[x]],
            main=paste("FY",names(degree)[x]), cex.nodes = sizes[[x]],ylim=c(0.01,.99),ordering=order, horizontal=F,col.nodes="black", bg.nodes= 1+colors)
    dev.off()
})

###dendroarcs for one topic between fiscal years
lapply(seq(1,models[[2]]@k), function(x) {
    dendroArc(model = models[[2]],topicN = x,FYs = c(2009,2015))
})

####Analysis of topics models built on FY-specific Corpus



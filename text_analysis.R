library(tm)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(topicmodels)
library(lubridate)
library(parallel)
library(igraph)
library(class)
library(cluster)
library(gplots)
library(RColorBrewer)

#library(networktools)
#If you want to force a reprocessing of the documents into a Corpus set this value to "TRUE"
reset<-FALSE

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

if(!file.exists("Corpus/1.txt") || reset){
  source("makeCorpus.R")
  abstrCorpus<-makeCorpus("ESlit.xml","stopwords.txt", 30)
} else {
  ##read in corpus docs.
  abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
  metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
  for (x in c("PMID","GrantID","Date", "FY", "FY.Q")) {
    meta(abstrCorpus, x)<-metaData[,x]
  }
}

if(!file.exists("Corpus/SP/SP_Goal1") || reset){
  source("makeCorpus.R")
  spCorpus<-makeSPCorpus("data/Strategic_goals/",
                         stopwordList = "stopwords.txt", "Goal",30)
} else {
  spCorpus<-Corpus(DirSource("Corpus/SP/"), readerControl = list(language="english"))
}

################
##Term Document Matrix Creation
################

#This is the basic data structure to mine for term usage trends, clustering, association rule mining, etc.
tdm.monogram.tfidf<-TermDocumentMatrix(abstrCorpus, 
                                 control = list(weighting=weightTfIdf))

tdm.monogram<-TermDocumentMatrix(abstrCorpus)

#################
##Ngram Analysis
#################

tdm.bigram <- TermDocumentMatrix(abstrCorpus, control = list(tokenize = NgramTokenizer))
##function(x) weightTfIdf(x,normalize=F)))

##Run one of the following commands before proceeding. 
##tdm.monogram are single term frequencies.
##tdm.bigram are two term frequencies

tdm<-tdm.monogram
tdm<-tdm.monogram.tfidf
tdm<-tdm.bigram

tdm.sp<-TermDocumentMatrix(spCorpus)
tdm.sp.tfidf<-TermDocumentMatrix(spCorpus, control=list(weighting=weightTfIdf))

###########
##TermFreq exploration and visualization
###########

tfidfHisto(tdm.monogram.tfidf ,fact = "FY", "mean")

tfHisto(tdm,"FY")

wordCloud(tdm.monogram.tfidf,fact="FY", 75, "mean","tfidf")
wordCloud(tdm.monogram.tfidf,fact="FY.Q", 75, "mean","tfidf")

wordCloud(tdm,fact="FY", 75, pre="tf")
wordCloud(tdm,fact="FY.Q", 75, pre="tf")

wordCloudMontage(tdm = tdm.sp,file = "SP_TF_wordcloud.png", path = resultsPath)
wordCloudMontage(tdm = tdm.sp.tfidf,file = "SP_TfIdf_wordcloud.png", path = resultsPath)


#############
##Topic Modelling
#############
library(proxy)
dtm<-DocumentTermMatrix(c(abstrCorpus, spCorpus))
dtm<-t(t(as.matrix(dtm))[as.vector(apply(t(as.matrix(dtm)), 1, sum)>10),])

docRemove<-which(rowSums(dtm)==0)
dtm<-dtm[-docRemove,]

seq.k<-c(50,100)

#models<-mclapply(seq.k, mc.cores = 4, function(k) LDA(dtm, k) )
models<-lapply(seq.k, function(k) LDA(dtm, k) )

model.lglk<-as.data.frame(as.matrix(lapply(models, logLik)))
LogLik.df<-data.frame("topics"=seq.k, 
                      "LL"=as.numeric(as.matrix(model.lglk)))

png(paste(resultsPath,"LDA_topicNumber_optimziation.png", sep="/"), height=1200, width=1200, units="px")
plot(LogLik.df$LL~LogLik.df$topics, pch=19, col="red", main="LDA Simulation with 10 docs per FY")
dev.off()

topTermBeta<-lapply(models, function(x){
    y<-as.matrix(x@beta)
    colnames(y)<-x@terms
    rownames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topTermsDist<-lapply( topTermBeta, function(x) {
    dist(x,method = "cosine")
})

names(topTermsDist)<-lapply(models, function(x) x@k)
lapply(names(topTermsDist), function(x){
    png(paste0(resultsPath,"/","TopicClustering_byTerms_TopicNumber_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topTermsDist[[x]]), cex=1)
    dev.off()
})

topDocGamma<-lapply(models, function(x) {
    y<-as.matrix(x@gamma)
    colnames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

topDocDist<-lapply(topDocGamma, function(x){
    dist(t(x),method="cosine")
})
names(topDocDist)<-lapply(models, function(x) x@k)

lapply(names(topDocDist), function(x){
    png(paste0(resultsPath,"/","TopicClustering_byDoc_TopicNumber_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topDocDist[[x]]), cex=1)
    dev.off()
})

#######
#Network Output
#######

meta(abstrCorpus, "Type")<-"Abstract"
write.table(file = "Network/AbstrNodeAttrs.csv", x=meta(abstrCorpus)[,c("PMID","FY","FY.Q","Type")],sep=",",
            quote=F, row.names=F, col.names=T)

spMeta<-as.data.frame(cbind(names(spCorpus), "StrategicPlan"))
colnames(spMeta)<-c("ID","Type")
write.table(spMeta, "Network/SpNodeAttrs.csv",sep=",",
            quote=F, row.names=F, col.names=T)

lapply(models, function(x) {
    dir.create(paste0("Network/Topics", x@k))    
    path<-paste0("Network/Topics", x@k)
    topicNodes<-as.data.frame(apply(terms(x,4),2,function(z) paste(z,collapse="|")))
    colnames(topicNodes)<-"TopicWords"
    topicNodes$ID<-rownames(topicNodes)
    write.table(topicNodes, paste0(path,"/TopicNodes.csv"), row.names=F,sep=",", quote=F)
})

lapply(topDocGamma, function(x){
    path<-paste0("Network/Topics", ncol(x))
    colnames(x)<-paste(rep("Topic",ncol(x)),seq(1,ncol(x)))
    x<-as.data.frame(x)
    x$id<-c(meta(abstrCorpus)[-docRemove,"PMID"], names(spCorpus))
    t<-reshape(x, times = colnames(x)[-ncol(x)], varying = colnames(x)[-ncol(x)], direction="long",v.names ="Weight",idvar = "id" ,ids = rownames(x),timevar = "Topic")
    t<-t[t$Weight>0.01,]
    write.table(t, paste0(path, "/TopicDocumentProbEdges.csv"), sep=",",
                          row.names=F, col.names=T,quote=F)
    })

lapply(topTermBeta, function(x){
    path<-paste0("Network/Topics", nrow(x))
    x<-as.data.frame(x)
    x$Topics<-paste(rep("Topic",nrow(x)),seq(1,nrow(x)))
    t<-reshape(x, times = colnames(x)[-ncol(x)], varying = colnames(x)[-ncol(x)], direction="long",v.names ="Weight",idvar = "Topics" ,ids = rownames(x),timevar = "Term")
    t<-t[t$Weight>-250,]
    write.table(t, paste0(path, "/TopicTermWeightEdges.csv"), sep=",",
                row.names=F, col.names=T,quote=F)
})

library(reshape2)

lapply(topDocDist, function(x){
    path<-paste0("Network/Topics", nrow(x))
    x<-as.matrix(x)
    rownames(x)<-seq(1,nrow(x))
    colnames(x)<-seq(1,ncol(x))
    t<-melt(as.matrix(x), varnames=c("col","row"))   
    t<-t[t$row>t$col,]
    t$col<-paste(rep("Topic", nrow(x)), t$col)
    t$row<-paste(rep("Topic", nrow(x)), t$row)
    colnames(t)<-c("Source", "Target","Weight")
    write.table(t,paste0(path,"/TopicTopicbyDocSimilarity.csv"), sep=",", row.names=F, quote=F, col.names=T)    
})

lapply(topTermsDist, function(x){
    path<-paste0("Network/Topics", nrow(x))
    x<-as.matrix(x)
    rownames(x)<-seq(1,nrow(x))
    colnames(x)<-seq(1,ncol(x))
    t<-melt(as.matrix(x), varnames=c("col","row"))   
    t<-t[t$row>t$col,]
    t$col<-paste(rep("Topic", nrow(x)), t$col)
    t$row<-paste(rep("Topic", nrow(x)), t$row)
    colnames(t)<-c("Source", "Target","Weight")
    write.table(t,paste0(path,"/TopicTopicbyTermSimilarity.csv"), sep=",", row.names=F, quote=F, col.names=T)        
})

dtm.df<-as.data.frame(dtm)
dtm.df$id<-c(meta(abstrCorpus)[-docRemove,"PMID"], names(spCorpus))
dtm.df<-reshape(dtm.df, times = colnames(dtm.df)[-ncol(dtm.df)],varying = colnames(dtm.df)[-ncol(dtm.df)], v.names = "TF",idvar = "id",ids = "id",direction = "long")



############
##Abstract SP goals correlations
############

corp<-c(abstrCorpus,spCorpus)

tail(meta(corp), 15)

dtm<-DocumentTermMatrix(corp, control=list(weigthing=weightTfIdf))
dtm.m<-dtm[,apply(as.matrix(dtm)[1296:1306,],2, sum)>0]

dtm.d<-dist(dtm.m, method="euclidean")
l<-dim(dtm)[1]-11
m<-do.call(cbind, lapply(1:l, function(x) dtm.d[(x+(l)):(x+(l+10))]))
#rownames(m)<-rownames(dtm)[1296:1306]
rownames(m)<-c("SP1","SP2","SP3","SP4","SP5","SP6","SP7","SP8","SP9","SP10","SP11")
sp.d<-dist(m)


################
##Parameters
################

#Correlation Threshold for association rule mining and Word grapgh plot
corLimit<-0.1

#Sparsity removes terms that appear infrequently in the matrix must be <1
sparsity<-0.9

#Number of clusters to seed for kmeans clustering.
k<-10

################
##Keywords Dictionary
################
keywords.SP<-read.csv("Keywords_by_SP_Goals.csv", stringsAsFactors=F)
findAssocs(tdm,terms = c("exposome","ewass"), corlimit = corLimit)

#######
##Network of word correlations
#######

sp.tdm<-DocumentTermMatrix(spCorpus,control = list(weighting=weightTfIdf))
g<-similarity.graph(m=dtm.m, vertex.grouping.vars =list(Goal=rownames(dtm.m)),
                    similarity.measure="correlation", min.similarity=0.15)

  


############
##K-means clustering
############
k<-11
l<-dim(as.matrix(dtm))[1]
tdm.t<-t(tdm.monogram.tfidf)
dtm<-DocumentTermMatrix(corp,control=list(weighting=weightTfIdf))
dtm<-dtm[,apply(as.matrix(dtm)[(l-10):l,],2, sum)>0]
kmeansResults<-kmeans(dtm, k)
round(kmeansResults$centers, digits=3)

kResults<-aggregate(as.matrix(dtm), by=list(kmeansResults$cluster), mean)
kClusterResult<-do.call(cbind, lapply(getFactorIdx("FY",meta(corp)),function(x){
    tapply(kmeansResults$cluster[x], kmeansResults$cluster[x], length)    
            }
    )
)
colnames(kClusterResult)<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))
barplot((kClusterResult/colSums(kClusterResult)), names.arg = colnames(kClusterResult))
barplot(t(t(kClusterResult)/colSums(kClusterResult)))

clusplot(as.matrix(dtm), kmeansResults$cluster, color=TRUE, shade=TRUE, labels=TRUE, lines=0)


for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResults$centers[i, ], decreasing = T) 
  cat(names(s)[1:8], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

##K nearest Neighbors Analysis
corp<-c(abstrCorpus,spCorpus)
dtm<-DocumentTermMatrix(corp, control=list(weigthing=weightTfIdf))

l<-dim(as.matrix(dtm))[1]
dtm.m<-dtm[,apply(as.matrix(dtm)[(l-10):l,],2, sum)>0]

knnResults<-lapply(getFactorIdx("FY", meta(abstrCorpus)),
    function(x){    
        knn(as.matrix(dtm.m)[(l-10):l,],as.matrix(dtm.m)[x,], 
          rownames(as.matrix(dtm.m)[(l-10):l,]))
    })

FYs<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))
knnSummary<-do.call(rbind, lapply(knnResults, function(x) tapply(x, x, length)))          
rownames(knnSummary)<-FYs       
          
          
          
          
          
          
          

library(tm)
library(wordcloud)
library(slam)
library(topicmodels)
library(lubridate)
library(parallel)
library(class)
library(cluster)
library(RColorBrewer)
library(gplots)
library(proxy)
library(dendextend)

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
#tdm<-tdm.monogram.tfidf
#tdm<-tdm.bigram

tdm.sp<-TermDocumentMatrix(spCorpus)
tdm.sp.tfidf<-TermDocumentMatrix(spCorpus, control=list(weighting=weightTfIdf))

###########
##TermFreq exploration and visualization
###########

tfidfHisto(tdm.monogram.tfidf ,fact = "FY", "mean")
tfHisto(tdm,"FY")

tf<-rowSums(as.matrix(tdm))
tf<-tf[order(-tf)]

#tf.bi<-do.call(rbind, mclapply(seq(1,dim(tdm.bigram)[1]), mc.cores=16, function(x){
#    rowSums(as.matrix(tdm.bigram[x,]))
#}))

tf.bi<-row_sums(tdm.bigram)

tf.bi<-tf.bi[order(-tf.bi)]

png(paste0(resultsPath,"/Zipfs_plots.png"), height=3000, width=3000, units="px")
par(mfrow=c(2,1), cex=4)
plot(tf.bi, ylim=c(0,12500),lwd=2,type="l", xlab="Rank Ordered Terms", ylab="Term Count", main="Zipf's Law plot")
lines(tf, col="blue", lwd=2)
legend("topright",lty=c(1,1), col=c("black","blue"), legend=c("Bigrams", "Unigrams"), bty="n")

plot(tf.bi, xlim=c(0,10000),lwd=2,ylim=c(0,1000), type="l", xlab="Rank Ordered Terms", ylab="Term Count", main="Zipf's Law plot")
lines(tf, col="blue",lwd=2)
dev.off()


wordCloud(tdm.monogram.tfidf,fact="FY", 75, "mean","tfidf")
wordCloud(tdm.monogram.tfidf,fact="FY.Q", 75, "mean","tfidf")

wordCloud(tdm,fact="FY", 75, pre="tf")
wordCloud(tdm,fact="FY.Q", 75, pre="tf")

wordCloudMontage(tdm = tdm.sp,file = "SP_TF_wordcloud.png", path = resultsPath)
wordCloudMontage(tdm = tdm.sp.tfidf,f=0.001,file = "SP_TfIdf_wordcloud.png", path = resultsPath)


#############
##Topic Modelling
#############
dtm<-DocumentTermMatrix(c(abstrCorpus, spCorpus))
dtm<-t(t(as.matrix(dtm))[as.vector(apply(t(as.matrix(dtm)), 1, sum)>15),])

docRemove<-which(rowSums(dtm)==0)
dtm<-dtm[-docRemove,]

seq.k<-c(25,50,100)

#models<-mclapply(seq.k, mc.cores = 4, function(k) LDA(dtm, k) )
if(file.exists("LDA_models2015aug24_1041.rda")){
  load("LDA_models2015aug24_1041.rda")
} else{
    models<-mclapply(seq.k, mc.cores=2, function(k) LDA(dtm, k) )
    save(models, file = paste0("LDA_models",getDate(),".rda"))
}

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
dev.off()

topDocGamma<-lapply(models, function(x) {
    y<-as.matrix(x@gamma)
    colnames(y)<-apply(terms(x,4),2,function(z) paste(z,collapse=","))
    y
})

png(paste0(resultsPath, "GammaDistbyTopic.png"), height=600, width=1400, units="px")
boxplot(log10(topDocGamma[[1]]), range = 0, las=2)
dev.off()

topDocDist<-lapply(topDocGamma, function(x){
    dist(t(x),method="correlation")
})
names(topDocDist)<-lapply(models, function(x) x@k)

lapply(names(topDocDist), function(x){
    png(paste0(resultsPath,"/","TopicClustering_byDoc_TopicNumber_",x,".png"), height=1200, width=2400, units="px")
    plot(hclust(topDocDist[[x]]), cex=1)
    dev.off()
})


topDocDist.fy<-lapply(topDocGamma, function(x){
    f<-unique(meta(abstrCorpus)[,"FY"])
    idx<-lapply(f, function(x) which(meta(abstrCorpus)[,"FY"]==x) )
    lapply(idx, function(y) {
        f<-meta(abstrCorpus)[y[1],"FY"]
        dist(t(x[y,]),method="correlation")
        })
    })

names(topDocDist.fy)<-lapply(models, function(x) x@k)

lapply(names(topDocDist.fy), function(x){
    lapply(seq(1,length(topDocDist.fy[[x]])), function(y){
        png(paste0(resultsPath,"/","TopicClustering_byDoc_TopicNumber_",x,"FY",y,".png"), height=1200, width=2400, units="px")
        plot(hclust(topDocDist.fy[[x]][[y]]), cex=1)
        dev.off()        
    }) 
})

dev.off()
######
#FY dendrogram comparisons
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
      tanglegram(common_subtrees_color_branches=TRUE, hang=T,lab.cex=2, xlim=c(1,0.5))
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

#######
#Network Output
#######

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

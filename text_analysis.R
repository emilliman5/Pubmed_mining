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
  abstrCorpus<-makeCorpus("pubmed_result.xml","stopwords.txt", 30)
} else {
  ##read in corpus docs.
  abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
  metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','Date','character','numeric'))
  for (x in c("GrantID","Date", "FY", "FY.Q")) {
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
corp<-c(abstrCorpus,spCorpus)
                 
dtm<-DocumentTermMatrix(corp, control=list(weigthing=weightTf))
dtm.abstr<-DocumentTermMatrix(abstrCorpus, control=list(weighting=weightTf))
dtm.sp<-DocumentTermMatrix(spCorpus, control=list(weighting=weightTf))

l<-dim(dtm)[1]

lda.sp<-LDA(dtm[(l-10):l,], 10)
lda.sp.predict<-posterior(lda.sp, dtm[-c((l-10):l),])

trainSet<-do.call(c,lapply(1:10, function(x) which(lda.sp.predict[[2]][,x]>0.97)))

dtm.sp2<-dtm[c(trainSet,(l-10):l),]
lda.sp2<-LDA(dtm.sp2,10)

lda.sp.predict2<-posterior(lda.sp2, dtm[-c((l-10):l),])
lapply(1:10, function(x){
    plot(density(lda.sp.predict2[[2]][lda.sp.predict2[[2]][,x]>0.01,x], bw=0.001),col="blue", main=paste0("Posterior Probabilities for topic ",x))
    lines(density(lda.sp.predict[[2]][lda.sp.predict[[2]][,x]>0.01,x],bw = 0.001), col="red")
  })

png(paste(resultsPath, "LDA_iteration_scatter.png", sep="/"), height=2400, width=1800, units="px")
par(mfrow=c(5,2))
lapply(1:10, function(x) smoothScatter(lda.sp.predict[[2]][,x], 
                                       lda.sp.predict2[[2]][,x], xlab="First Prediction Pass", 
                                       bandwidth = 0.0125, ylab="Second Prediction", colramp=colorRampPalette(c("white", "lightblue", "blue", "orange", "orangered2")),main=paste0("Topic Model ",x)))
dev.off()

lda.results<-lapply(getFactorIdx("FY", meta(abstrCorpus)),
                                   function(x){
                                       lda.sp.predict2[[2]][x,]
                                   })
names(lda.results)<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))

lda.correlation<-lapply(lda.results, function(x) cor(x))

png(paste(resultsPath,"Topic_Correlations.png", sep="/"), height=2500, width=1250, units="px")
lapply(seq_along(lda.correlation), function(y, n, i) {heatmap.2(y[[i]],trace = "none", 
                                  col=colorRampPalette(rev(brewer.pal(9, "RdBu"))), main = paste0("FY",n[[i]]))}, y=lda.correlation, n=names(lda.correlation))
dev.off()

l<-length(topics(lda))
lda.sp<-topics(lda)[(l-10):l]

lda.summary<-do.call(rbind,lapply(lda, function(x) tapply(topics(x), topics(x), length)))
rownames(lda.summary)<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))
lapply(lda, function(x) terms(x,3))








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
corLimit<-0.15

#Sparsity removes terms that appear infrequently in the matrix must be <1
sparsity<-0.9

#Number of clusters to seed for kmeans clustering.
k<-10

################
##Keywords Dictionary
################
keywords.SP<-read.csv("Keywords_by_SP_Goals.csv", stringsAsFactors=F)
findAssocs(tdm,terms = c("puberty", "pregnancy","lactation"), corlimit = corLimit)

#######
##Network of word correlations
#######

sp.tdm<-DocumentTermMatrix(spCorpus,control = list(weighting=weightTfIdf))
g<-similarity.graph(m=dtm.m, vertex.grouping.vars =list(Goal=rownames(dtm.m)),
                    similarity.measure="correlation", min.similarity=0.15)

  
###########
##Hierchical Clustering and Dendrogram
###########

tdm2<-removeSparseTerms(tdm, sparse = sparsity)
tdm2.m<-t(as.matrix(tdm))
distMatrix<-dist(dist(scale(tdm2.m)))
fit<-hclust(distMatrix,method = "ward.D")

png(paste0(resultsPath,"/Word_dendrogram.png"), height=4500, width=9000, units="px")
plot(fit, cex=0.75)
dev.off()

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
          
          
          
          
          
          
          

library(tm)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(topicmodels)
library(lubridate)
library(parallel)
library(networktools)
library(igraph)
library(class)
library(cluster)

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
  metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','Date','numeric'))
  for (x in c("GrantID","Date", "FY")) {
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

###########
##TermFreq exploration and visualization
###########

tfidfHisto(tdm.monogram.tfidf ,fact = "FY", "mean")

tfHisto(tdm,"FY")

wordCloud(tdm.monogram.tfidf,fact="FY", 75, "mean","tfidf")
wordCloud(tdm.monogram.tfidf,fact="FY.Q", 75, "mean","tfidf")

wordCloud(tdm,fact="FY", 75, pre="tf")
wordCloud(tdm,fact="FY.Q", 75, pre="tf")
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
tdm.t<-t(tdm.monogram.tfidf)
dtm<-DocumentTermMatrix(corpse,control=list(weighting=weightTfIdf))
dtm<-dtm[,apply(as.matrix(dtm)[5268:5278,],2, sum)>0]
kmeansResults<-kmeans(dtm, k)
round(kmeansResults$centers, digits=3)

kResults<-aggregate(as.matrix(tdm.t), by=list(kmeansResults$cluster), mean)

clusplot(as.matrix(dtm), kmeansResults$cluster, color=TRUE, shade=TRUE, labels=TRUE, lines=0)


for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResults$centers[i, ], decreasing = T) 
  cat(names(s)[1:8], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

#############
##Topic Modelling
#############
corp<-c(abstrCorpus,spCorpus)
dtm<-DocumentTermMatrix(corp, control=list(weigthing=weightTf))
dtm.abstr<-DocumentTermMatrix(abstrCorpus, control=list(weighting=weightTf))
dtm.sp<-DocumentTermMatrix(spCorpus, control=list(weighting=weightTf))

lda<-LDA(dtm, 12)

lda.summary<-do.call(rbind, lapply(getFactorIdx("FY", meta(abstrCorpus)),
    function(x){
        tapply(topics(lda)[x], topics(lda)[x], length)
    }
))
rownames(lda.summary)<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))
l<-length(topics(lda))
lda.sp<-topics(lda)[(l-10):l]

lda.summary<-do.call(rbind,lapply(lda, function(x) tapply(topics(x), topics(x), length)))
rownames(lda.summary)<-do.call(c, lapply(getFactorIdx("FY", meta(abstrCorpus)), function(x) meta(abstrCorpus)[x[1],"FY"]))
lapply(lda, function(x) terms(x,3))

sp.lda<-LDA(dtm.sp,11)

sp.lda.predict<-lapply(getFactorIdx("FY", meta(abstrCorpus)),
            function(x){
                LDA(dtm[x,], 12, lda.sp)
            }
        )



topic.assign<-posterior(lda.sp, dtm.abstr)


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
          
          
          
          
          
          
          

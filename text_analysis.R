library(tm)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(topicmodels)
library(lubridate)
library(parallel)

#If you want to force a reprocessing of the documents into a Corpus set this value to "TRUE"
reset<-TRUE

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

tfidfHisto(tdm.monogram.tfidf ,fact = "FY", "max")

tfHisto(tdm,"FY")

wordCloud(tdm.monogram.tfidf,fact="FY", 75, "mean")

wordCloud(tdm,fact="FY", 50)


############
##Abstract SP goals correlations
############

corpse<-c(abstrCorpus,spCorpus)

tail(meta(corpse), 15)

dtm<-DocumentTermMatrix(corpse, control=list(weigthing=weightTfIdf))
dtm<-dtm[,apply(as.matrix(dtm)[1296:1306,],2, sum)>0]

dtm.d<-dist(dtm, method="euclidean")
m<-lapply(1:5267, function(x) dtm.d[])
m<-do.call(cbind, lapply(1:5267, function(x) dtm.d[(x+5267):(x+5277)]))

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

freq.terms<-findFreqTerms(tdm,lowfreq = low)
png(paste0(resultsPath,"/Word_graph.png"), height=2400, width=3200, units="px")
plot(tdm, term=freq.terms, corThreshold = corLimit, weighting=F)
dev.off()

##Word cloud
  
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

tdm.t<-t(tdm)
kmeansResults<-kmeans(tdm.t, k)
round(kmeansResults$centers, digits=3)

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
dtm<-as.DocumentTermMatrix(tdm)
lda<-LDA(dtm, 12)
(topics<-terms(lda,6))

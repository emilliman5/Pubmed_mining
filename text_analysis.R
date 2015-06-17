library(tm)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(topicmodels)
library(lubridate)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

if(!file.exists("Corpus/1.txt") || reset){
  ##call function to read and process corpus
} else {
  ##read in corpus docs.
  abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
  metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','Date','numeric'))
  for (x in c("GrantID","Date", "FY")) {
    meta(abstrCorpus, x)<-metaData[,x]
  }
}


spCorpus<-Corpus(DirSource("Corpus/SP/"), readerControl = list(language="english"))

abstrCorpus<-c(abstrCorpus, spCorpus)

################
##Term Document Matrix Creation
################

#This is the basic data structure to mine for term usage trends, clustering, association rule mining, etc.
tdm.monogram<-TermDocumentMatrix(abstrCorpus, 
                                 control = list(weighting=weightTfIdf))

#################
##Ngram Analysis
#################

tdm.bigram <- TermDocumentMatrix(abstrCorpus, control = list(tokenize = NgramTokenizer))
##function(x) weightTfIdf(x,normalize=F)))

##Run one of the following commands before proceding. 
##tdm.monogram are single term frequencies.
##tdm.bigram are two term frequencies

tdm<-tdm.monogram
tdm<-tdm.bigram

###########
##TermFreq exploration
###########
tdm

tdm.m<-as.matrix(tdm)
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
idf<-apply(tdm.m, 1, function(x) length(x)/sum(x>=1))

png(file.path(resultsPath,"TermFreq_Distributions.png", fsep = "/"), height=4000, width=2400, units="px")
par(mfrow=c(3,1), cex=4)
hist(sqrt(rowSums(tdm.m)), breaks=100, col="blue4", main="Distribution of terms in Corpus")
hist(sqrt(rowMeans(tdm.m)), breaks=100, col="red", main="Distribution of Avg. term usage per Document in Corpus")
hist(idf, breaks=100, col="green",main="Distribution of idf Scores")
dev.off()


################
##Parameters
################

#Frequency Threshold; term are retained with they have a frequency above this value
low<-quantile(rowSums(as.matrix(tdm)), probs = 0.99)

paste0("Number of terms with a total frequency greater than ", round(low,0),": ", sum(rowSums(as.matrix(tdm))>low))

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

sapply(as.factor(meta(abstrCorpus)[,"FY"]), function(x) {
  tdm.s<-tdm[,which(as.factor(meta(abstrCorpus)[,"FY"])==x)]
  tdm.df<-data.frame(word=rownames(tdm.s), freq=tdm.s)
  png(paste0(resultsPath,"/",x,"_wordCloud.png"), height=1600, width=1600, units="px")
  wordcloud(tdm.df$word, tdm.df$freq, scale=c(15,0.5), min.freq = low, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
  dev.off()
})
  
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

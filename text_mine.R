library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)
library(topicmodels)
library(lubridate)

# setwd("~/workspace/Pubmed_mining/")

reset=FALSE

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

fiscalYear.start<-10
fiscalYear.end<-9

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

if(!file.exists("Corpus/1.txt") || reset){
  
  stopWords<-read.table("stopwords.txt")
  myStopwords<-c(stopwords('english'), stopWords$V1)
  
  pubmed<-xmlParse("pubmed_result.xml",useInternalNodes = T)
  top<-xmlRoot(pubmed)
  
  nodes<-getNodeSet(top,"//PubmedData/History/PubMedPubDate[@PubStatus='pubmed']")
  pubdate.df<-sapply(nodes, function(x)  paste(xmlSApply(x, xmlValue)[1:3], collapse = "-"))
  
  abstr.df<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/Article", function(node)
  {
    grantID<-xmlValue(node[['GrantList']][['Grant']][['GrantID']])
    title<-xmlValue(node[['ArticleTitle']])
    abstr<-xmlValue(node[['Abstract']][['AbstractText']])
    data.frame("GrantID"=grantID, "Title"=title, "Abstract"=abstr, stringsAsFactors=F)
  } ))
  
  abstr.df<-cbind(pubdate.df, abstr.df)
  abstrCorpus<-Corpus(DataframeSource(abstr.df[,3:4]))
  
  meta(abstrCorpus, "GrantID")<-abstr.df[,"GrantID"]
  meta(abstrCorpus, "Date")<-abstr.df[,"pubdate.df"]
  
  abstrCorpus<-tm_map(abstrCorpus, function(x){
    meta(x, "GrantID")<-LETTERS[round(runif(26))]
    meta(x, "Date")<-"2015-06-10"
    x
    })
  
  # keywords<-xpathApply(top, "//KeywordList", xmlValue)
  # keywords.df<-do.call("rbind",keywords)
  # #abstrCorpus<-Corpus(DataframeSource(keywords.df))
  # 
  # mesh<-xpathApply(top, "//MeshHeadingList", xmlValue)
  # #mesh.df<-do.call("rbind",mesh)
  
  abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
  abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
  abstrCorpus<-tm_map(abstrCorpus, removeNumbers)
  abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
  dictCorpus<-abstrCorpus
  abstrCorpus<-tm_map(abstrCorpus, stemDocument)
  abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
  
  abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=24)
  abstrCorpus<-Corpus(VectorSource(abstrCorpus))
  
  dir.create("Corpus")
  writeCorpus(abstrCorpus,"Corpus/")
} else {
  ##read in corpus docs.
  abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
  }

################
##Term Document Matrix
################

#This is the basic data structure to mine for term usage trends, clustering, association rule mining, etc.
tdm.monogram<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[100:200,1:10])

#################
##Ngram Analysis
#################

tdm.bigram <- TermDocumentMatrix(abstrCorpus, control = list(tokenize = NgramTokenizer))
inspect(removeSparseTerms(tdm.bigram[, 1:10], sparsity))
inspect(tdm.bigram)




################
##Term Usage Analysis
################

###Paramters for the analyses below

#Frequency Threshold; term are retained with they have a frequency above this value
low<-quantile(rowSums(tdm.m), probs = 0.99)

#Correlation Threshold for association rule mining and Word grapgh plot
corLimit<-0.1

#Sparsity removes terms that appear infrequently in the matrix must be <1
sparsity<-0.9

#Number of clusters to seed for kmeans clustering.
k<-10


keywords.SP<-read.csv("Keywords_by_SP_Goals.csv")
findAssocs(tdm,terms = c("puberty", "pregnancy","lactation"), corlimit = corLimit)

###########
##TermFreq exploration
###########
tdm.m<-as.matrix(tdm)
tdm.m.s<-tdm.m[sort(rowSums(tdm.m), decreasing = T),]
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
myNames<-names(tdm.s)
idf<-apply(tdm.m, 1, function(x) length(x)/sum(x>=1))

png(file.path(resultsPath,"TermFreq_Distributions.png", fsep = "/"), height=4000, width=2400, units="px")
par(mfrow=c(3,1), cex=4)
hist(log2(rowSums(tdm.m)), breaks=100, col="blue4", main="Distribution of terms in Corpus")
hist(log2(rowMeans(tdm.m)), breaks=100, col="red", main="Distribution of Avg. term usage per Document in Corpus")
hist(idf, breaks=100, col="green",main="Distribution of idf Scores")
dev.off()

#######
##Network of word correlations
#######
(freq.terms<-findFreqTerms(tdm,lowfreq = low))
png(paste0(resultsPath,"/Word_graph.png"), height=2400, width=3200, units="px")
plot(tdm, term=freq.terms, corThreshold = corLimit, weighting=T)
dev.off()

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png(paste0(resultsPath,"/wordCloud.png"), height=1600, width=1600, units="px")
wordcloud(tdm.df$word, tdm.df$freq, scale=c(15,0.5), min.freq = low, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
dev.off()

###########
##Hierchical Clustering and Dendrogram
###########

tdm2<-removeSparseTerms(tdm, sparse = sparsity)
tdm2.m<-as.matrix(tdm2)
distMatrix<-dist(dist(scale(tdm2.m)))
fit<-hclust(distMatrix,method = "ward.D")

png(paste0(resultsPath,"/Word_dendrogram.png"), height=800, width=1200, units="px")
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

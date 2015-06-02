library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)
library(topicmodels)

# setwd("~/workspace/Pubmed_mining/")

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

fiscalYear.start<-10
fiscalYear.end<-9

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

pubmed<-xmlTreeParse("~/Downloads/pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract/AbstractText", xmlValue)
titles<-xpathApply(top, "//MedlineCitation/Article/ArticleTitle", xmlValue)
pubdate<-xpathApply(top, "//PubmedData/History/PubMedPubDate[@PubStatus='pubmed']", xmlValue)
pubdate.df<-do.call("rbind", lapply(pubdate, function(x)  paste(xmlSApply(x, xmlValue)[1:3], sep = "-")))
grantID<-xpathApply(top, "//MedlineCitation/Article/GrantList/Grant/GrantID", xmlValue)

abstr.df<-do.call("rbind", abstr)
abstrCorpus<-Corpus(DataframeSource(abstr.df))

keywords<-xpathApply(top, "//KeywordList", xmlValue)
keywords.df<-do.call("rbind",keywords)
abstrCorpus<-Corpus(DataframeSource(keywords.df))

mesh<-xpathApply(top, "//MeshHeadingList", xmlValue)
mesh.df<-do.call("rbind",mesh)
#abstrCorpus<-Corpus(DataframeSource(mesh.df))

abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
abstrCorpus<-tm_map(abstrCorpus, removeNumbers)

stopWords<-read.table("stopwords.txt")
myStopwords<-c(stopwords('english'), stopWords$V1)

abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
dictCorpus<-abstrCorpus
abstrCorpus<-tm_map(abstrCorpus, stemDocument)
abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
inspect(abstrCorpus[1:3])

abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=24)
abstrCorpus<-Corpus(VectorSource(abstrCorpus))

tdm<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[100:200,1:10])

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

################
##Some basic analyses
################
low<-quantile(rowSums(tdm.m), probs = 0.99)

(freq.terms<-findFreqTerms(tdm,lowfreq = low))
keywords.SP<-read.csv("Keywords_by_SP_Goals.csv")
findAssocs(tdm,terms = c("puberty", "pregnancy","lactation"), corlimit = 0.1)


#######
##Network of word correlations
#######

png(paste0(resultsPath,"/Word_graph.png"), height=2400, width=3200, units="px")
plot(tdm, term=freq.terms, corThreshold = 0.2, weighting=T)
dev.off()

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png(paste0(resultsPath,"/wordCloud.png"), height=1600, width=1600, units="px")
wordcloud(tdm.df$word, tdm.df$freq, scale=c(15,0.5),min.freq = low, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
dev.off()

###########
##Hierchical Clustering and Dendrogram
###########

tdm2<-removeSparseTerms(tdm, sparse =0.9)
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
k<-10
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


#################
##Ngram Analysis
#################

tdm.bigram <- TermDocumentMatrix(abstrCorpus, control = list(tokenize = NgramTokenizer))
inspect(removeSparseTerms(tdm.bigram[, 1:10], 0.9))
inspect(tdm.bigram)

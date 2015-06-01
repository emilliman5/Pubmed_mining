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

dir.create("results/")
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

pubmed<-xmlTreeParse("pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract", xmlValue)
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

abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=8)
abstrCorpus<-Corpus(VectorSource(abstrCorpus))

tdm<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[100:200,1:10])

################
##Some basic analyses
################

findFreqTerms(tdm,lowfreq = 10)
findAssocs(tdm,terms = c("particulate", "toxic", "air"), corlimit = 0.25)

tdm.m<-as.matrix(tdm)
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
myNames<-names(tdm.s)

#######
##Network of word correlations
#######

term.freq<-subset(tdm.s, tdm.s>=245)
freq.terms<-findFreqTerms(tdm, lowfreq=245)
png(paste0(resultsPath,"/Word_graph.png"), height=2400, width=3200, units="px")
plot(tdm, term=freq.terms, corThreshold = 0.1, weighting=T)
dev.off()

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png(paste0(resultsPath,"/wordCloud.png"), height=1600, width=1600, units="px")
wordcloud(tdm.df$word, tdm.df$freq, min.freq = 245, colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
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

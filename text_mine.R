library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)

# setwd("~/workspace/Pubmed_mining/")

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

dir.create("results/")
dir.create(paste0("results/",getDate()))
resultsPath<-paste0("results/",getDate())

pubmed<-xmlTreeParse("pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract", xmlValue)
abstr.df<-do.call("rbind", abstr)

abstrCorpus<-Corpus(DataframeSource(abstr.df))

keywords<-xpathApply(top, "//KeywordList", xmlValue)
keywords<-do.call("rbind",keywords)

mesh<-xpathApply(top, "//MeshHeadingList", xmlValue)
mesh.df<-do.call("rbind",mesh)
#abstrCorpus<-Corpus(DataframeSource(mesh.df))

abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
abstrCorpus<-tm_map(abstrCorpus, removeNumbers)

myStopwords<-c(stopwords('english'), "available", "via")

#a specific set of stopwords will need to be developed for this analysis

abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
dictCorpus<-abstrCorpus
abstrCorpus<-tm_map(abstrCorpus, stemDocument)
abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
inspect(abstrCorpus[1:3])

tmpCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=4)
tmpCorpus<-Corpus(VectorSource(tmpCorpus))
abstrCorpus<-tmpCorpus
#inspect(abstrCorpus[1:3])

tdm<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[100:200,1:10])

################
##Some basic analyses
################

findFreqTerms(tdm,lowfreq = 50)
findAssocs(tdm,terms = c("tumorcell", "cell", "pollutants"), corlimit = 0.25)

tdm.m<-as.matrix(tdm)
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
myNames<-names(tdm.s)

term.freq<-subset(tdm.s, tdm.s>=50)
freq.terms<-findFreqTerms(tdm, lowfreq=50)
png(paste0(resultsPath,"/Top25_word_graph.png"), height=1200, width=1600, units="px")
plot(tdm, term=freq.terms, corThreshold = 0.1, weighting=T)
dev.off()

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png(paste0(resultsPath,"/wordCloud.png"), height=800, width=800, units="px")
wordcloud(tdm.df$word, tdm.df$freq, min.freq = 20, colors=brewer.pal(9, "BuGn"), random.order=F)
dev.off()

tdm2<-removeSparseTerms(tdm, sparse = 0.99)
tdm2.m<-as.matrix(tdm2)
distMatrix<-dist(dist(scale(tdm2.m)))
fit<-hclust(distMatrix,method = "ward.D")
png(paste0(resultsPath,"/Word_dendrogram.png"), height=800, width=1200, units="px")
plot(fit, cex=0.75)
dev.off()

library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(snow)

##To Do:
#1. Troubleshoot word stem completion
#2. Create custom stopwords dictionary (common words that are not meaningful)
#3. Test data scaling
#4. Pair some metadata with abstracts; Authors, title, institution???
#5. retain chemical names (should numbers be removed or kept?)
#6. Run analysis on MeSH terms
#7. Generate word association graphs
#8. Use n-grams
#9. Create dictionary of relevant terms

setwd("~/workspace/Pubmed_mining/")

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

dir.create(paste0("results/",getDate()))
resultsPath<-paste0("results/",getDate())

pubmed<-xmlTreeParse("../../Downloads/pubmed_result.xml",useInternalNodes = T)
top<-xmlRoot(pubmed)

abstr<-xpathApply(top, "//MedlineCitation/Article/Abstract", xmlValue)
abstr.df<-do.call("rbind", abstr)

abstrCorpus<-Corpus(DataframeSource(abstr.df))

abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
abstrCorpus<-tm_map(abstrCorpus, removeNumbers)

myStopwords<-c(stopwords('english'), "available", "via")

#a specific set of stopwords will need to be developed for this analysis

##"r" does not exist in the stopwords vector...
#myStopwords<-myStopwords[-which(myStopwords=="r")]

abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
abstrCorpus<-tm_map(abstrCorpus, stemDocument)
dictCorpus<-abstrCorpus
abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
inspect(abstrCorpus[1:3])

##stemCompletion breaks corpus...
tmpCorpus<-parLapply(abstrCorpus, stemCompletion_mod)
#inspect(abstrCorpus[1:3])


tdm<-TermDocumentMatrix(abstrCorpus)
inspect(tdm[1:10,1:10])

################
##Some basic analyses
################

findFreqTerms(tdm,lowfreq = 250)
findAssocs(tdm,terms = c("human", "risk", "exposur"), corlimit = 0.25)

tdm.m<-as.matrix(tdm)
tdm.s<-sort(rowSums(tdm.m), decreasing = T)
myNames<-names(tdm.s)

term.freq<-subset(tdm.s, tdm.s>=500)
freq.terms<-findFreqTerms(tdm, lowfreq=500)
png(paste0(resultsPath,"Top25_word_graph.png"), height=800, width=1200, units="px")
plot(tdm, term=freq.terms, corThreshold = 0.1, weighting=T)
dev.off()

##Word cloud :-)

tdm.df<-data.frame(word=myNames, freq=tdm.s)
png(paste0(resultsPath,"wordCloud.png"), height=800, width=800, units="px")
wordcloud(tdm.df$word, tdm.df$freq, min.freq = 250, colors=brewer.pal(9, "BuGn"), random.order=F)
dev.off()

tdm2<-removeSparseTerms(tdm, sparse = 0.9)
tdm2.m<-as.matrix(tdm2)
distMatrix<-dist(dist(scale(tdm2.m)))
fit<-hclust(distMatrix,method = "ward.D")
png(paste0(resultsPath,"Word_dendrogram.png"), height=800, width=1200, units="px")
plot(fit, cex=0.75)
dev.off()

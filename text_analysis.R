library(tm)
library(wordcloud)
library(slam)
library(lubridate)
library(parallel)
library(proxy)

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

####Extra Corpus cleaning


###Descriptives of Corpus
png(paste0(resultsPath, "/Abstracts_per_FY.png"), height=1000, width=1200, units="px")
    par(mfrow=c(2,1), cex=2)
    barplot(tapply(meta(abstrCorpus)[,"FY"], meta(abstrCorpus)[,"FY.Q"],length), main="Abstracts per FY.Q", las=2)
    barplot(tapply(meta(abstrCorpus)[,"FY"], meta(abstrCorpus)[,"FY"],length ), main="Abstracts per FY", las=2)
dev.off()

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

##This is probably an inappropriate graphic as tf-idf does not summarize well across the corpus
##tf-idf is really a measure of a words importance in a document
#wordCloud(tdm.monogram.tfidf,fact="FY", 75, "mean","tfidf")
#wordCloud(tdm.monogram.tfidf,fact="FY.Q", 75, "mean","tfidf")

wordCloud(tdm,fact="FY", 75, pre="tf")
wordCloud(tdm,fact="FY.Q", 75, pre="tf")

wordCloudMontage(tdm = tdm.sp,file = "SP_TF_wordcloud.png", path = resultsPath)
#wordCloudMontage(tdm = tdm.sp.tfidf,f=0.001,file = "SP_TfIdf_wordcloud.png", path = resultsPath)

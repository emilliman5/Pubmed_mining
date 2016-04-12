#!/usr/bin/Rscript
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(slam))
suppressMessages(library(lubridate))
suppressMessages(library(parallel))
suppressMessages(library(proxy))
suppressMessages(library(docopt))

doc<-"This script does an initial cleaning and analysis of a set of 
        documents (the corpus). It will ouptut a series of plots to 
        describe the vocabulary as well as a create corpus that can be used by 
        for topicmodeling by the topic_modeling.R

Usage:  text_analysis.R -x=<pubmed> [-r=<nih>] -d=<dir> [-s=<stopwords>] [-c=<cores>] [--reset]
        text_analysis.R [-x=<pubmed>] -r=<nih> -d=<dir> [-s=<stopwords>] [-c=<cores>] [--reset]

Options:
    -x --xml=<pubmed>           Pubmed results in XML format
    -r --reporter=<nih>         NIH Reporter export in CSV format
    -s --stopwords=<stopwords>  Stop word list, one word per line, plain text [default: stopwords.txt]
    -d --dir=<dir>              Directory to write Corpus and meta data outputs
    -c --cores=<cores>          Number of cores to use for Corpus processing [default: 16]
    --reset                     Force a reprocessing of the Corpus, the default is to not reprocess the corpus if one exists
    -h --help                   This helpful message"

my_opts<-docopt(doc)
print(my_opts)    ##This is for testing purposes

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}
source("makeCorpus.R")

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)
corpusPath<-paste0("data/",my_opts$dir)
dir.create(corpusPath, recursive = T, showWarnings = F)

file.copy(from=my_opts$xml,to=paste0(corpusPath))
print(c("XML file is null:",!is.null(my_opts$xml)))

if(!is.null(my_opts$xml)){
    print("Processing Corpus....")
    pubmed.df<-pubmedParse(my_opts$xml)
    metaData<-pubmed.df[,1:5]
    metaData[,"FY.Q"]<-quarter(pubmed.df[,"pubdate.df"]+91, with_year=T)
    metaData[,"FY"]<-floor(metaData[,"FY.Q"])
    abstrCorpus<-makeCorpus(abstr.df = pubmed.df,stopwordsList = my_opts$stopwords, cores = my_opts$cores)
    writeCorpus(abstrCorpus, paste0(corpusPath,"/Corpus"))
    write.csv(metaData, file=paste0(corpusPath,"/CorpusMetaData.txt"), header=T)
} else {
  ##read in corpus docs.
    print("Loading previous corpus...")
    abstrCorpus<-Corpus(DirSource(paste0(corpusPath,"/Corpus")), 
                                readerControl = list(language="english"))
    metaData<-read.csv(paste0(corpusPath, "CorpusMetaData.txt"),colClasses=c('character','character','Date','character','numeric'))
    for (x in c("PMID","GrantID","Date", "FY", "FY.Q")) {
        meta(abstrCorpus, x)<-metaData[,x]
    }
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

###########
##TermFreq exploration and visualization
###########

tfidfHisto(tdm.monogram.tfidf ,fact = "FY", "mean")
tfHisto(tdm,"FY")

tf<-rowSums(as.matrix(tdm))
tf<-tf[order(-tf)]

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

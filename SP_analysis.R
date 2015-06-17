library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)
library(topicmodels)

# setwd("~/workspace/Pubmed_mining/")
# setwd("~/Pubmed_mining/")

#3 Set the number of cores available on your computer
cores<-30


extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate(),"/StrategicPan/")
dir.create(resultsPath, recursive = T)

files<-list.files(pattern = "SP_",path="data/Strategic_goals/",full.names = T)
files<-files[grep("Goal", files)]

docs<-do.call("rbind", lapply(files, function(x) paste(readLines(file(x)), collapse=" ")))
docs<-cbind(gsub("data/Strategic_goals//","",gsub(".txt", "",files)), docs)

SP<-VCorpus(VectorSource(docs[,2]))

SP<-tm_map(SP, removePunctuation)
SP<-tm_map(SP, removeNumbers)
stopWords<-read.table("stopwords.txt", colClass=c("character"))
myStopwords<-c(stopwords('english'), stopWords$V1)

SP<-tm_map(SP, removeWords, myStopwords)
dictCorpus<-SP
SP<-tm_map(SP, stemDocument)
SP<-tm_map(SP, stripWhitespace)
SP<-tm_map(SP,content_transformer(tolower))

SP<-mclapply(SP, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
SP<-Corpus(VectorSource(SP))

dir.create("Corpus/SP/")
writeCorpus(SP,path = "Corpus/SP")

tdm<-TermDocumentMatrix(SP)
colnames(tdm)<-docs[,1]

wordCloudMontage(tdm, "SP_wordCloud_tf.png",resultsPath, c(4,6))

tdm.tfidf<-TermDocumentMatrix(SP, control = list(weighting=weightTfIdf))
colnames(tdm.tfidf)<-docs[,1]

wordCloudMontage(tdm.tfidf, "SP_wordCloud_tfidf.png",resultsPath, c(4,6), f=0)

tdm2<-removeSparseTerms(tdm.tfidf, sparse =0.85)
tdm2.m<-as.matrix(tdm2)
dtm2<-t(as.matrix(tdm2))

row.names(tdm2.d)<-docs[,1]
dist.w<-dist(scale(tdm2.w))
dist.d<-dist(scale(dtm2))
fit.w<-hclust(dist.w, method = "ward.D")
fit.d<-hclust(dist.d, method = "ward.D")

png(paste0(resultsPath,"StrategicPlan_dendrogram.png"), height=800, width=1200, units="px")
par(mfrow=c(2,1))
plot(fit.d)
plot(fit.w, cex=0.75)
dev.off()

heatmap(tdm2.m)



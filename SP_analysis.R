library(XML)
library(tm)
library(SnowballC)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(parallel)
library(topicmodels)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
  source(extraFunFile, keep.source=TRUE);
}

files<-list.files(pattern = "SP_",path="data/Strategic_goals/",full.names = T)
files<-files[grep("Goal", files)]

docs<-do.call("rbind", lapply(files, function(x) paste(readLines(file(x)), collapse=" ")))
docs<-cbind(gsub("data/Strategic_goals//","",gsub(".txt", "",files)), docs)

SP<-VCorpus(VectorSource(docs[,2]))
SP<-tm_map(SP, removePunctuation)
SP<-tm_map(SP, removeNumbers)
stopWords<-read.table("stopwords.txt")
myStopwords<-c(stopwords('english'), stopWords$V1)

SP<-tm_map(SP, removeWords, myStopwords)
dictCorpus<-SP
SP<-tm_map(SP, stemDocument)
SP<-tm_map(SP, stripWhitespace)
SP<-tm_map(SP,content_transformer(tolower))

SP<-mclapply(SP, stemCompletion2, dictionary=dictCorpus, mc.cores=24)
SP<-Corpus(VectorSource(SP))

writeCorpus(SP)

tdm<-TermDocumentMatrix(SP,)
colnames(tdm)<-docs[,1]
inspect(tdm[1:20,])


png(paste0(resultsPath,"/StrategicPlan_wordcloud.png"), height=4000, width=6000, units="px")
par(mfrow=c(3,4))
par()
apply(as.matrix(tdm), 2, function(x) plot.new(); 
      text(0.5, 0.5, colnames(x)); 
      wordcloud(row.names(as.matrix(tdm)), x, min.freq = 1, 
                colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
      )
dev.off()

tdm2<-removeSparseTerms(tdm, sparse =0.85)
tdm2.w<-as.matrix(tdm2)
tdm2.d<-t(as.matrix(tdm2))

row.names(tdm2.d)<-docs[,1]
distMatrix<-dist(scale(tdm2.w))
dist.d<-dist(scale(tdm2.d))
fit.w<-hclust(distMatrix,method = "ward.D")
fit.d<-hclust(dist.d,method = "ward.D")

png(paste0(resultsPath,"/StrategicPlan_dendrogram.png"), height=800, width=1200, units="px")
par(mfrow=c(2,1))
plot(fit.d)
plot(fit.w, cex=0.75)
dev.off()

heatmap(tdm2.m)


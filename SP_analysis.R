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
inspect(tdm[1:20,])


png("StrategicPlan_wordClouds.png", height=3500, width=6000, units="px")
layout(matrix(seq(1,24), nrow=4), heights=c(1,8,1,8))
par(mar=rep(0,4))
sapply(colnames(as.matrix(tdm)), function(x){ 
  plot.new()
  text(x=0.5, y=0.5, x, cex=8)
  wordcloud(row.names(as.matrix(tdm)), as.matrix(tdm)[,x], min.freq = 1, scale=c(12,0.5), 
            colors=brewer.pal(9, "BuGn")[-(1:4)], random.order=F)
    }
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


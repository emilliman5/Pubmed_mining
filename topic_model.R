suppressMessages(library(tm))
suppressMessages(library(topicmodels))
suppressMessages(library(parallel))
suppressMessages(library(ggplot2))
suppressMessages(library(docopt))

doc<-"  This script performs topic modeling on a Corpus of documents

Usage:  topic_modeling.R --corpus=<corpus> -d=<dir> [-m=<k>] [-c=<cores>] [--Remodel]

Options:
    --corpus=<corpus>           Path to corpus files [default: data/Corpus/]
    -m --models=<k>             Number of topics to model on, separate values by a comma only [default: 50,100,250,500,1000]
    -c --cores=<cores>          Number of cores to use for Corpus processing [default: 6] [default: 16]
    --Remodel                   Do not perform topic modeling the corpus (Useful if you have topic models and just want some)
    -h --help                   This helpful message"

my_opts<-docopt(doc)
print(my_opts)

seq.k<-unlist(strsplit(my_opts$k, ","))
seq.k<-seq.k[order(seq.k, decreasing=T)]    ##starts modeling on the large k first for slightly better efficiency

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

abstrCorpus<-Corpus(DirSource(paste0(my_opts$corpus, "/Corpus/")), 
                    readerControl = list(language="english"))
metaData<-read.csv(paste0(my_opts$corpus,"/CorpusMetaData.txt"),colClasses=c('character','character','Date','numeric','integer', 'character','character'))

###Make sure that the Corpus and metaData are in the same order
names(abstrCorpus)<-gsub(".txt", "", names(abstrCorpus))
idx<-unlist(lapply(names(abstrCorpus), function(x) which(metaData$PMID==x)))
metaData<-metaData[idx,]
write.csv(paste0(my_opts$corpus,"/CorpusMetaData.txt"))

for (x in colnames(metaData)) {
    meta(abstrCorpus, x)<-metaData[,x]
}

dtm<-DocumentTermMatrix(c(abstrCorpus, spCorpus))
TermDocFreq<-colSums(as.matrix(dtm)>0)
TermFreq<-colSums(as.matrix(dtm))
dtm<-as.matrix(dtm)[,TermFreq>15]

docRemove<-rowSums(dtm)==0
meta(abstrCorpus, "InModel")<-!docRemove[1:length(metaData$PMID)]
write.csv(meta(abstrCorpus)[meta(abstrCorpus)$InModel==TRUE,], paste0(my_opts$corpus,"models/ModelsMetaData.txt"),row.names=F)
dtm<-dtm[-docRemove,]
rownames(dtm)<-c(meta(abstrCorpus)[-docRemove,1])

#models<-mclapply(seq.k, mc.cores = 4, function(k) LDA(dtm, k) )
if(file.exists("LDA_models_current.rda") & !model){
    load("LDA_models_current.rda")
} else{
    models<-mclapply(seq.k, mc.cores=2, function(k) LDA(dtm, k) )
    save(models, file = paste0(my_opts$corpus,"models/LDA_models",getDate(),".rda"))
    save(models, file = paste0("data/LDA_models_current.rda"))
    lapply(models, function(x) write.csv2(t(terms(x, 10)), file=paste0(my_opts$corpus,"/models/TopicKeywords/Top10WordsperTopic_for_",x@k,"Topics_model.txt")))
}

if(file.exists("CTM_LDA_models.rda") & !model){
    load("CTM_LDA_models.rda")
} else{
    ctm.models<-mclapply(seq.k, mc.cores=4, function(k) CTM(dtm,k))
    save(ctm.models, file = paste0("CTM_LDA_models",getDate(),".rda"))
    save(ctm.models, file = paste0("CTM_LDA_models_current.rda"))
    }

fy<-levels(as.factor(meta(abstrCorpus)[,"FY"]))

if(file.exists("LDA_FY_models_current.rda") & !model){
    load("LDA_FY_models_current.rda")
} else{
    models.fy<-lapply(fy, function(y){
        pmid<-meta(abstrCorpus)[,"FY"]==y
        dtm.fy<-dtm[pmid,]
        mclapply(mc.cores=2, seq.k, function(k) LDA(dtm.fy,k))
    })
    names(models.fy)<-fy
    save(models.fy, file=paste0("data/LDA_FY_models", getDate(),".rda"))
    save(models.fy, file=paste0(my_opts$corpus,"/models/LDA_FY_models_current.rda")
    lapply(1:length(models.fy), function(x) lapply(models.fy[[x]], 
                                                   function(y) write.csv2(t(terms(y, 10)),
                                                                          file=paste0(my_opts$corpus,"/models/TopicKeywords/Top10WordsperTopic_for_",y@k,"Topics_model_",names(models.fy)[x],".txt")))) 
}

model.lglk<-as.data.frame(as.matrix(lapply(models, logLik)))
LogLik.df<-data.frame("topics"=seq.k, 
                      "LL"=as.numeric(as.matrix(model.lglk)))

png(paste(resultsPath,"LDA_topicNumber_LL.png", sep="/"), height=1200, width=1200, units="px")
plot(LogLik.df$LL~LogLik.df$topics, pch=19, col="red", main="LDA Simulation with 10 docs per FY")
dev.off()

png(paste0(resultsPath, "/GammaDistbyTopic.png"), height=2400, width=1600, units="px")
par(mar=c(22,4,4,2), mfrow=c(3,1))
lapply(models, function(x) boxplot(x@gamma,names = apply(terms(x,3),2,function(z) paste(z,collapse=",")), range = 0, las=2, main="Distribution of Gammas by Topic", ylab="Gamma", cex.axis=1.5))
dev.off()

lapply(seq(length(models.fy)), function (z) {   
    png(paste0(resultsPath, "/GammaDistbyTopic_FY", names(models.fy)[z],".png"), height=3200, width=1600, units="px")
    par(mar=c(22,4,4,2), mfrow=c(4,1))
    lapply(models.fy[[z]], function(x) boxplot(x@gamma,names = apply(terms(x,3),2,function(z) paste(z,collapse=",")), range = 0, las=2, main="Distribution of Gammas by Topic", ylab="Gamma", cex.axis=1.5))
    dev.off()
})

model.fy.lglk<-do.call(cbind, lapply(models.fy, function(x) do.call(rbind,lapply(x, logLik))))
rownames(model.fy.lglk)<-seq.k
colnames(model.fy.lglk)<-names(models.fy)
model.fy.lglk<-melt(as.matrix(model.fy.lglk))[,1:3]
colnames(model.fy.lglk)<-c("Topic Number","FY","LL")

png(paste0(resultsPath, "/TopicModelperFY_LogLik.png"), height=1000, width=1600, units="px")
qplot(`Topic Number`, LL, data=model.fy.lglk, facets=.~FY)
dev.off()

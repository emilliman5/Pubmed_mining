library(tm)
library(topicmodels)
library(parallel)
library(ggplot2)

#If you want to force a reprocessing of the documents into a Corpus set this value to "TRUE"
reset<-FALSE
model<-FALSE

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

if(!file.exists("Corpus/") || reset){
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

dtm<-DocumentTermMatrix(c(abstrCorpus, spCorpus))
dtm<-t(t(as.matrix(dtm))[as.vector(apply(t(as.matrix(dtm)), 1, sum)>15),])

docRemove<-which(rowSums(dtm)==0)
dtm<-dtm[-docRemove,]

seq.k<-c(25,50,100)

#models<-mclapply(seq.k, mc.cores = 4, function(k) LDA(dtm, k) )
if(file.exists("LDA_models_current.rda") & !model){
    load("LDA_models_current.rda")
} else{
    models<-mclapply(seq.k, mc.cores=2, function(k) LDA(dtm, k) )
    save(models, file = paste0("LDA_models",getDate(),".rda"))
    save(models, file = paste0("LDA_models_current.rda"))
    lapply(models, function(x) write.csv2(t(terms(x, 10)), file=paste0("Top10WordsperTopic_for_",x@k,"Topics_model.txt")))
}

seq.k.fy<-c(25,50,100,250)
fy<-levels(as.factor(meta(abstrCorpus)[,"FY"]))

if(file.exists("LDA_FY_models_current.rda") & !model){
    load("LDA_FY_models_current.rda")
} else{
    models.fy<-lapply(fy, function(y){
        pmid<-meta(abstrCorpus)[,"FY"]==y
        dtm.fy<-dtm[pmid,]
        mclapply(mc.cores=2, seq.k.fy, function(k) LDA(dtm.fy,k))
    })
    names(models.fy)<-fy
    save(models.fy, file=paste0("LDA_FY_models", getDate(),".rda"))
    save(models.fy, file="LDA_FY_models_current.rda")
    lapply(1:length(models.fy), function(x) lapply(models.fy[[x]], 
                                                   function(y) write.csv2(t(terms(y, 10)),
                                                                          file=paste0("Top10WordsperTopic_for_",y@k,"Topics_model_",names(models.fy)[x],".txt")))) 
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
rownames(model.fy.lglk)<-c(25,50,100,250)
colnames(model.fy.lglk)<-names(models.fy)
model.fy.lglk<-melt(as.matrix(model.fy.lglk))[,1:3]
colnames(model.fy.lglk)<-c("Topic Number","FY","LL")

png(paste0(resultsPath, "/TopicModelperFY_LogLik.png"), height=1000, width=1600, units="px")
qplot(`Topic Number`, LL, data=model.fy.lglk, facets=.~FY)
dev.off()

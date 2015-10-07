library(tm)

extraFunFile<-"textMine_funcs.R"
if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
}

dir.create("results/",showWarnings = F)
resultsPath<-paste0("results/",getDate())
dir.create(resultsPath)

##load Corpus.
abstrCorpus<-Corpus(DirSource("Corpus/"), readerControl = list(language="english"))
metaData<-read.csv("CorpusMetaData.txt",colClasses=c('character','character','Date','character','numeric'))
for (x in c("PMID","GrantID","Date", "FY", "FY.Q")) {
    meta(abstrCorpus, x)<-metaData[,x]
}

spCorpus<-Corpus(DirSource("Corpus/SP/"), readerControl = list(language="english"))

##load Topic Models
load("LDA_models_current.rda")
load("LDA_FY_models_current.rda")

ones<-read.table("data/ONES_grants_2012_present.txt")
ones$V2<-substr(ones$V1, 5,12)

pmids<-lapply(ones$V2, function(x) grep(x,meta(abstrCorpus)[,"GrantID"]))
names(pmids)<-ones$V2

barplot(unlist(lapply(pmids, length)), names=names(pmids), las=3)

models[[2]]@gamma[1:5,1:5]
lapply(seq_along(pmids), function(x){
    if(length(pmids[[x]])>0){
        png(paste0(resultsPath,"/",names(pmids)[x], "Ones_grantees_topics.png"), height=600, width=1000, units="px")
        par(mar=c(15,4,2,1))
        barplot(colSums(models[[2]]@gamma[which(models[[2]]@documents %in% meta(abstrCorpus)[pmids[[x]],1]),])
            , names=apply(terms(models[[2]],3),2,function(z) paste(z,collapse=",")), las=3, main=names(pmids)[x],cex.names=1)
        dev.off()
    }
})

x<-do.call(rbind, lapply(seq_along(pmids), function(x){
    colSums(models[[2]]@gamma[which(models[[2]]@documents %in% meta(abstrCorpus)[pmids[[x]],1]),])
}))

png(paste0(resultsPath, "/Ones_grantees_topicsSum.png"), height=800, width=1400, units="px")
par(lty=0,mar=c(15,4,2,1))
barplot(x, las=3, 
        names=apply(terms(models[[2]],3),2,function(z) paste(z,collapse=",")), 
        col=palette(rainbow(18)),ylab="Sum Topic Probility" )
dev.off()
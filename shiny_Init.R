library(tm)
library(topicmodels)
library(proxy)
library(parallel)
library(slam)
library(docopt)

doc<-"This script takes the topic models, corpus and metadata and creates the files/data necessary for deployment to the shiny app.

Usage:  shiny_Init.R --corpus=<corpusDir> --shiny=<shinydir>

Options:
    --corpus=<corpusDir>        Directory where corpus and models are stored
    --shiny=<shinyDir>          Directory where shiny app is located [default:shiny/]
    -h --help                   This helpful message"

my_opts<-docopt(doc)
print(my_opts)    ##This is for testing purposes

ICs<-c("NCI","NICHD","NIMHD","NCCIH",
       "NEI","NIDID","NINDS","NCATS",
       "NHLBI","NIDCR","NINR","NHGRI",
       "NIDDK","NLM","NIAID","NIEHS",
       "CIT","NIAMS","NIGMS","CSR",
       "FDA","NCTR","NIBIB","NIMH",
       "FIC","NIH", "HHS","EPA","HHS",
       "PHS","RFA","FOA")
tlc<-list(AHRG="HS",
          NIH=c("AA","AG","AI","AR","AT","CA","CL","DA","DC",
                "DE","DK","EB","ES","EY","GM","HD","HG","HL",
                "LM","MD","MH","NR","NS","RM","RR","TR","TW",
                "OD","WH"),
       CDC=c("CC","CD","CE","CH","CI","CK","DD","DP","EH","EP","GD","GH",
             "HK","HM","HY","IP","LS","LS","MN","ND","OE","OH","OW","PH",
             "PR","PS","SE","SH","SO","TP","TS","WC"),
       FDA=c("FD","BI","BJ","BK","BL","BM","BN","BO","BP","BQ","BR","BS",
             "BT","BU"),
       SAMHA=c("SU","OA","SM","SP","SU","TI"),
       VA=c("BX","CU","CX","HX","RD","RX"))

activityCodes<-scan("data/NIH_activity_codes.txt",what = "character")
activityCodes<-c(activityCodes,"Z01","ZIA")


corpus<-Corpus(DirSource(paste0(my_opts$corpusDir,"/Corpus/")),
               readerControl = list(language="english"))

names(corpus)<-gsub(".txt","", names(corpus))
load(paste0(my_opts$corpusDir,"/models/LDA_models_current.rda"))

if(file.exists(paste0(my_opts$corpusDir,"/models/ModelsMetaData.txt"))){
    modelMetaData<-read.csv(paste0(my_opts$corpusDir,"/models/ModelsMetaData.txt",
                              colClasses=c('character','character','Date','character',
                                           'character','numeric','integer'))
    write.csv(modelMetaData, file=paste0(my_opts$shinyDir,"/data/ModelsMetaData.txt"), row.names=F)
} else{
    metaData<-read.csv(paste0(my_opts$corpusDir,"/CorpusMetaData.txt",
                              colClasses=c('character','character','Date','numeric','integer','character',
                                           'character'))
    z<-unlist(lapply(names(corpus), function(x) which(metaData$PMID==x)))
    modelMetaData<-metaData[z,]
    write.csv(modelMetaData,paste0(my_opts$corpusDir,"/models/ModelsMetaData.txt", row.names=F)                       
    write.csv(modelMetaData, file=paste0(my_opts$shinyDir,"/data/ModelsMetaData.txt"), row.names=F)
}



dtm<-DocumentTermMatrix(corpus)
term.assoc<-crossprod_simple_triplet_matrix(dtm)/
    (sqrt(col_sums(dtm^2) %*% t(col_sums(dtm^2))))
term.assoc<-as.simple_triplet_matrix(term.assoc)
save(term.assoc,file = paste0(my_opts$corpusDir,"/models/termAssoc.rda"))
save(term.assoc,file = paste0(my_opts$shinyDir,"/models/termAssoc.rda"))
tdm<-TermDocumentMatrix(corpus)
save(tdm, file=paste0(my_opts$corpusDir,"/Corpus_TDM.rda"))
save(tdm, file=paste0(my_opts$shinyDir,"/Corpus_TDM.rda"))

beta.tree<-lapply(models, 
                  function(x) lapply(c("cosine", "Hellinger", "correlation", "Bhjattacharyya"),
                                      function (z) hclust(dist(exp(x@beta), z))))
save(beta.tree, file = paste0(my_opts$corpusDir,"/models/beta.tree.rda"))
save(beta.tree, file = paste0(my_opts$shinyDir,"/models/beta.tree.rda"))

###GrantID-PMID co-occurency network
grantIDs<-strsplit(metaData$GrantID, "\\|")
gi<-as.character(unlist(grantIDs))
gi1<-gsub("\\d","0", gi)
gi1<-gsub("[A-Za-z]","A",gi1)
levels(as.factor(gi1))

gi<-gsub("/|\\\\|:|#|\\.$|\\(|\\)|,", "", gi)
gi<-gsub(paste(ICs,collapse="|"), "",gi)
gi<-gsub("[A-Z]{4,}[\\s|-]","",gi, perl=T)
gi<-gsub("^[^A-Za-z0-9]\\s*","",gi)
gi<-gsub("^-","", gi, perl=T)


gi2<-gsub("\\d", "0",gi)
gi2<-gsub("[A-Za-z]", "A",gi2,perl = T)
levels(as.factor(gi2))
summary(as.factor(gi2))
gi3<-gsub(paste0("^[\\d]",paste(activityCodes,collapse="|^")),"", gi, perl=T)
gi3<-gsub("\\s|-|\\.|\\*|_", "",gi3, perl=T)
gi3<-gsub(".*([A-Z]{2}\\d{6}).*", "\\1", gi3, perl=T)
gi4<-gsub("\\d", "0",gi3)
gi4<-gsub("[A-Za-z]", "A",gi4,perl = T)
levels(as.factor(gi4))

grants.table<-data.frame(PMID=rep(metaData$PMID,sapply(grantIDs, length)), 
                                    grantID=gi3,
                                    year=rep(metaData$FY,sapply(grantIDs, length)))
write.table(grants.table, paste0(my_opts$corpusDir,"/PMIDs_to_grants.txt"),col.names = T,sep="\t",quote=T, row.names=F)
write.table(grants.table, paste0(my_opts$shinyDir,"/PMIDs_to_grants.txt"),col.names = T,sep="\t",quote=T, row.names=F)




##FY model-model distance measures.
edges<-lapply(2:(length(models.fy)-1), function(x){
    d<-dist(exp(models.fy[[x]][[2]]@beta), exp(models.fy[[(x+1)]][[2]]@beta), method="bhjattacharyya")
    e<-dist2Table(d)
    e$col<-paste0("FY",substr(names(models.fy)[x],3,4),"_",e$col)
    e$row<-paste0("FY",substr(names(models.fy)[(x+1)],3,4),"_",e$row)
    colnames(e)<-c("N1","N2","Value")
    e
})

nodes<-do.call(rbind, lapply(seq_along(edges), function(x) {
    n<-data.frame(ID=unique(edges[[x]]$N1), x=x, y=seq_along(unique(edges[[x]]$N1)))
}))

nodes<-rbind(nodes, data.frame(data.frame(ID=unique(edges[[6]]$N2), x=7, y=seq_along(unique(edges[[6]]$N2)))))

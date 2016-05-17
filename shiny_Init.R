suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(topicmodels))
suppressPackageStartupMessages(library(proxy))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(slam))
suppressPackageStartupMessages(library(docopt))

doc<-"This script takes the topic models, corpus and metadata and creates the files/data necessary for deployment to the shiny app.

Usage:  shiny_Init.R --corpus=<corpus> --shiny=<shiny>

Options:
    --corpus=<corpus>        Directory where corpus and models are stored
    --shiny=<shiny>          Directory where shiny app is located [default:current directory]
    -h --help                   This helpful message"

my_opts<-docopt(doc)
print(my_opts)    ##This is for testing purposes

if(is.null(my_opts$shiny)){
    my_opts$shiny<-getwd()
}

my_opts$shiny<-gsub("/$", "", my_opts$shiny)
my_opts$corpus<-gsub("/$","",my_opts$corpus)

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

activityCodes<-"C06,D43,D71,DP1,DP2,DP3,DP4,DP5,DP7,E11,F05,F30,F31,F32,F33,F37,F38,F99,FI2,G07,G08,G11,G12,G13,G20,G94,H13,H25,H50,H57,H62,H64,H75,H79,HD4,I01,IK3,K00,K01,K02,K05,K06,K07,K08,K12,K14,K18,K21,K22,K23,K24,K25,K26,K30,K43,K76,K99,KD1,KL1,KL2,KM1,L30,L32,L40,L50,L60,M01,OT1,OT2,P01,P20,P2C,P30,P40,P41,P42,P50,P51,P60,PL1,PM1,PN1,PN2,R00,R01,R03,R13,R15,R18,R21,R24,R25,R28,R30,R33,R34,R35,R36,R37,R41,R42,R43,R44,R49,R50,R55,R56,R61,R90,RC1,RC2,RC3,RC4,RF1,RL1,RL2,RL5,RL9,RM1,RS1,S06,S07,S10,S11,S21,S22,SB1,SC1,SC2,SC3,SI2,T01,T02,T09,T14,T15,T32,T34,T35,T37,T42,T90,TL1,TL4,TU2,U01,U09,U10,U11,U13,U17,U18,U19,U1A,U1B,U1Q,U1V,U21,U22,U23,U24,U27,U2C,U2G,U2R,U30,U32,U34,U36,U38,U41,U42,U43,U44,U45,U47,U48,U49,U50,U51,U52,U53,U54,U55,U56,U57,U58,U59,U60,U61,U62,U65,U66,U75,U79,U81,U82,U83,U84,U90,UA1,UA5,UB1,UC1,UC2,UC3,UC4,UC6,UC7,UD1,UE1,UE2,UF1,UF2,UG1,UG3,UG4,UH1,UH2,UH3,UH4,UL1,UM1,UM2,UP5,UR6,UR8,US3,US4,UT1,UT2,VF1,VF1,X01,X02,X98,X99"
activityCodes<-c(unlist(strsplit(activityCodes,",")),"Z01","ZIA")

corpus<-Corpus(DirSource(paste0(my_opts$corpus,"/Corpus/")),
               readerControl = list(language="english"))

names(corpus)<-gsub(".txt","", names(corpus))
writeCorpus(corpus, paste0(my_opts$shiny,"/Corpus"))

load(paste0(my_opts$corpus,"/models/LDA_models_current.rda"))
save(models,paste0(my_opts$shiny,"/data/models/LDA_models_current.rda"))

metaData<-read.csv(paste0(my_opts$corpus,"/CorpusMetaData.txt"),
                              colClasses=c('character','character','Date','numeric','integer','character',
                                           'character'))

if(file.exists(paste0(my_opts$corpus,"/models/ModelsMetaData.txt"))){
    modelMetaData<-read.csv(paste0(my_opts$corpus,"/models/ModelsMetaData.txt"),
                              colClasses=c('character','character','Date','character',
                                           'character','numeric','integer'))
    write.csv(modelMetaData, file=paste0(my_opts$shiny,"/data/ModelsMetaData.txt"), row.names=F)
} else{
    z<-unlist(lapply(names(corpus), function(x) which(metaData$PMID==x)))
    modelMetaData<-metaData[z,]
    write.csv(modelMetaData,paste0(my_opts$corpus,"/models/ModelsMetaData.txt"), row.names=F)                       
    write.csv(modelMetaData, file=paste0(my_opts$shiny,"/data/ModelsMetaData.txt"), row.names=F)
}

dtm<-DocumentTermMatrix(corpus)
term.assoc<-crossprod_simple_triplet_matrix(dtm)/
    (sqrt(col_sums(dtm^2) %*% t(col_sums(dtm^2))))
term.assoc<-as.simple_triplet_matrix(term.assoc)
save(term.assoc,file = paste0(my_opts$corpus,"/models/termAssoc.rda"))
save(term.assoc,file = paste0(my_opts$shiny,"/data/models/termAssoc.rda"))
tdm<-TermDocumentMatrix(corpus)
save(tdm, file=paste0(my_opts$corpus,"/Corpus_TDM.rda"))
save(tdm, file=paste0(my_opts$shiny,"/data/Corpus_TDM.rda"))

beta.tree<-lapply(models, 
                  function(x) lapply(c("cosine", "Hellinger", "correlation", "Bhjattacharyya"),
                                      function (z) hclust(dist(exp(x@beta), z))))
save(beta.tree, file = paste0(my_opts$corpus,"/models/beta.tree.rda"))
save(beta.tree, file = paste0(my_opts$shiny,"/data/models/beta.tree.rda"))

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
write.table(grants.table, paste0(my_opts$corpus,"/PMIDs_to_grants.txt"),col.names = T,sep="\t",quote=T, row.names=F)
write.table(grants.table, paste0(my_opts$shiny,"/data/PMIDs_to_grants.txt"),col.names = T,sep="\t",quote=T, row.names=F)


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

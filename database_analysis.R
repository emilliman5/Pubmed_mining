library(VennDiagram)
library(XML)

###PMC xml parse
pub.file<-"data/pmc_result.xml"
pubmed<-xmlParse(pub.file,useInternalNodes = T)
top<-xmlRoot(pubmed)
pmids.2<-getNodeSet(top, "//front/article-meta/article-id[@pub-id-type='pmid']")
pmids<-xmlSApply(pmids.2, xmlValue)

pmid<-metaData<-read.csv("data/CorpusMetaData_2016-02-01.txt",
                         colClasses=c('character','character','Date','character','numeric','character','character', "logical"))
spires<-read.csv("Spires_db.csv", stringsAsFactors=F)
spires$PMID<-as.character(spires$PMID)
spires$Pub.Date<-gsub(" \\d\\d$","",spires$Pub.Date)
spires$Pub.Date<-gsub(" ", "-", spires$Pub.Date)
spires$Date<-as.Date(paste(spires$Pub.Date, " 1"), "%Y %b %d")

length(spires$PMID[spires$Date>=as.Date("2008-10-01")])
pubs<-data.frame(PMID=unique(c(as.character(spires$PMID[spires$Date>=as.Date("2008-10-01")]), pmid$PMID, pmids)), 
                 Spires=0, Pubmed=0, PMC=0)

pubs$Spires[pubs$PMID %in% spires$PMID]<-1
pubs$Pubmed[pubs$PMID %in% pmid$PMID]<-1
pubs$PMC[pubs$PMID %in% pmids]<-1

sum(spires$PMID %in% pmid$PMID)
table(pubs[,2:3])

png("Spires_Pubmed_congruency.png", height=600, width=600, units="px")
draw.pairwise.venn(sum(pubs$Pubmed), sum(pubs$Spires), 20755,
                   category = c("Pubmed","Spires"), fill=c("blue","red"),cex=2, cat.cex=2.5)
dev.off()

venn.diagram(list(Pubmed=pmid$PMID, 
                  Spires=spires$PMID[spires$Date>=as.Date("2008-10-01")],
                  PMC=pmids), na="remove","Spires_Pubmed_PMC_congruency.tiff", fill=c("red","blue","green"))

x<-calculate.overlap(list(Pubmed=pmid$PMID, 
                       Spires=spires$PMID[spires$Date>=as.Date("2008-10-01")],
                       PMC=pmids))

lapply(x, function(x) length(x[!is.na(x)]))

draw.triple.venn(unlist(x))

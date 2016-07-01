library(XML)
library(tm)
library(SnowballC)
library(lubridate)
library(parallel)

NIHreporterParse<-function(file){
  
  table<-readHTMLTable(file, stringsAsFactors=F)
  table<-as.data.frame(table)
  table<-table[,c(2,3,4,5,6,9,10,18,19,20,37,38,39,42,46,47)]
  table<-table[!is.na(table[,2]),]
  table[table[,11]=="",11]<-"NO"
  table[table[,16]=="",16]<-0
  table[,16]<-as.numeric(table[,16])
  table[,1]<-gsub("DESCRIPTION \\(provided by applicant\\):","", table[,1])
  table[,1]<-gsub("Public Health Relevance:","", table[,1],ignore.case = T)
  table[,4]<-gsub("Project Narrative","", table[,4],ignore.case = T)

  return(table)
  
}

PMCParse<-function(pmc.file){
    pub.file<-"data/pmc_result.xml"
    pubmed<-xmlParse(pub.file,useInternalNodes = T)
    top<-xmlRoot(pubmed)
    pmids.2<-getNodeSet(top, "//front/article-meta/article-id[@pub-id-type='pmid']")
    pmids<-xmlSApply(pmids.2, xmlValue)

}

pubmedParse<-function(pub.file){

  pubmed<-xmlParse(pub.file,useInternalNodes = T)
  top<-xmlRoot(pubmed)
  
  nodes<-getNodeSet(top,"//PubmedData/History/PubMedPubDate[@PubStatus='pubmed']")
  pubdate.df<-sapply(nodes, function(x)  paste(xmlSApply(x, xmlValue)[1:3], collapse = "-"))
  pmid<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/PMID", function(node)
      xmlValue(node)))
  
  journal<-xmlSApply(getNodeSet(top,"//Article/Journal/ISSN"), xmlValue)
  grantNodes<-getNodeSet(top, "//GrantList")
  grantID<-xmlSApply(grantNodes, function(x) xmlSApply(x, function(y)
      xmlValue(y[['GrantID']])))
  grantID<-do.call("rbind", lapply(grantID, function(x) paste(x, collapse="|")))
  
  abstr.df<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/Article", function(node)
  {
    title<-xmlValue(node[['ArticleTitle']])
    abstr<-xmlValue(node[['Abstract']][['AbstractText']])
    data.frame("Title"=title, "Abstract"=abstr, stringsAsFactors=F)
  } ))
  
  abstr.df<-cbind(as.data.frame(pmid),as.data.frame(grantID),pubdate.df,journal, abstr.df)
  abstr.df[,"pubdate.df"]<-as.Date(abstr.df[,"pubdate.df"], format = "%Y-%m-%d")
  colnames(abstr.df)[1:2]<-c("PMID","GrantID")
  return(abstr.df)
}

makeCorpus<-function(abstr.df, stopwordsList, cores){
  
  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  } else{ break }
  
  myStopwords<-stopwords('english')
  if(!is.null(stopwordsList)){
      stopwordList<-read.table(stopwordsList)
      myStopwords<-c(myStopwords,stopwordList[,1])
  }
  myStopwords<-tolower(myStopwords)
  
  abstrCorpus<-Corpus(DataframeSource(abstr.df[,c("Title","Abstract")]))
  
  abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower), mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, toSpace, "/|@|\\||-|_|\\\\", mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, removePunctuation, mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, removeNumbers, mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, toSpace, "[^[:alnum:] ]", perl=T, mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, toSpace, "[\\s\\t][A-z]{1,2}[\\s\\t]", perl=T, mc.cores=cores)
  dictCorpus<-abstrCorpus
  abstrCorpus<-tm_map(abstrCorpus, stemDocument, mc.cores=cores)
  abstrCorpus<-tm_map(abstrCorpus, stripWhitespace, mc.cores=cores)  
  
  abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
  abstrCorpus<-Corpus(VectorSource(abstrCorpus))
  abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords, mc.cores=cores)
  
  names(abstrCorpus)<-abstr.df[,"PMID"]
  return(abstrCorpus)
} 

makeSPCorpus<-function(SP_path="data/Strategic_goals",
                       stopwordList="stopwords.txt", pat=c("Goal","Theme","all"), cores=4){
  files<-list.files(pattern = "SP_",path=SP_path,full.names = T)
  if(pat!="all"){
    files<-files[grep(pat, files)]
  }
  
  stopWords<-read.table(stopwordList, colClass=c("character"))
  myStopwords<-c(stopwords('english'), stopWords$V1)
  
  docs<-do.call("rbind", lapply(files, function(x) paste(readLines(file(x)), collapse=" ")))
  docs<-cbind(gsub("data/Strategic_goals//","",gsub(".txt", "",files)), docs)
  
  SP<-VCorpus(VectorSource(docs[,2]))
  SP<-tm_map(SP, toSpace, "/|@|\\||-|_|\\\\", mc.cores=cores)
  SP<-tm_map(SP, removePunctuation, mc.cores=cores)
  SP<-tm_map(SP, removeNumbers, mc.cores=cores)
  
  SP<-tm_map(SP, removeWords, myStopwords, mc.cores=cores)
  dictCorpus<-SP
  SP<-tm_map(SP, stemDocument, mc.cores=cores)
  SP<-tm_map(SP, stripWhitespace, mc.cores=cores)
  SP<-tm_map(SP,content_transformer(tolower), mc.cores=cores)
  
  SP<-mclapply(SP, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
  SP<-Corpus(VectorSource(SP))
  SP<-tm_map(SP, toSpace, "[\\s\\t][A-z]{1,2}[\\s\\t]", perl=T, mc.cores=cores)
 
  meta(SP, "id")<-docs[,1]
  
  dir.create("data/Corpus/SP/")
  writeCorpus(SP,path = "data/Corpus/SP",filenames = meta(SP)[,1])
  SP
  
}
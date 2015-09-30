makeCorpus<-function(pub.file, stopwordList="stopwords.txt", cores=4){

  library(XML)
  library(tm)
  library(SnowballC)
  library(lubridate)
  library(parallel)
  
  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  } else{ break }
  
  stopWords<-read.table(stopwordList, colClasses = c("character"))
  myStopwords<-c(stopwords('english'), stopWords$V1)
  myStopwords<-tolower(myStopwords)
  
  pubmed<-xmlParse(pub.file,useInternalNodes = T)
  top<-xmlRoot(pubmed)
  
  nodes<-getNodeSet(top,"//PubmedData/History/PubMedPubDate[@PubStatus='pubmed']")
  pubdate.df<-sapply(nodes, function(x)  paste(xmlSApply(x, xmlValue)[1:3], collapse = "-"))
  pmid<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/PMID", function(node)
      xmlValue(node)))
  
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
  
  abstr.df<-cbind(as.data.frame(pmid),as.data.frame(grantID),pubdate.df, abstr.df)
  abstr.df[,"pubdate.df"]<-as.Date(abstr.df[,"pubdate.df"], format = "%Y-%m-%d")
  colnames(abstr.df)[1:2]<-c("PMID","GrantID")
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
  meta(abstrCorpus, "PMID")<-abstr.df[,"PMID"]
  meta(abstrCorpus, "GrantID")<-abstr.df[,"GrantID"]
  meta(abstrCorpus, "Date")<-abstr.df[,"pubdate.df"]
  meta(abstrCorpus, "FY.Q")<-quarter(abstr.df[,"pubdate.df"]+90, with_year=T)
  meta(abstrCorpus, "FY")<-floor(meta(abstrCorpus)[,"FY.Q"])
  names(abstrCorpus)<-abstr.df[,"PMID"]
  dir.create("Corpus")
  writeCorpus(abstrCorpus,"Corpus/")
  write.csv(meta(abstrCorpus), "CorpusMetaData.txt",row.names=F)
  
  abstrCorpus
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
  
  dir.create("Corpus/SP/")
  writeCorpus(SP,path = "Corpus/SP",filenames = meta(SP)[,1])
  SP
  
}
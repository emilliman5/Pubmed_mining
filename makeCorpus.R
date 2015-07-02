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
  
  abstr.df<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/Article", function(node)
  {
    grantID<-xmlValue(node[['GrantList']][['Grant']][['GrantID']])
    title<-xmlValue(node[['ArticleTitle']])
    abstr<-xmlValue(node[['Abstract']][['AbstractText']])
    data.frame("GrantID"=grantID, "Title"=title, "Abstract"=abstr, stringsAsFactors=F)
  } ))
  
  abstr.df<-cbind(pubdate.df, abstr.df)
  abstr.df[,1]<-as.Date(abstr.df[,1], format = "%Y-%m-%d")
  abstrCorpus<-Corpus(DataframeSource(abstr.df[,3:4]))
  
  abstrCorpus<-tm_map(abstrCorpus, content_transformer(tolower))
  abstrCorpus<-tm_map(abstrCorpus, toSpace, "/|@|\\||-|_|\\\\")
  abstrCorpus<-tm_map(abstrCorpus, removePunctuation)
  abstrCorpus<-tm_map(abstrCorpus, removeNumbers)
  dictCorpus<-abstrCorpus
  abstrCorpus<-tm_map(abstrCorpus, stemDocument)
  abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)  
  
  abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
  abstrCorpus<-Corpus(VectorSource(abstrCorpus))
  abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
  meta(abstrCorpus, "GrantID")<-abstr.df[,"GrantID"]
  meta(abstrCorpus, "Date")<-abstr.df[,"pubdate.df"]
  meta(abstrCorpus, "FY.Q")<-quarter(abstr.df[,"pubdate.df"]+90, with_year=T)
  meta(abstrCorpus, "FY")<-floor(meta(abstrCorpus)[,"FY.Q"])
  
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
  SP<-tm_map(SP, toSpace, "/|@|\\||-|_|\\\\")
  SP<-tm_map(SP, removePunctuation)
  SP<-tm_map(SP, removeNumbers)
  
  SP<-tm_map(SP, removeWords, myStopwords)
  dictCorpus<-SP
  SP<-tm_map(SP, stemDocument)
  SP<-tm_map(SP, stripWhitespace)
  SP<-tm_map(SP,content_transformer(tolower))
  
  SP<-mclapply(SP, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
  SP<-Corpus(VectorSource(SP))
  meta(SP, "id")<-docs[,1]
  
  dir.create("Corpus/SP/")
  writeCorpus(SP,path = "Corpus/SP",filenames = meta(SP)[,1])
  SP
  
}
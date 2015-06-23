makeCorpus<-function(pub.file, stopword="stopwords.txt", cores=30){

  library(XML)
  library(tm)
  library(SnowballC)
  library(lubridate)
  
  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  } else{ break }
  
  stopWords<-read.table(stopword, colClasses = c("character"))
  myStopwords<-c(stopwords('english'), stopWords$V1)
  
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
  abstrCorpus<-tm_map(abstrCorpus, removeWords, myStopwords)
  dictCorpus<-abstrCorpus
  abstrCorpus<-tm_map(abstrCorpus, stemDocument)
  abstrCorpus<-tm_map(abstrCorpus, stripWhitespace)
  
  abstrCorpus<-mclapply(abstrCorpus, stemCompletion2, dictionary=dictCorpus, mc.cores=cores)
  abstrCorpus<-Corpus(VectorSource(abstrCorpus))
  meta(abstrCorpus, "GrantID")<-abstr.df[,"GrantID"]
  meta(abstrCorpus, "Date")<-abstr.df[,"pubdate.df"]
  meta(abstrCorpus, "FY")<-quarter(abstr.df[,"pubdate.df"]+90, with_year=T)
  
  dir.create("Corpus")
  writeCorpus(abstrCorpus,"Corpus/")
  write.csv(meta(abstrCorpus), "CorpusMetaData.txt",row.names=F)
  abstrCorpus
} 
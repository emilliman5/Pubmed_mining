makeCorpus.2<-function(pub.file, cores=4)
{
  library(NLP)
  library(openNLP)
  library(openNLPmodels.en)
  library(tm)
  library(XML)
  library(SnowballC)
  library(lubridate)
  library(parallel)
  options(java.parameters = "-Xmx24000m")
  
  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  } else{ break }
  
  # stopWords<-read.table(stopwordList, colClasses = c("character"))
  # myStopwords<-c(stopwords('english'), stopWords$V1)
  # myStopwords<-tolower(myStopwords)
  
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
  
  # convert corpus files into strings
  abstrCorpus <- lapply(abstrCorpus, function(x){
    x <- as.String(x)  }  )
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  ###############################################################
  # Start actual PoS-tagging
  # apply annotators to Corpus
  abstrCorpus <- lapply(abstrCorpus, function(x){
    y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
    y2 <- annotate(x, pos_tag_annotator, y1)
    # y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", unlist(lapply(y2w, function(p) substr(x,p$start, p$end))), tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
  names(abstrCorpus)<-abstr.df$PMID
  return(abstrCorpus)
}

getTags<-function()
{
  read.table(text="CC Coordinating conjunction
             CD Cardinal number
             DT Determiner
             EX Existential there
             FW Foreign word
             IN Preposition or subordinating conjunction
             JJ Adjective
             JJR Adjective, comparative
             JJS Adjective, superlative
             LS List item marker
             MD Modal
             NN Noun, singular or mass
             NNS Noun, plural
             NNP Proper noun, singular
             NNPS Proper noun, plural
             PDT Predeterminer
             POS Possessive ending
             PRP Personal pronoun
             PRP$ Possessive pronoun
             RB Adverb
             RBR Adverb, comparative
             RBS Adverb, superlative
             RP Particle
             SYM Symbol
             TO to
             UH Interjection
             VB Verb, base form
             VBD Verb, past tense
             VBG Verb, gerund or present participle
             VBN Verb, past participle
             VBP Verb, non 3rd person singular present
             VBZ Verb, 3rd person singular present
             WDT Wdeterminer
             WP Wh pronoun
             WP$ Possessive whpronoun
             WRB Whadverb", sep="\n")
}
getTaggedWords<-function(corpus, tags)
{
  
  
  
  
}
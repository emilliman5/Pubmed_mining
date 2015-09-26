library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)
options(java.parameters = "-Xmx24000m")

rm(abstrCorpus, pubdate.df,pub.file,pubmed, stopWords,top,myStopwords,grantID,grantNodes,nodes)
Corpus<-abstrCorpus
# convert corpus files into strings
Corpus <- lapply(Corpus, function(x){
  x <- as.String(x)  }  )

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
###############################################################
# Start actual PoS-tagging
# apply annotators to Corpus
Corpus.tagged <- lapply(Corpus, function(x){
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  # y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
  y2w <- subset(y2, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", unlist(lapply(y2w, function(p) substr(x,p$start, p$end))), tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  }  )

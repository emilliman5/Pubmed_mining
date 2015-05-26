library(XML)
library(tm)

pubmed<-xmlTreeParse("../Downloads/pubmed_result.xml",getDTD = F)
r<-xmlRoot(pubmed)

#xmlCorp<-Corpus(XMLSource(pubmed))

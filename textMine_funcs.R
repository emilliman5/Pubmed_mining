stemCompletion_mod <- function(x,dict=dictCorpus) 
{
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),
                                                         dictionary=dict),sep="", collapse=" ")))
}
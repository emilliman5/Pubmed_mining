stemCompletion_mod <- function(x,dict=dictCorpus) 
{
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),
                                                         dictionary=dict),sep="", collapse=" ")))
}

getDate<-function()
{
  ## Purpose is to define a data in the format
  ## 2015may05 (YYYYmmDD)
  tolower(format(Sys.time(), "%Y%b%d"));
}

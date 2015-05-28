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

stemCompletion2 <- function(x, dictionary) 
{
  
  x <- unlist(strsplit(as.character(x), " "))  
  x <- x[x != ""]  
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ") 
  PlainTextDocument(stripWhitespace(x))
  
}
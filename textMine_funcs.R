getDate<-function()
{
  ## Purpose is to define a data in the format
  ## 2015may05 (YYYYmmDD)
  tolower(format(Sys.time(), "%Y%b%d_%H%M"));
}

stemCompletion2 <- function(x, dictionary) 
{
  
  x <- unlist(strsplit(as.character(x), " "))  
  x <- x[x != ""]  
  x <- stemCompletionF(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ") 
  PlainTextDocument(stripWhitespace(x))
  
}

NgramTokenizer <-function(x,n=2)
{
  unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
}

stemCompletionF<-function (x, dictionary, 
          type = c("prevalent", "first", "longest", "none", "random", "shortest")) 
{
 
  if (inherits(dictionary, "Corpus")) 
    dictionary <- unique(unlist(lapply(dictionary, words)))
  type <- match.arg(type)
  y<-unique(x)
  possibleCompletions <- lapply(y, function(w) grep(sprintf("^%s", 
                                                            w), dictionary, value = TRUE))
  switch(type, first = {
    setNames(sapply(possibleCompletions, "[", 1), x)
  }, longest = {
    ordering <- lapply(possibleCompletions, function(x) order(nchar(x), 
                                                              decreasing = TRUE))
    possibleCompletions <- mapply(function(x, id) x[id], 
                                  possibleCompletions, ordering, SIMPLIFY = FALSE)
    setNames(sapply(possibleCompletions, "[", 1), x)
  }, none = {
    setNames(x, x)
  }, prevalent = {
    possibleCompletions <- lapply(possibleCompletions, function(x) sort(table(x), 
                                                                        decreasing = TRUE))
    n <- structure(names(sapply(possibleCompletions, "[", 1)))
    n[match(x,names(n))]
    #setNames(if (length(n)) n else rep(NA, length(x)), x)
  }, random = {
    setNames(sapply(possibleCompletions, function(x) {
      if (length(x)) sample(x, 1) else NA
    }), x)
  }, 
  shortest = {
    ordering <- lapply(possibleCompletions, function(x) order(nchar(x)))
    possibleCompletions <- mapply(function(x, id) x[id], 
                                  possibleCompletions, ordering, SIMPLIFY = FALSE)
    setNames(sapply(possibleCompletions, "[", 1), x)
  })
}
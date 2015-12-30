process_NIH_reporter<-function(file){

  library(tm)
  library(lubridate)
  library(parallel)

  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  }

  table<-read.csv(file, skip = 4, stringsAsFactors = F)
  table<-table[,c(2,3,4,6,9,10,37,42,46)] 
  table[,1]<-gsub("DESCRIPTION \\(provided by applicant\\):","", table[,1])
  table[,1]<-gsub("Public Health Relevance:","", table[,1],ignore.case = T)
  
  }

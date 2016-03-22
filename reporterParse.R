process_NIH_reporter<-function(file){

  library(tm)
  library(lubridate)
  library(parallel)

  extraFunFile<-"textMine_funcs.R"
  if (file.exists(extraFunFile)) {
    source(extraFunFile, keep.source=TRUE);
  }

  table<-read.csv(file, skip = 4, stringsAsFactors = F)
  table<-table[,c(2,3,4,5,6,9,10,18,19,20,37,38,39,42,46,47)]
  table[table[,11]==" ",11]<-"NO"
  table[is.na(table[,16]),16]<-0
  table[,1]<-gsub("DESCRIPTION \\(provided by applicant\\):","", table[,1])
  table[,1]<-gsub("Public Health Relevance:","", table[,1],ignore.case = T)
  table[,4]<-gsub("Project Narrative","", table[,4],ignore.case = T)
  }

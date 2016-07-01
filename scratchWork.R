source("makeCorpus.R")

files<-dir(path="data/NIH_reporter/",pattern="cleaned.csv", full.names = T)
reporter<-lapply(files, NIHreporterParse)
reporter<-do.call(rbind, reporter)

l<-do.call(rbind, lapply(reporter, function(x) unlist(lapply(x, nchar))))
unlist(lapply(reporter[1,], nchar))

exporter<-read.csv("data/NIH_exporter/RePORTER_PRJ_C_FY2016_021.csv", header=T)
exporterAbs<-read.csv("data/NIH_exporter/RePORTER_PRJABS_C_FY2016_021.csv", header=T)

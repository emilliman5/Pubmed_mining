library(XML)
library(RCurl)
library(parallel)
library(httr)
library(rjson)

base<-"http://exporter.nih.gov/ExPORTER_Catalog.aspx"
ex<-getURL(base)
ex<-htmlParse(ex,encoding = "UTF-8")
links<-xpathSApply(ex,"//a/@href")
links<-links[grep("CSVs/final",links)]
files<-do.call(c, lapply(strsplit(links, "/"), tail, n=1))
sapply( 1:length(links), function(n) download.file(url=paste0(base, links[n]),destfile = files[n]))

abstr<-getURL(paste0(base, "?sid=0&index=1"))
abstr<-htmlParse(abstr,encoding = "UTF-8")
links<-xpathSApply(abstr,"//a/@href")
links<-links[grep("CSVs/final",links)]
files<-do.call(c, lapply(strsplit(links, "/"), tail, n=1))
sapply( 1:length(links), function(n) download.file(url=paste0(base, links[n]),destfile = files[n]))

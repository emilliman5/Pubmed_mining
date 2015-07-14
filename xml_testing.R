abstr.df<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/Article", function(node)
{
    grantID<-xmlValue(node[['GrantList']][['Grant']][['GrantID']],recursive = T)
    title<-xmlValue(node[['ArticleTitle']])
    abstr<-xmlValue(node[['Abstract']][['AbstractText']])
    data.frame("GrantID"=grantID, "Title"=title, "Abstract"=abstr, stringsAsFactors=F)
} ))

grantIDs<-xmlApply(top[['PubmedArticle']][['MedlineCitation']], function(x){
    getNodeSet(x, "//GrantList/Grant/GrantID")
})

g<-xmlSApply(gn, xmlValue)

gn<-getNodeSet(top, "//GrantList")

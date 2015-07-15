abstr.df<-do.call("rbind", xpathApply(top, "//PubmedArticle/MedlineCitation/Article", function(node)
{
    title<-xmlValue(node[['ArticleTitle']])
    abstr<-xmlValue(node[['Abstract']][['AbstractText']])
    data.frame("GrantID"=grantID, "Title"=title, "Abstract"=abstr, stringsAsFactors=F)
} ))


grantNodes<-getNodeSet(top, "//GrantList")
grantID<-xmlSApply(grantNodes, function(x) xmlSApply(x, function(y)
    xmlValue(y[['GrantID']])))

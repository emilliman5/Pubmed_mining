tDCor<-simil(topDocGamma[[1]],"correlation", by_rows=F)
tDCosine<-simil(topDocGamma[[1]],"cosine", by_rows=F)
tDBhy<-simil(topDocGamma[[1]],"Bhjattacharyya", by_rows=F)
tDEuclid<-simil(topDocGamma[[1]], "Euclidean", by_rows=F)
tDMan<-simil(topDocGamma[[1]], "manhattan", by_rows=F)

smoothScatter(tDMan, tDEuclid)
smoothScatter(tDCor,tDCosine)
smoothScatter(tDCor,tDBhy)
smoothScatter(tDCosine, tDBhy)
smoothScatter(tDCosine, tDEuclid)
smoothScatter(tDBhy, tDEuclid)
smoothScatter(tDCor, tDEuclid)

hist(tDMan)
hist(tDCor)
hist(tDCosine)
hist(tDBhy)
hist(tDEuclid)
x<-c(1,2)
d<-dendlist(as.dendrogram(hclust(tDCosine)), as.dendrogram(hclust(tDCor)),
            as.dendrogram(hclust(tDBhy)),  as.dendrogram(hclust(tDEuclid)))
lapply(list(c(1,2),c(2,3),c(3,4),c(1,3),c(1,4),c(2,4)), function(x){
 png(paste0("DendroDistComapare",x[1],"and",x[2],".png"), height=1200, width=1200, units="px")
    dendlist(d[[x[1]]],d[[x[2]]]) %>% untangle(method= "DendSer") %>% 
    tanglegram(common_subtrees_color_branches=TRUE)
    dev.off()
 })

pca<-prcomp(t(topDocGamma[[1]]))
plot(pca$x[,1], pca$x[,2], pch=19,col=c(1:50))
plot(pca$x[,2], pca$x[,3], pch=19, col=c(1:50))

x<-topDocGamma[[1]]
l<-levels(cut(x,100))
x[x<=0.00987]<-0
x.dCosine<-simil(x, by_rows=F, method="cosine")
x.dCor<-simil(x, by_rows=F, method="correlation")
smoothScatter(x.dCosine, tDCosine)

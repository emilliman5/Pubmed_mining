library(igraph)

setwd("results/2015aug06_0840/")



############
##Static Network Output
############
nodes<-read.csv("results//2015aug05_0846//Network//Topics50//TopicNodes.csv", stringsAsFactors=F)
edges<-read.csv("results//2015aug05_0846//Network//Topics50//TopicTopicbyDocSimilarity.csv", stringsAsFactors=F)         

nodes<-nodes[,c("ID","TopicWords","Type")]
colnames(edges)<-c("from","to","Weight","Type")          
edges<-edges[edges$Weight<0.9,]

sum(edges$Weight<0.9)
length(edges$Weight)

net<-graph.data.frame(edges, nodes, directed=F)
plot(net, vertex.label=NA, edge.width=((E(net)$Weight*30)-22), edge.curved=0.2,
     layout=layout.fruchterman.reingold)

l<-layout.fruchterman.reingold(net, repulserad=vcount(net)^3)

plot(net, vertex.label=NA, edge.width=8/E(net)$Weight-7, edge.curved=0.2,
     layout=l)
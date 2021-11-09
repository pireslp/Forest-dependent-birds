####set working directory####

install.packages("igraph")
library(igraph)

galheiro=read.table('Galheiro.txt', h=T) #reading Galheiro network
galheiro = ifelse(galheiro==0,0,1)
gal=as.matrix(galheiro)

gloria=read.table('Glória.txt', h=T) #reading Glória network
gloria = ifelse(gloria==0,0,1)
glo=as.matrix(gloria)

saojose=read.table('São José.txt', h=T) #reading São José network
sj = ifelse(saojose==0,0,1)
sj=as.data.frame(sj, method = 1)
sj<- rename(sj, m1 = Morfo_1, m2 = Morfo_2,m3 = Morfo_3,m4 = Morfo_4,
            m5 = Morfo_5, m6 = Morfo_6, m7 = Morfo_7, m8 = Morfo_8, m9 = Morfo_9,
            m10 = Morfo_10, m12 = Morfo_12,m13 = Morfo_13)
sj=as.matrix(sj)

AF=read.table('Água Fria.txt', h=T) #reading Água Fria network
AF = ifelse(AF==0,0,1)
AF = as.data.frame(AF)
AF[13:14]=NULL
AF=as.matrix(AF)

#plot praphs
graph=function(x){
  x <- graph_from_incidence_matrix(x)
  V(x)$color <- ifelse(V(x)$type, "grey", "black")
  V(x)$shape <- ifelse(V(x)$type, "square", "circle")
  plot(x, edge.width= 1.5, edge.color= "black", edge.curved = 3, arrow.mode = 0, 
       edge.arrow.size=0, edge.width = 2, vertex.label=NA)}


A=graph(gal)
gal <- graph_from_incidence_matrix(gal)
V(gal)$color <- ifelse(V(gal)$type, "grey", "black")
V(gal)$shape <- ifelse(V(gal)$type, "square", "circle")


glo <- graph_from_incidence_matrix(glo)
V(glo)$color <- ifelse(V(glo)$type, "grey", "black")
V(glo)$shape <- ifelse(V(glo)$type, "square", "circle")


AF <- graph_from_incidence_matrix(AF)
V(AF)$color <- ifelse(V(AF)$type, "grey", "black")
V(AF)$shape <- ifelse(V(AF)$type, "square", "circle")


sj <- graph_from_incidence_matrix(sj)
V(sj)$color <- ifelse(V(sj)$type, "grey", "black")
V(sj)$shape <- ifelse(V(sj)$type, "square", "circle")


par(mfrow=c(2,2), mar=c(1,0,1,0), oma = c(0, 0, 0.2, 0.2))
plot(AF, edge.width= 1.5, edge.color= "black", arrow.mode = 0, 
     edge.arrow.size=0, edge.width = 2, vertex.label=NA, main = "Água Fria", cex=1.9)
plot(gal, edge.width= 1.5, edge.color= "black", arrow.mode = 0, 
     edge.arrow.size=0, edge.width = 2, vertex.label=NA, main = "Galheiro")
plot(glo, edge.width= 1.5, edge.color= "black", arrow.mode = 0, 
     edge.arrow.size=0, edge.width = 2, vertex.label=NA, main = "Glória")
plot(sj, edge.width= 1.5, edge.color= "black", arrow.mode = 0, 
         edge.arrow.size=0, edge.width = 2, vertex.label=NA, main = "São José")
    dev.off()

    
 
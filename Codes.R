######set working directory#####


library(bipartite) 
library(dplyr) 
library(WriteXLS) 
library(vegan)
library(rnetcarto)
library(gmodels)
library(tidyverse)
library(ggplot2)


#### importing the networks #####
galheiro=read.table('Galheiro.txt', h=T) #reading Galheiro network
galheiro = ifelse(galheiro==0,0,1)
gal=as.matrix(galheiro)
nulls.galheiro <- nullmodel(galheiro, method = 1) #creating 1000 null models for Galheiro network using Patefeld algorithm


gloria=read.table('Glória.txt', h=T) #reading Glória network
gloria = ifelse(gloria==0,0,1)
glo=as.matrix(gloria)
nulls.gloria <- nullmodel(gloria, method = 1) #creating 1000 null models for Glória network using Patefeld algorithm

saojose=read.table('São José.txt', h=T) #reading São José network
sj = ifelse(saojose==0,0,1)
sj=as.data.frame(sj, method = 1)
sj<- rename(sj, m1 = Morfo_1, m2 = Morfo_2,m3 = Morfo_3,m4 = Morfo_4,
                  m5 = Morfo_5, m6 = Morfo_6, m7 = Morfo_7, m8 = Morfo_8, m9 = Morfo_9,
                  m10 = Morfo_10, m12 = Morfo_12,m13 = Morfo_13)
sj=as.matrix(sj)
nulls.sj <- nullmodel(sj, method = 1) #creating 1000 null models for Galheiro network using Patefeld algorithm

AF=read.table('Água Fria.txt', h=T) #reading Água Fria network
AF = ifelse(AF==0,0,1)
AF=as.matrix(AF)
nulls.AF <- nullmodel(AF)

#### z-scores to normalize network metrics #####
zscore=function(x){mean(x)
  sd <- sd(x)
  (x - mean(x))/sd}

#### Centrality metrics####
#normalized degree
ND.glo=as.matrix(specieslevel(glo, index = "normalised degree", level="lower"))
ND.glo=zscore(ND.glo)

ND.af=as.matrix(specieslevel(AF, index = "normalised degree", level="lower"))
ND.af=zscore(ND.af)

ND.sj=as.matrix(specieslevel(sj, index = "normalised degree", level="lower"))
ND.sj=zscore(ND.sj)

ND.gal=as.matrix(specieslevel(gal, index = "normalised degree", level="lower"))
ND.gal=zscore(ND.gal)

ND=rbind(ND.glo,ND.af,ND.sj,ND.gal)

#CLOSENESS
CC.glo=specieslevel(glo, index = "closeness", level="lower" )
CC.glo[,2]=NULL
CC.glo=as.matrix(CC.glo)
CC.glo=zscore(CC.glo)

CC.af=specieslevel(AF, index = "closeness", level="lower" )
CC.af[,2]=NULL
CC.af=as.matrix(CC.af)
CC.af=zscore(CC.af)

CC.sj=specieslevel(sj, index = "closeness", level="lower" )
CC.sj[,2]=NULL
CC.sj=as.matrix(CC.sj)
CC.sj=zscore(CC.sj)

CC.gal=specieslevel(gal, index = "closeness", level="lower" )
CC.gal[,2]=NULL
CC.gal=as.matrix(CC.gal)
CC.gal=zscore(CC.gal)

CC=rbind(CC.glo,CC.af,CC.sj,CC.gal)

#Betweenness
BD.glo=specieslevel(glo, index = "betweenness", level="lower" )
BD.glo[,2]=NULL
BD.glo=as.matrix(BD.glo)
BD.glo=zscore(BD.glo)

BD.af=specieslevel(AF, index = "betweenness", level="lower" )
BD.af[,2]=NULL
BD.af=as.matrix(BD.af)
BD.af=zscore(BD.af)

BD.sj=specieslevel(sj, index = "betweenness", level="lower" )
BD.sj[,2]=NULL
BD.sj=as.matrix(BD.sj)
BD.sj=zscore(BD.sj)

BD.gal=specieslevel(gal, index = "betweenness", level="lower" )
BD.gal[,2]=NULL
BD.gal=as.matrix(BD.gal)
BD.gal=zscore(BD.gal)

BD=rbind(BD.glo,BD.af,BD.sj,BD.gal)

#binding all centrality metrics
nm=cbind(ND, CC,BD)
nm[is.na(nm)]<-0
nm=as.data.frame(nm)

####Correlations among centrality metrics####
cor(nm)
cor.test(nm$normalised.degree,nm$closeness)
cor.test(nm$normalised.degree,nm$betweenness)
cor.test(nm$closeness,nm$betweenness)

####PCA####

pca=prcomp(nm)
summary(pca)

escores=pca$x 
escores=escores[,1]
escores=as.data.frame(escores)

####importing the complete table with centrality metrics, areas, and species traits ####
glmm=read.table("Table.txt", h=T)


####Generating model set####

library(MuMIn)
library(lme4)

models= lmer(escores~Gape+factor(Habitat)+Frug.lvl+(1|Area),data=glmm, 
                            na.action="na.fail")
summary(models)
models.set<-dredge(models)
best.models=subset(models.set, delta < 2)%>%as.data.frame #selecting the top-ranked

#confidence intervals of predictor variable
ci= lmer(escores~factor(Habitat)+(1|Area),data=glmm, 
             na.action="na.fail")
confint(ci)


####Figures####

#figures
install.packages("igraph")
library(igraph)

bar.plot=read.table('Barplot2.txt', h=T)

figure2=ggplot(bar.plot, aes(fill=Habitat, y=normalized, x=reorder(rownames(bar.plot),-normalized))) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_brewer(palette = "Paired", name="Habitat dependency",labels=c("Dependent", "Independent", "Semi dependent"))+
  theme_classic()+
  labs(x="", y = "Relative importance to network structure")+
  theme(axis.line.x =  element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour="black"),
        axis.title.y = element_text(color="black", size=10, face="bold", vjust=3))+
  geom_hline(yintercept=0)
plot(figure2)


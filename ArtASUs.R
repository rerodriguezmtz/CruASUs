rm(list=ls(all=TRUE))
setwd("~/GitHub/CruASUs")
Abu<- read.csv("~/GitHub/CruASUs/CruColl data.csv")# all data
Div <- read.csv("~/GitHub/CruASUs/CruColl iNEXT.csv") #data by zone
AbuNMDS<- read.csv("~/GitHub/CruASUs/ArtColl nMDS.csv")# by order
IVI<- read.csv("~/GitHub/CruASUs/CruColl IVI.csv")# for Importance Value Index

#Load libraries
library (ggplot2)
library(plyr) 
library(dplyr) 
library(iNEXT)
library(vegan)

# N per order per method
Abu$Order=factor(Abu$Order, levels = c("Amphipoda", "Cumacea", "Isopoda", "Tanaidacea","Decapoda"))
AbuN<-ddply(Abu,. (Substrate, Order), summarise, N= sum(N))
AbuN

# DIVERSITY PLOTS
out1 <- iNEXT(Div[,c("ASUs", "CG", "Total")],q=c(0, 1, 2), 
              datatype="abundance", 
              knots=40, se=TRUE, nboot=300,
              endpoint=NULL, conf=0.95) 
out1 # print values

out1$DataInfo

ggiNEXT(out1, type=1, facet.var="order")+facet_wrap(~order, scales="free")+ 
  scale_colour_manual(values=c("red2", "deepskyblue4", "black")) + 
  scale_fill_manual(values=c("red2", "deepskyblue4", "black")) + ylab("Species diversity")+ 
  theme_bw(base_size=22)

## Sample completeness
ggiNEXT(out1, type=2)+ 
  scale_colour_manual(values=c("red2", "deepskyblue4", "black")) + 
  scale_fill_manual(values=c("red2", "deepskyblue4", "black")) +  
  theme_bw(base_size=22)


# NMDS PLOT
Abu1<-AbuNMDS[,4:181]
Abu2<-AbuNMDS[,1:3]

## Turn N to relative abundance by dividing each value by sample
Abu1 <- decostand(Abu1, method = "total")

Abu3 = as.matrix(Abu1)

## check total abundance in each sample
apply(Abu1, 1, sum)

set.seed(123)
nmds = metaMDS(Abu3, distance = "bray")
nmds

##extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

##add columns to data frame 
data.scores$Method = AbuNMDS$Method
data.scores$Survey = AbuNMDS$Survey
data.scores$Replica = AbuNMDS$Replica

##check assumption of homogeneity of multivariate dispersion
distances<-vegdist(Abu3)

## Anosim test
an = anosim(Abu3, Abu2$Method, distance = "bray", permutations = 9999)
an

## Plot
ggplot(data.scores, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 8, aes( shape = Survey, colour = Method))+ 
  theme(axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 18, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 18), 
        axis.title.x = element_text(face = "bold", size = 18, colour = "black"), 
        legend.title = element_text(size = 18, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", y = "NMDS2", colour = "Method",  shape = "Survey")+ 
  scale_colour_manual(values = c("red2", "deepskyblue4"))+
  annotate("text", x = -0.4, y = 1.2, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0, size = 6)+
  annotate("text", x = 0.42, y = 1.2, label =(expression(paste("R = 0.9790"))), size = 6)+
  annotate("text", x = 1.0, y = 1.2, label =(expression(paste(,italic("P"),"= 0.0073"))), size = 6)

##bootstraping and testing for differences between groups
fit<- adonis(Abu3~Method, data=Abu2, permutaitns=999, method ="bray")
fit

## Assess goodness of ordination fit (stress plot)
stressplot(nmds)

#IMPORTANCE VALUE INDEX
IVIASUs<- IVI[IVI$Substrate=="ASUs",]
IVICR<- IVI[IVI$Substrate=="CG",]

ggplot() + 
  geom_bar(data=IVIColl, aes(x = Species, y=-IVI, fill=Substrate), stat="identity")+
  geom_bar(data=IVICR, aes(x = Species, y=IVI, fill=Substrate), stat="identity")+
  geom_hline(yintercept = 0, color =c("white"))+
  scale_y_continuous(labels = abs, limits = max(IVI$IVI) * c(-1,1))+ 
  coord_flip()+
  ggtitle("             ASUs                             Coral gravel")+
  labs(y="Importance Value Index (%)", x = "Species")+
  theme(axis.text.y = element_text(hjust=0)) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("red2", "deepskyblue4"))+
  theme_grey(base_size=22)+
  theme(legend.position = "NONE")

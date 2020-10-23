rm(list=ls(all=TRUE))
setwd("~/GitHub/CruASUs")
Abu<- read.csv("~/GitHub/CruASUs/CruColl data.csv")# all data
Div <- read.csv("~/GitHub/CruASUs/CruColl iNEXT.csv") #data by zone
PANMDS<- read.csv("~/GitHub/CruASUs/ArtColl nMDS PA.csv")# for nMDS using presence/absence
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
out1 <- iNEXT(Div[,c("ASUs", "CR", "Total")],q=c(0, 1, 2), 
              datatype="abundance", 
              knots=40, se=TRUE, nboot=300,
              endpoint=NULL, conf=0.95) 
out1 # print values

out1$DataInfo

ggiNEXT(out1, type=1, facet.var="order")+facet_wrap(~order, scales="free")+ 
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "black")) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "black")) + ylab("Species diversity")+ 
  theme_bw(base_size=15)

## Sample completeness
ggiNEXT(out1, type=2)+ 
  scale_colour_manual(values=c("#E69F00", "#56B4E9", "black")) + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "black")) +  
  theme_bw(base_size=15)

# NMDS PLOT
PA1<-PANMDS[,4:181]
PA2<-PANMDS[,1:3]

## Obtain matrix
PA3 = as.matrix(PA1)

## nMDS
set.seed(123)
nmds = metaMDS(PA3, distance = "bray")
nmds

##extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

##add columns to data frame 
data.scores$Method = PANMDS$Method
data.scores$Survey = PANMDS$Survey
data.scores$Replica = PANMDS$Replica

##check assumption of homogeneity of multivariate dispersion
distances<-vegdist(PA3)

## Plot
ggplot(data.scores, aes(x = NMDS1, y = NMDS2))+ 
  geom_point(size = 10, aes( shape = Survey, colour = Method))+ 
  theme(axis.text.y = element_text(colour = "black", size = 30), 
        axis.text.x = element_text(colour = "black", size = 30), 
        legend.text = element_text(size = 30,  colour ="black"), 
        legend.position = "right", axis.title.y = element_text( size = 30), 
        axis.title.x = element_text( colour = "black", size =30), 
        legend.title = element_text(size = 30, colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", y = "NMDS2", colour = "Method",  shape = "Survey")+ 
  scale_colour_manual(values = c("#E69F00", "#56B4E9"))+
  annotate("text", x = -0.6, y = 1.28, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0, size = 10)+
  annotate("text", x = 0.25, y = 1.3, label =(expression(paste(""*" "~R^2*"","= 0.1142"))), size = 10)+
  annotate("text", x = 0.8, y = 1.28, label =(expression(paste(italic("P")," = 0.0027"))), size = 10)

##bootstraping and testing for differences between groups
fit<- adonis(PA3~Method, data=PA2, permutations=9999, method ="bray")
fit

#IMPORTANCE VALUE INDEX
IVIASUs<- IVI[IVI$Substrate=="ASUs",]
IVICR<- IVI[IVI$Substrate=="CR",]

ggplot() + 
  geom_bar(data=IVIASUs, aes(x = Species, y=-IVI, fill=Substrate), stat="identity")+
  geom_bar(data=IVICR, aes(x = Species, y=IVI, fill=Substrate), stat="identity")+
  geom_hline(yintercept = 0, color =c("white"))+
  scale_y_continuous(labels = abs, limits = max(IVI$IVI) * c(-1,1))+ 
  coord_flip()+
  ggtitle("       ASUs            Coral rubble")+
  labs(y="Importance Value Index (%)", x = "Species")+
  theme(axis.text.y = element_text(hjust=0)) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme_minimal(base_size=20)+
  theme(legend.position = "NONE")


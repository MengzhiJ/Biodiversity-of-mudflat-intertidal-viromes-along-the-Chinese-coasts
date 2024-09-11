#R_code (v4.2.0) for Figure 4
#load all need packages
library(ggplot2)
library(dplyr)
library(readxl)
library(ggpmisc)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(vegan)
library(SoDA)
library(circlize)
library(viridis)
library(reshape2)
library(VennDiagram)
setwd('D:/Tidal_spatial/R_data/github/Fig.4/raw_data_for_figure4')

#Fig.4a
data<-read_excel("Fig.4.xlsx")
data$taxonomy=factor(data$taxonomy, levels=c("Chloroflexi","Bacteroidetes","Deltaproteobacteria","Gammaproteobacteria","Alphaproteobacteria","Unknown"))
data$value=factor(data$value, levels=c('Microbe',"Virus"))

ggplot(data=data, aes(x=value, y=variable))+
  geom_bar(stat="identity", color="#696969",width=0.35,size=0.15,position="stack", aes(fill=taxonomy))+
  scale_fill_manual(breaks = c("Alphaproteobacteria","Gammaproteobacteria","Deltaproteobacteria","Chloroflexi","Bacteroidetes","Unknown"),
                    values = c('#2570AE', '#539DDA',"#97CBFF", '#3CB371','#CD853F','lightgray'))+
  labs(x="",y="# of core OTUs")+
  theme(legend.background=element_rect(color="white"))+
  theme(legend.key = element_blank())+
  theme(axis.line = element_line(color=))+
  theme_test()+ theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
                       axis.title.x = element_text(size=6.5),
                       axis.title.y = element_text(size=6.5),
                       axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
                       axis.text.y=element_text(size=6,colour = 'black'),
                       axis.ticks = element_line(size = 0.25),
                       axis.ticks.length = unit(0.04, "cm"),
                       panel.border = element_rect(size=0.4))+
                       theme(legend.position ="none")

#Fig.4b
data<-read.delim('latitude.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ggplot(data = data, mapping = aes(x=Latitude,y=Richness))+
  geom_point(aes(fill=Type),size = 1.2, alpha = 0.8,shape=21,stroke=0.15)+
  labs(x="Absolute latitude",y="Richness")+
  geom_smooth(aes(color=Type),method = 'lm',se=F,size=0.4,fullrange=F,formula=y~poly(x,2))+
  scale_color_manual(breaks=c("vPCs","vOTUs","mOTUs"),values=c("#7AC5CD","#FF9966","gray"))+
  scale_fill_manual(breaks=c("vPCs","vOTUs","mOTUs"),
                    values=c("#7AC5CD","#FF9966","gray"))+
  theme_test()+ theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
                      axis.title.x = element_text(size=6.5),
                      axis.title.y = element_text(size=6.5),
                      axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
                      axis.text.y=element_text(size=6,colour = 'black'),
                      axis.ticks = element_line(size = 0.25),
                      axis.ticks.length = unit(0.04, "cm"),
                      panel.border = element_rect(size=0.4))+
  theme(legend.position ="none")+
  stat_poly_eq(formula=y~poly(x,2),aes(color=Type,label = paste(..rr.label.., ..p.value.., sep = "~`,`~")),size=1.7,
               label.x = )

#Fig.4c
#distance_calculate
votu<-read.delim('vOTUs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
distv<-vegdist(t(votu),method = 'bray')
distv_num<-1-as.numeric(distv)
distv_num<-log10(distv_num)
distv_num<-as.numeric(distv_num)

motu<-read.delim('mOTUs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
distm<-vegdist(t(motu),method = 'bray')
distm_num<-1-as.numeric(distm)
distm_num<-log10(distm_num)
distm_num<-as.numeric(distm_num)

vPC<-read.delim('vPCs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
distP<-vegdist(t(vPC),method = 'bray')
distP_num<-1-as.numeric(distP)
distP_num<-log10(distP_num)
distP_num<-as.numeric(distP_num)

loca<-read_excel('DDR.xlsx')
loca_geo<-geoXY(loca$Latitude,loca$Longitude)
rownames(loca_geo)<-rownames(loca)
dist_loca<-vegdist(loca_geo,method = 'euclidean')
dist_loca_log<-log10(dist_loca)
dist_loca_num<-as.numeric(dist_loca_log)
votu_dist<-data.frame(distv_num,dist_loca_num)
motu_dist<-data.frame(distm_num,dist_loca_num)
vPC_dist<-data.frame(distP_num,dist_loca_num)
write.csv(votu_dist,file="votu_dist.csv",row.names = F)
write.csv(motu_dist,file="motu_dist.csv",row.names = F)
write.csv(vPC_dist,file="vPC_dist.csv",row.names = F)

data_new<-read.csv('bray.csv')
data_new$Type=factor(data_new$Type, levels=c("vPCs","vOTUs","mOTUs"))
data_new<-data_new[order(data_new$Type, decreasing = TRUE), ]
ggplot(data = data_new,aes(x=dist_loca_num,y=distv_num))+
  geom_point(aes(color=Type),size = 0.35, alpha = 0.65,shape=16)+
  theme(axis.line = element_line(color="black"))+
  labs(x="",y="")+
  scale_color_manual(breaks=c("vPCs","vOTUs","mOTUs"),
                     values=c("#7AC5CD","#FF9966","lightgray"))+
  scale_fill_manual(breaks=c("vPCs","vOTUs","mOTUs"),values=c("#7AC5CD","#FF9966","lightgray"))+
  geom_smooth(aes(color=Type),method = 'lm',se=F,size=0.5,fullrange=F)+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=6.5),
        axis.title.y = element_text(size=6.5),
        axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
        axis.text.y=element_text(size=6,colour = 'black'),
        axis.ticks = element_line(size = 0.25),
        axis.ticks.length = unit(0.04, "cm"),
        panel.border = element_rect(size=0.4))+
  scale_x_continuous(breaks = seq(0,6,1))+xlim(5.25,6.5)+
  stat_poly_eq(aes(color=Type,label = paste(..eq.label..,..rr.label..,..p.value.., sep = "~`,`~")),size=5,
               formula = y ~ x,parse = TRUE)

#Fig.4d
#info dataset
df <- read.csv("Fig.4d.csv", header=TRUE,stringsAsFactors = FALSE,check.names = FALSE)
df_melt<-melt(df,id.vars = 'Region')
colnames(df_melt)<-c('from','to','value')
df_melt$to<-as.character(df_melt$to)

#define factor
df_sum<-apply(df[,2:ncol(df)],2,sum)+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)
df_melt$from<-factor(df_melt$from,levels=df$Region[order$ix],order=TRUE)

df_melt<-dplyr:: arrange (df_melt, from)

#define color
mycolor = c(Intertidal_zone= "#02C874", Marine = "#C4A8FF", Soil = "#F7BF90",
            Human = "#FBEA50", Freshwater = "#4D94F0", RefSeq = "gray")
names(mycolor) <-df$Region

#circos plot
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

chordDiagram(
  x = df_melt,order = c("Intertidal_zone","Marine","Soil","Human","Freshwater","RefSeq"),
  grid.col = mycolor,
  transparency = 0.6,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

#circos add labels
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(
      x = mean(xlim),
      y = 3.5,
      labels = sector.index,
      facing = "bending",
      cex = 1.2
    )
    circos.axis(
      h = "top",
      major.at = c(0,5,10,15,20,25,30,35,40),
      minor.ticks = 1,
      major.tick.length = 0.5,
      labels.niceFacing = FALSE)
  }
)

#Fig.4e 
venn<-venn.diagram(list(A = c(1:2566), B = c(1:3,4:20,21,22:28,29:56,57:210,2567:5142,5143:5144,5145:5182,5183:5214,5215:5224,5225:5583),C=c(1:3,4:20,22:28,211:215,216:248,2567:2570,5143:5144,5145:5182,5183:5214,5584:7617,7618:7620,7621:7627,7628:7753),D=c(1:3,21,249,250:253,2567:2570,5143:5144,5215:5224,7618:7620,7621:7627,7754:7795,7796:9934),E=c(1:3,4:20,21,29:56,211:215,249,254:307,2567:2570,5145:5182,5225:5583,7618:7620,7628:7753,7754:7795,9935:12315)),
             fill = c("#02C874","#C4A8FF","#F7BF90","#FBEA50","#4D94F0"),
             alpha = c(0.7, 0.7,0.7,0.7,0.7), cex = 1.4,
             cat.cex=0,cat.fontface = 4,lty =2, 
             filename = NULL)
pdf(file = "Fig.4e.pdf")
grid.draw(venn)
dev.off()

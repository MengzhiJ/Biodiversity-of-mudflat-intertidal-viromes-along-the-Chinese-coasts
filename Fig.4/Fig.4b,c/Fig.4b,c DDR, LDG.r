library(readxl)
library(ggpmisc)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(vegan)
library(SoDA)
library(ggplot2)
library(readxl)


data<-read.delim('I:/Tidal/R_data/Fig.4/latitude.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)


#LDG
ggplot(data = data, mapping = aes(x=Latitude,y=Richness))+
  geom_point(aes(fill=Type),size = 2.5, alpha = 0.85,shape=21)+
  labs(x="Absolute latitude",y="Richness")+
  theme(axis.line = element_line(color="black"))+
    geom_smooth(aes(color=Type),method = 'gam',se=F,size=1,fullrange=F,formula=y~s(x,k=3))+
  theme_test()+ theme(legend.position ="none")+scale_color_manual(breaks=c("vPCs","vOTUs","mOTUs"),
                                                                  values=c("#7AC5CD","#FF8040","gray"))+
  scale_fill_manual(breaks=c("vPCs","vOTUs","mOTUs"),
                     values=c("#7AC5CD","#FF8040","gray"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.2),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+stat_cor(aes(color=Type,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
                                                       label.x = )
ggsave("Fig.4b.tiff",width=5.5,height=5,path="I:/Tidal/R_data/Fig.4")


#DDR
library(vegan)
library(SoDA)
library(ggplot2)
library(readxl)
votu<-read.delim('I:/Tidal/R_data/Fig.4/vPCs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
distv<-vegdist(t(votu),method = 'bray')
distv_num<-1-as.numeric(distv)
distv_num<-log10(distv_num)
distv_num<-as.numeric(distv_num)

loca<-read_excel('I:/Tidal/R_data/Fig.4/DDR.xlsx')
xy_loca<-geoXY(loca$Latitude,loca$Longitude)
rownames(xy_loca)<-rownames(loca)
dist_loca<-vegdist(xy_loca,method = 'euclidean')
dist_loca_log<-log10(dist_loca)
dist_loca_num<-as.numeric(dist_loca_log)

data_new<-data.frame(distv_num,dist_loca_num)

data_new<-read.csv('I:/Tidal/R_data/Fig.4/Bray.csv')


ggplot(data = data_new,aes(x=dist_loca_num,y=distv_num))+
  geom_point(aes(color=Type),size = 1, alpha = 0.85,shape=16)+
  theme(axis.line = element_line(color="black"))+labs(x="",y="")+
  theme_test()+ theme(legend.position ="none")+scale_color_manual(breaks=c("Virus","Host"),
                                                                  values=c("#FF8040","gray"))+
  scale_fill_manual(breaks=c("Virus","Host"),
                    values=c("#FF8040","gray"))+geom_smooth(aes(color=Type),method = 'lm',se=F,size=1,fullrange=F)+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.2),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+scale_x_continuous(breaks = seq(0,6,1))+xlim(5.2,6.5)+
  stat_poly_eq(aes(color=Type,label = paste(..eq.label..,..rr.label.., sep = "~`,`~")),
               formula = y ~ x,parse = TRUE)
  ggsave("Fig.4c.tiff",width=5.7,height=5,path="I:/Tidal/R_data/Fig.4")

#hillR-DDR (abandon)
library(hillR)
votu<-read.delim('I:/Tidal/R_data/Fig.4/host.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
distv<-hill_taxa_parti_pairwise(t(votu),q=2)
distv_num<-as.matrix(distv[,7])
distv_num<-log10(distv_num)
distv_num<-as.numeric(distv_num)

loca<-read_excel('I:/Tidal/R_data/Fig.4/DDR.xlsx')
xy_loca<-geoXY(loca$Longitude,loca$Latitude)
rownames(xy_loca)<-rownames(loca)
dist_loca<-vegdist(xy_loca,method = 'euclidean')
dist_loca_log<-log10(dist_loca)
dist_loca_num<-as.numeric(dist_loca_log)

data_new<-data.frame(distv_num,dist_loca_num)

ggplot(data=data_new,aes(x=dist_loca_num,y=distv_num))+geom_point(size=1.1,aes(dist_loca_num,distv_num),alpha=0.7)+
  labs(x="",y="")+
  geom_smooth(method="lm",fullrange=T,se=T,size=1.2)+
stat_poly_eq(aes(label = paste(..eq.label..,..rr.label.., sep = "~`,`~")),
             formula = y ~ x,parse = TRUE)

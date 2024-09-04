#R_code (v4.2.0) for Figure 1
#load all need packages
library(geojsonsf)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggspatial)
library(cowplot)
library(tidyverse)
library(scales)
library(ggsci)
library(ggforce)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
library(vegan)
library(ggpmisc)
#Fig. 1a
#read map data
setwd("D:/Tidal_spatial/R_data/github/Fig.1/raw_data_for_figure1/")
API_pre = "http://xzqh.mca.gov.cn/data/"
China = st_read(dsn = paste0(API_pre, "quanguo.json"), stringsAsFactors=FALSE) 
st_crs(China) = 4326
China_line = st_read(dsn = paste0(API_pre, "quanguo_Line.geojson"), stringsAsFactors=FALSE) 
st_crs(China_line) = 4326
gjx <- China_line[China_line$QUHUADAIMA == "guojiexian",]
province <- read.delim("province.txt", row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
#read map colour
colour <- read.csv("colour.csv")
#read minimap data
nine_lines = read_sf('nanhai.geojson') 
#define colour
colour$new_colour <- rep(0,nrow(colour))
colour$new_colour[which(colour$shengfen=="Jiangsu")] <- 1
colour$new_colour[which(colour$shengfen=="Guangdong")] <- 1
colour$new_colour[which(colour$shengfen=="Guangxi")] <- 1
colour$new_colour[which(colour$shengfen=="Liaoning")] <- 1
colour$new_colour[which(colour$shengfen=="Zhejiang")] <- 1
colour$new_colour[which(colour$shengfen=="Fujian")] <- 1
colour$new_colour[which(colour$shengfen=="Shandong")] <- 1
colour$new_colour[which(colour$shengfen=="Hainan")] <- 1
colour$QUHUADAIMA <- as.character(colour$QUHUADAIMA)
China <- dplyr::left_join(China,colour,by= "QUHUADAIMA")
#China map
map<-ggplot()+
  geom_sf(data =China,aes(fill = factor(new_colour)),size=1,color="black") +
  geom_sf(data = gjx)+theme(legend.position = "none")+
  theme(legend.key = element_blank())+
  theme(axis.line = element_line(color="white"))+
  theme_test()+ 
  theme(aspect.ratio = 0.95,
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA,color="white",linetype=1,size=0.3))+
  scale_fill_manual(values = c("white","lightgray"))+
  theme(legend.position = "none")
#minimap
nine_map <- ggplot() +
  geom_sf(data = China,fill='NA', size=0.1) + 
  geom_sf(data = nine_lines,color='#3C3C3C',size=0.1)+
  coord_sf(ylim = c(-4028017,-1877844),xlim = c(117131.4,2115095),crs="+proj=laea +lat_0=40 +lon_0=104")+
  theme(aspect.ratio = 1.25, 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=0.3),
        plot.margin=unit(c(0,0,0,0),"mm"))
#combine two map
ggdraw()+draw_plot(map)+draw_plot(nine_map, x = 0.8, y = 0.17, width = 0.1, height = 0.15)

ggsave("Fig.1a-1.pdf",width=4,height=4,path="D:/")

#sampled map (province)
HN = read_sf('hainan.json') 
GD = read_sf('guangdong.json') 
SD = read_sf('shandong.json')
ZJ = read_sf('zhejiang.json') 
JS = read_sf('jiangsu.json') 
LN = read_sf('liaoning.json') 
GX = read_sf('guangxi.json') 
FJ = read_sf('fujian.json') 
mydata <- read.delim('Fig.1a.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ggplot() +geom_sf(data = HN,color='#363636',fill="#FFFFFF",size=1)+
  geom_sf(data = JS,color='#363636',fill="#FFFFFF",size=1)+geom_sf(data = ZJ,color='#363636',fill="#FFFFFF",size=1)+
  geom_sf(data = LN,color='#363636',fill="#FFFFFF",size=1)+geom_sf(data = SD,color='#363636',fill="#FFFFFF",size=1)+
  geom_sf(data = GX,color='#363636',fill="#FFFFFF",size=1)+geom_sf(data = GD,color='#363636',fill="#FFFFFF",size=1)+
  geom_sf(data = FJ,color='#363636',fill="#FFFFFF",size=1)+
  xlim(95,127)+ylim(18,45)+ theme_bw()+labs(x="",y="")+
  annotation_scale(location = "bl",text_cex = 0.5,height = unit(0.12, "cm"),line_width = 0.5)+
  theme(aspect.ratio = 1.33,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=0.4),
        plot.margin=unit(c(0,0,0,0),"mm"),
        axis.ticks = element_line(size = 0.3),
        axis.ticks.length = unit(0.05, "cm"),
        axis.text.x = element_text(size=7,colour = 'black'),
        axis.text.y=element_text(size=7,colour = 'black'))+
  theme(plot.background = element_rect(fill="white",color="white",size = 0.4))+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))+
  geom_point(data=mydata,aes(x=Longitude,y=Latitude),color="#FB8B40",size=0.7,shape=15)

ggsave("Fig.1a-2.pdf",width=3.6,height=3.6,path="D:/")

#Fig. 1b
sales <- c(20.3,79.7)
names<-c("a","b")
share<-sales/sum(sales)*100
data <- data.frame(
  sales,share,names)
ggplot()+
  geom_arc_bar(data=data,aes(x0 = 0, y0 = 0, r0 = 0, r = 1,amount=sales,explode=c(0,0.1),fill=names,color=names),stat="pie")+
  coord_fixed()+theme_void()+theme(legend.position = "none")+
  scale_color_manual(breaks=c("a","b"),values=c("lightgray","black"))+
  scale_fill_manual(breaks=c("a","b"),values=c("lightgray","black"))
ggsave("Fig.1b-1.pdf",width=3,height=3,path="D:/")

data<-data.frame(variable=c(14102,733,150,144,156,145,60,145,59,121,213), group = paste0("a", 1:11))
ggplot(data, aes(x = 3, y = variable, fill = group))+ geom_col() +
  coord_polar(theta = "y") +xlim(c(0.5, 4.5))+theme_void()+theme(legend.position = "none")+
  scale_fill_manual(breaks=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"),
                    values=c("#7AC5CD","#ff9d6f","#A0E632","#D8B0FF","#A67A13","#8E9CFF","#9D3CFF",
                              "#00A0FF","#FB6501","#E6AF2D","#EDE76D"))
ggsave("Fig.1b-2.pdf",width=3,height=3,path="D:/")

#Fig. 1c
data<-read_excel("Fig.1c.xlsx")
data$letter = factor(data$letter, levels=c("Lytic","Lysogenic"))
ggplot(data,aes(x=letter, y=ratio))+
  geom_violin(aes(fill=letter),trim=F,width=0.32,alpha=0.9,size=0.15)+
  geom_boxplot(width=0.18,size=0.15)+
  labs(x="",y="Relative proportion (%)")+
  geom_jitter(aes(x=letter, y=ratio,fill=letter),shape=21,stroke=0.15,size=0.65,
              position=position_jitterdodge(jitter.width = 0.4),alpha=0.8)+
  scale_fill_manual(values = c("#7AC5CD","#FF9B64"))+
  theme(axis.line = element_line(color="black"))+
  theme_test()+ 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=6.5),
        axis.ticks = element_line(size = 0.3),
        axis.ticks.length = unit(0.04, "cm"),
        axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
        axis.text.y=element_text(size=6,colour = 'black'),
        panel.border = element_rect(size=0.35))+
  ylim(0,100)+ theme(legend.position="none")

ggsave("Fig.1c.pdf",width=1.9,height=1.9,path="D:/")

#Fig. 1d
data<-read_excel("Fig.1d.xlsx")
data$tax=factor(data$tax,levels=c("Proteobacteria","Bacteroidetes","Chloroflexi","Acidobacteria","Actinobacteria",
                                  "Gemmatimonadetes","Planctomycetes","Verrucomicrobia","Nitrospirae","Cyanobacteria",
                                  "Other bacteria","Thaumarchaeota","Bathyarchaeota","Euryarchaeota","Thermoplasmatota",
                                  "Woesearchaeota","Altiarchaeota","Heimdallarchaeota","Kariarchaeota","Other archaea"))

data$class=factor(data$class, levels=c("Bacteria",'Archaea'))

legend<-ggplot(data, aes(x=class, y=proportion))+
  geom_bar(stat="identity", color="#696969",width=0.48,size=0.13,position="stack", aes(fill=tax))+
  scale_fill_manual(breaks = c("Proteobacteria","Bacteroidetes","Chloroflexi","Acidobacteria","Actinobacteria",
                               "Gemmatimonadetes","Planctomycetes","Verrucomicrobia","Nitrospirae","Cyanobacteria",
                               "Other bacteria","Thaumarchaeota","Bathyarchaeota","Euryarchaeota","Thermoplasmatota",
                               "Woesearchaeota","Altiarchaeota","Heimdallarchaeota","Kariarchaeota","Other archaea"),
                    values = c('#518DBE','#D79D65',"#63C28D",'#A155E8','#F1BD3E',
                               '#F18282',"#EDD9B4","#ECCEED",'#C0D0E5','#85D7BB',
                               "#CBCBCB",'#7DBDF1','#F1AE6D','#96D796',"#BC9BFF",
                               "#FFDF33",'#FB998E','#FCEBD0',"#F9E8F9",'#DCDCDC'))+
  labs(x="",y="Relative proportion (%)")+
  theme(legend.background=element_rect(color="white"),legend.key.size = unit(15, "pt"))+
  theme(legend.key = element_blank())+
  theme(axis.line = element_line(color=))+
  theme_test()+  
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title = element_text(size=6.5),
        axis.ticks = element_line(size = 0.23),
        axis.ticks.length = unit(0.04, "cm"),
        axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
        axis.text.y=element_text(size=6,colour = 'black'),
        panel.border = element_rect(size=0.35))+
#legend
theme(legend.title = element_blank(),legend.key.height = unit(0.2, "cm"), 
        legend.key.width = unit(0.2, "cm"),
        legend.text = element_text(size = 5))
legend1 <- get_legend(legend)
ggdraw(legend1)

ggsave("Fig.1d1.pdf",width=1.6,height=2,path="D:/")

#Fig. 1e
otu  <- read.delim('vOTU.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu1 <- read.delim('vPCs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu2 <-read.delim('mOTU.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu=otu[,-dim(otu)[1]]
otu1=otu1[,-dim(otu1)[1]]
otu2=otu2[,-dim(otu2)[1]]
otu=t(otu)
otu1=t(otu1)
otu2=t(otu2)
sp <- specaccum(otu,method="random")
sp1 <- specaccum(otu1,method="random")
sp2 <- specaccum(otu2,method="random")
result <- with(sp,data.frame(sites,richness,sd))
result1 <- with(sp1,data.frame(sites,richness,sd))
result2 <- with(sp2,data.frame(sites,richness,sd))
write.csv (result2,file ="culmulation_mOTU.csv", row.names = T,  quote =FALSE) 
write.csv (result1,file ="culmulation_PCS.csv", row.names = T,  quote =FALSE) 
write.csv (result,file ="culmulation_vOTU.csv", row.names = T,  quote =FALSE) 
result2<-read.csv('culmulation_mOTU.csv') 
result1 <-read.csv('culmulation_PCS.csv') 
result <-read.csv('culmulation_vOTU.csv') 
all_new <- data.frame(result,result1,result2)

ggplot(all_new)+
  geom_point(aes(x=sites.1,y=richness.1),shape=19,size=0.4,color="#7AC5CD",alpha=0.5)+
  geom_errorbar(aes(sites.1,ymin=richness.1-sd.1,ymax=richness.1+sd.1),lwd=0.14,width=0.7,alpha=0.85,position=position_dodge(0),
                color="#7AC5CD")+
  geom_point(aes(x=sites.2,y=richness.2),shape=19,size=0.4,color="gray",alpha=0.5)+
  geom_errorbar(aes(sites.2,ymin=richness.2-sd.2,ymax=richness.2+sd.2),lwd=0.14,width=0.7,alpha=0.85,position=position_dodge(0),
                color="gray")+
  geom_point(aes(x=sites,y=richness),shape=19,size=0.4,color="#FF9B64",alpha=0.5)+
  geom_errorbar(aes(sites,ymin=richness-sd,ymax=richness+sd),lwd=0.14,width=0.85,alpha=0.7,position=position_dodge(0),
                color="#FF9B64")+
  labs(x="# of samples",y="Cumulative number")+
  theme_test()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=6.5),
        axis.title.y = element_text(size=7),
        axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
        axis.text.y=element_text(size=6,colour = 'black'),
        axis.ticks = element_line(size = 0.3),
        axis.ticks.length = unit(0.04, "cm"),
        panel.border = element_rect(size=0.35))+
  scale_x_continuous(limits =c(0,100),breaks = seq(0,100,20))+ylim(0,10000)

ggsave("Fig.1e.pdf",width=2,height=1.9,path="D:/")

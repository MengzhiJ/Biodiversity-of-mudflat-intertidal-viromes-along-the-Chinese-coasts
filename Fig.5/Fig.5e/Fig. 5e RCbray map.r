library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
setwd("Fig.5")
data<-read_excel("RCbray.xlsx")
data$class <- factor(data$class,level=c("vOTUs","mOTUs","Virus (host)","Host"))
ggplot(data,aes(x=class,y=Ratio,fill=level))+
  geom_bar(stat="identity", color="NA",width=0.7,size=0.4,position="stack", aes(fill=level),alpha=0.8)+
  theme_bw()+labs(x="",y="Raup-Crick proportion")+
  scale_fill_manual(breaks=c("more","mid","less"),values = c("#7AC5CD","#FF8040","#d0d0d0"))+
  theme_test()+theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
                     axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
                     axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
                     axis.text.y=element_text(size=16,colour = 'black'),
                     panel.border = element_rect(size=1),
                     legend.text = element_text(size=15),
                     legend.title = element_text(size=16))+theme(legend.position = 'none')


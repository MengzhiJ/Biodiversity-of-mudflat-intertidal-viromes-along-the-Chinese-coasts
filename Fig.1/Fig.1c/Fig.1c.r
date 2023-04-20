library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
data<-read_excel("I:/Tidal/R_data/Fig.1c/Fig.1c.xlsx")
data$lifestyle = factor(data$lifestyle, levels=c("Lytic virus","Lysogenic virus"))
ggplot(data,aes(x=lifestyle, y=ratio))+geom_violin(aes(fill=lifestyle),trim=F,width=0.3)+geom_boxplot(width=0.18)+
  labs(x="",y="Relative proportion (%)")+geom_jitter(aes(x=lifestyle, y=ratio,fill=lifestyle),shape=21,size=1.7,position=position_jitterdodge(jitter.width = 0.4))+
  scale_fill_manual(values = c("#7AC5CD","#ff9d6f"))+
  theme(axis.line = element_line(color="black"))+
  theme_test()+ theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
                       axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
                       axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
                       axis.text.y=element_text(size=16,colour = 'black'),
                       panel.border = element_rect(size=1.8),
                       legend.text = element_text(size=15),
                       legend.title = element_text(size=16))+
  ylim(0,100)+ theme(legend.position="none")
ggsave("Fig.1c.tiff",width=5.5,height=5,path="I:/Tidal/R_data/Fig.1c") 

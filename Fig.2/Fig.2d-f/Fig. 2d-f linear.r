library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
data<-read_excel("I:/Tidal/R_data/Fig.2d-f/Fig.2d-f.xlsx")

#Fig.2d
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(color="gray",size = 2, alpha = 0.85,shape=16)+
  labs(x="TOC (g/kg)",y="Abundance of Glycosyltransferase")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#7AC5CD",method = 'lm',se=T,size=0.8,fullrange=F,fill="lightgray")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        axis.text.x = element_text(hjust =0.5,size=13,colour = 'black'),
        axis.text.y=element_text(size=13,colour = 'black'),
        panel.border = element_rect(size=1),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  stat_cor(method = "pearson",aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = )
ggsave("Fig.2d.tiff", width= 4.7,height= 4.5,path="I:/Tidal/R_data/Fig.2d-f")   


#Fig.2e
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(color="gray",size = 2, alpha = 0.85,shape=16)+
  labs(x="TP (g/kg)",y="Abundance of Phosphate starvation-inducible")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#7AC5CD",method = 'lm',se=T,size=0.8,fullrange=F,fill="lightgray")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        axis.text.x = element_text(hjust =0.5,size=13,colour = 'black'),
        axis.text.y=element_text(size=13,colour = 'black'),
        panel.border = element_rect(size=1),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  stat_cor(method = "pearson",aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = )
  ggsave("Fig.2e.tiff", width= 4.7,height= 4.5,path="I:/Tidal/R_data/Fig.2d-f") 
  
#Fig.2f
  ggplot(data, mapping = aes(x=factor,y=gene))+
    geom_point(color="gray",size = 2, alpha = 0.85,shape=16)+
    labs(x="",y="Abundance of assimilatory sulphate reduction")+
    theme(axis.line = element_line(color="black"))+
    geom_smooth(color="#7AC5CD",method = 'lm',se=T,size=0.8,fullrange=F,fill="lightgray")+
    theme_test()+ theme(legend.position ="none")+
    theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
          axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
          axis.text.x = element_text(hjust =0.5,size=13,colour = 'black'),
          axis.text.y=element_text(size=13,colour = 'black'),
          panel.border = element_rect(size=1),
          legend.text = element_text(size=15),
          legend.title = element_text(size=16))+
  stat_cor(method = "pearson",aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = )
  ggsave("Fig.2F.tiff", width= 4.6,height= 4.5,path="I:/Tidal/R_data/Fig.2d-f") 
  
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
data<-read_excel("I:/Tidal/R_data/Fig.3b,c/Fig.3b-c.xlsx")

#Fig.3b
ggplot(data, mapping = aes(x=Microbe,y=Virus))+
  geom_point(color="gray",size = 2.8, alpha = 0.85,shape=16)+
  labs(x="Microbial normalized abundance",y="Viral normalized abundance")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#FF8040",method = 'lm',se=F,size=0.8,fullrange=F,formula=y~poly(x,2))+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.2),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  scale_x_continuous(breaks = seq(0,20000,2500))+
  stat_poly_eq(aes(label = paste(..rr.label..,p.value.label,sep = "~`,`~"),size=2),
               formula = y ~ poly(x,2),parse = TRUE)

ggsave("Fig.3b.tiff",width=5.25,height=5,path="I:/Tidal/R_data/Fig.3b,c/")

#Fig.3c
data<-read_excel("I:/Tidal/R_data/Fig.3b,c/Fig.3b-c.xlsx")
ggplot(data, mapping = aes(x=Microbe,y=Viruses))+
  geom_point(aes(color=Lifestyle),size = 2.5, alpha = 0.75,shape=16)+
  labs(x="Microbial normalized abundance",y="Relative abundance of viruses (%)")+
  geom_smooth(aes(color=Lifestyle),method = 'lm',se=F,size=0.9,fullrange=F,formula=y~poly(x,2))+
  theme_test()+ theme(legend.position ="none")+scale_color_manual(breaks=c("Lytic","Lysogenic"),
                                                                  values=c("#7AC5CD","#FF8040"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.2),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  ylim(20,80)+ scale_x_continuous(breaks = seq(0,20000,2500))+
  stat_poly_eq(aes(color=Lifestyle,label = paste(..rr.label..,p.value.label,sep = "~`,`~"),size=2),
               formula = y ~ poly(x,2),parse = TRUE)
  ggsave("Fig.3c.tiff",width=5.1,height=5,path="I:/Tidal/R_data/Fig.3b,c/")
 

  
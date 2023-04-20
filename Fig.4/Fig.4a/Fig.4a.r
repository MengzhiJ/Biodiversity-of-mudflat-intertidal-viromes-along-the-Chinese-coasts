library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
data<-read_excel("I:/Tidal/R_data/Fig.4/Fig.4a.xlsx")
data$letter=factor(data$taxonomy, levels=c("Chloroflexi","Bacteroidetes","Deltaproteobacteria","Gammaproteobacteria","Alphaproteobacteria","Unknown"))
data$value=factor(data$variable, levels=c("Virus",'Microbe'))

p <- ggplot(data=data, aes(x=variable, y=value)) 
p+geom_bar(stat="identity", color="#696969",width=0.35,size=0.4,position="stack", aes(fill=taxonomy))+
  scale_fill_manual(breaks = c("Alphaproteobacteria","Gammaproteobacteria","Deltaproteobacteria","Chloroflexi","Bacteroidetes","Unknown"),
    values = c('#2570AE', '#539DDA',"#97CBFF", '#3CB371','#CD853F','lightgray'))+
  labs(x="",y="# of core OTUs")+
  theme(legend.background=element_rect(color="white"),legend.key.size = unit(15, "pt"))+
  theme(legend.key = element_blank())+
  theme(axis.line = element_line(color=))+
  theme_test()+  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
                       axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
                       axis.text.x = element_text(hjust =0.5,size=17.5,colour = 'black'),
                       axis.text.y=element_text(size=16,colour = 'black'),
                       panel.border = element_rect(size=1.2),
                       legend.text = element_text(size=15),
                       legend.title = element_text(size=16))+ theme(legend.position ="none")
ggsave("Fig. 4a.tiff",width=5,height=5,path="I:/Tidal/R_data/Fig.4/") 

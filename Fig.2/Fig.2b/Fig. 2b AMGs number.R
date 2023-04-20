library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
data<-read_excel("I:/Tidal/R_data/Fig.2b/Fig.2b.xlsx") 
data$gene=factor(data$gene, levels=c('Nitrification','Organic degradation and synthesis','Sulphur reduction','Sulphur oxidation','Assimilatory sulphate reduction',
                                     'Phosphonate and phosphinate metabolism','Organic phosphoester hydrolysis','Two-component system','Phosphate starvation-inducible (phoH)',
                                     'Carbohydrates Esterase','Cobalamin Synthesis','Glycosyltransferase','Glycoside Hydrolase'))
ggplot(data)+
geom_bar(stat="identity",width=0.65, position="dodge", aes(x=gene, y=value,fill=class),alpha=0.8)+scale_fill_manual(values = c("#7AC5CD","#FF8040","#FFDC35","#7373B9"))+
  theme(panel.border = element_blank())+
  labs(x="",y="# of viral genomes",color="Type")+
  theme(axis.line = element_line(color="black"))+coord_flip()+
  theme_test()+theme( axis.title.x = element_text(size=13),axis.title.y = element_text(size=13),
                      axis.text.x = element_text(hjust =0.5,size=10.5,colour = 'black'),
                      axis.text.y=element_text(size=10.5,colour = 'black'),
                      panel.border = element_rect(size=1),
                      legend.text = element_text(size=9.5),legend.title = element_blank())+
  theme(legend.position=c(0.7,0.15))+ylim(0,250)
ggsave("Fig.2b.tiff", width= 6.5,height= 4.8,path="I:/Tidal/R_data/Fig.2b")                    

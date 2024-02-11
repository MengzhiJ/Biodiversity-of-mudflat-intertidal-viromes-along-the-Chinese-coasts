#Fig.2a
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
setwd('D:/Tidal_spatial/R_data/github/Fig.2')
data<-read_excel('Fig.2.xlsx')
data_m <- melt(data)
#Gene number (frequency)
data_m$COG=factor(data_m$COG, levels=c('S','L','M','O','K','U','F','T','E','G','H','D','J','N','Q','V','I','C','P','W','Z','A','B'))

ggplot(data_m, aes(x=COG, y=value),color=COG) + 
  geom_boxplot(outlier.size=0,outlier.colour='white',aes(fill=factor(COG)),size = 0.25)+
  scale_fill_manual(breaks=c('S','L','M','O','K','U','F','T','E','G','H','D','J','N','Q','V','I','C','P','W','Z','A','B'),
    values = c('#7AC5CD','#FFD700','#00E5EE','#00CD00','#35978f','#FF6347','#EE9A49','#E0EEEE','#1AFD9C','#F5DEB3','#E6E6FA','#FFE7BA',
             '#AE57A4','#008B8B','#B0C4DE','#8E8E8E','#bf812d','#D1EEEE','#FF8EFF','#FF0000','#FFC1C1','#9ACD32','#CD6839'))+
  labs(x='COG function classes',y='# of viral genes',color='Type')+theme_test()+
  theme( axis.title.x = element_text(size=13.5),axis.title.y = element_text(size=13.5),
         axis.text.x = element_text(hjust =0.5,size=11.5,colour = 'black'),
         axis.text.y=element_text(size=11.5,colour = 'black'),
         panel.border = element_rect(size=1.2),
         legend.text = element_text(size=10),legend.title = element_blank())+
  theme(legend.position='none')+guides(fill = guide_legend(ncol = 2, byrow = TRUE))
ggsave("Fig. 2a.tiff",width=5,height=3.2, path="D:/")   

#Gene relative proportion (abundance)
data_m$COG=factor(data_m$COG, levels=c('S','L','M','K','O','F','U','T','E','G','H','D','J','Q','N','I','V','C','P','W','Z','A','B'))

ggplot(data_m, aes(x=COG, y=value),color=COG) + 
  geom_boxplot(outlier.size=0,outlier.colour='white',aes(fill=factor(COG)),size = 0.28,width=0.6)+
  scale_fill_manual(breaks=c('S','L','M','K','O','F','U','T','E','G','H','D','J','Q','N','I','V','C','P','W','Z','A','B'),
                    values = c('#7AC5CD','#FFD700','#00E5EE','#35978f','#00CD00','#EE9A49','#FF6347','#E0EEEE','#1AFD9C','#F5DEB3','#E6E6FA','#FFE7BA',
                               '#AE57A4','#008B8B','#B0C4DE','#8E8E8E','#bf812d','#D1EEEE','#FF8EFF','#FF0000','#FFC1C1','#9ACD32','#CD6839'))+
  labs(x='COG function classes',y='Relative proportion (%)',color='Type')+theme_test()+
  theme( axis.title.x = element_text(size=13),axis.title.y = element_text(size=13),
         axis.text.x = element_text(hjust =0.5,size=11,colour = 'black'),
         axis.text.y=element_text(size=11,colour = 'black'),
         panel.border = element_rect(size=1),
         legend.text = element_text(size=10),legend.title = element_blank())+
  theme(legend.position='none')+guides(fill = guide_legend(ncol = 2, byrow = TRUE))
ggsave("Fig. 2a.tiff",width=6,height=3.8, path="D:/")  
#Fig.2b
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
data<-read_excel("Fig.2.xlsx") 
data$score=factor(data$score, levels=c('Low confidence','High confidence'))
data$gene=factor(data$gene, levels=c('Anaerobic oxidation of methane','Aerobic oxidation of methane','Nitrification','Organic degradation and synthesis','Sulphur oxidation','Dissimilatory sulphate reduction','Assimilatory sulphate reduction',
                                     'Phosphate transport system','Oxidative phosphorylation','Organic phosphoester hydrolysis','Pentose phosphate pathway','Phosphate starvation induction',
                                     'Ester degradation','Polysaccharide degradation'))
ggplot(data)+
  geom_bar(stat="identity",width=0.75, position="dodge", aes(x=gene, y=value,fill=class),alpha=1)+scale_fill_manual(values = c("#95D1D7","#6BB5FF","#FF9966","#FFD966","#AAAAD5"))+
  theme(panel.border = element_blank())+
  labs(x="",y="# of viral genes",color="Type")+
  theme(axis.line = element_line(color="black"))+coord_flip()+
  theme_test()+theme( axis.title.x = element_text(size=11.5),axis.title.y = element_text(size=11.5),
                      axis.text.x = element_text(hjust =0.5,size=10.5,colour = 'black'),
                      axis.text.y=element_text(size=10.5,colour = 'black'),
                      panel.border = element_rect(size=1),strip.text=element_text(size=12),
                      legend.text = element_text(size=11),legend.title = element_blank()
                      )+
  theme(legend.position='none')+ylim(0,50)+facet_wrap(~score)
ggsave("Fig. 2b.tiff", width= 8,height= 3.8,path="D:/")                    

#Fig.2d
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpmisc)
#TOC-methane oxidation
data<-read_excel("Fig.2.xlsx") 
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(fill="gray",size = 2.7, alpha = 0.85,shape=21)+
  labs(x="Total organic carbon (g/kg)",y="Abundance of methane oxidation genes")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#95D1D7",method = 'lm',se=T,size=1.2,fullrange=T,fill="lightgray")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        axis.text.x = element_text(hjust =0.5,size=13.5,colour = 'black'),
        axis.text.y=element_text(size=13.5,colour = 'black'),
        panel.border = element_rect(size=1.2))+ylim(-30,160)+
  stat_poly_eq(aes(label = paste(..adj.rr.label.., ..p.value.label.., sep = "~`,`~")),size=4.5,
           label.x = )
ggsave("Fig.2d.tiff", width= 4.6,height= 4.1,path="D:/")      
#SO42--sulphate reduction
data<-read_excel("Fig.2.xlsx") 
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(fill="gray",size = 2.7, alpha = 0.85,shape=21)+
  labs(x="SO42- (g/kg)",y="Abundance of sulphate reduction genes")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#95D1D7",method = 'lm',se=F,size=1.2,fullrange=F,fill="lightgray")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        axis.text.x = element_text(hjust =0.5,size=13.5,colour = 'black'),
        axis.text.y=element_text(size=13.5,colour = 'black'),
        panel.border = element_rect(size=1.2))+ylim(-40,350)+
  stat_poly_eq(aes(label = paste(eq.label,..adj.rr.label.., ..p.value.label.., sep = "~`,`~")),size=4.5,
               label.x = )
ggsave("Fig.S1A.tiff", width= 5.6,height= 5.1,path="D:/") 
#TP-phoh
data<-read_excel("Fig.2.xlsx")
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(fill="gray",size = 2.7, alpha = 0.85,shape=21)+
  labs(x="TP (g/kg)",y="Abundance of phosphate starvation-inducible genes")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#95D1D7",method = 'lm',se=F,size=1.2,fullrange=F,fill="lightgray")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
        axis.text.x = element_text(hjust =0.5,size=13.5,colour = 'black'),
        axis.text.y=element_text(size=13.5,colour = 'black'),
        panel.border = element_rect(size=1.2))+ylim(-20,230)+
  stat_poly_eq(aes(label = paste(eq.label,..adj.rr.label.., ..p.value.label.., sep = "~`,`~")),size=4.5,
               label.x = )
ggsave("Fig.S1B.tiff", width= 5.6,height= 5.1,path="D:/") 

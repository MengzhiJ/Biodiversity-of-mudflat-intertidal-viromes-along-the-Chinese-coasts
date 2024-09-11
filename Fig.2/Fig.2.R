#R_code (v4.2.0) for Figure 2
#load all need packages
library(ggplot2)
library(dplyr)
library(readxl)
library(ggpmisc)

#Fig.2a
setwd('D:/Tidal_spatial/R_data/github/Fig.2/raw_data_for_figure2')
data<-read_excel('Fig.2.xlsx')
data_m<-melt(data)
#Gene number (frequency)
data_m$COG=factor(data_m$COG, levels=c('S','L','M','O','K','U','F','T','E','G','H','D','J','N','Q','V','I','C','P','W','Z','A','B'))
ggplot(data_m, aes(x=COG, y=value),color=COG) + 
  geom_boxplot(outlier.size=0,outlier.colour='white',aes(fill=factor(COG)),size = 0.08)+
  scale_fill_manual(breaks=c('S','L','M','O','K','U','F','T','E','G','H','D','J','N','Q','V','I','C','P','W','Z','A','B'),
    values = c('#7AC5CD','#FFD700','#00E5EE','#00CD00','#35978f','#FF6347',
               '#EE9A49','#E0EEEE','#1AFD9C','#F5DEB3','#E6E6FA','#FFE7BA',
             '#AE57A4','#008B8B','#B0C4DE','#8E8E8E','#bf812d','#D1EEEE',
             '#FF8EFF','#FF0000','#FFC1C1','#9ACD32','#CD6839'))+
  labs(x='COG function classes',y='# of viral genes',color='Type')+
  theme_test()+
  theme( axis.title = element_text(size=5),
         axis.text.x = element_text(hjust =0.5,size=4.5,colour = 'black'),
         axis.text.y=element_text(size=4.5,colour = 'black'),
         axis.ticks = element_line(size = 0.2),
         axis.ticks.length = unit(0.03, "cm"),
         panel.border = element_rect(size=0.35))+
  theme(legend.position='none')

#Gene relative proportion (abundance)
data_m$COG=factor(data_m$COG, levels=c('S','L','M','K','O','F','U','T','E','G','H','D','J','Q','N','I','V','C','P','W','Z','A','B'))
ggplot(data_m, aes(x=COG, y=value),color=COG) + 
  geom_boxplot(outlier.size=0,outlier.colour='white',aes(fill=factor(COG)),size = 0.1,width=0.65)+
  scale_fill_manual(breaks=c('S','L','M','K','O','F','U','T','E','G','H','D','J','Q','N','I','V','C','P','W','Z','A','B'),
                    values = c('#7AC5CD','#FFD700','#00E5EE','#35978f','#00CD00','#EE9A49',
                               '#FF6347','#E0EEEE','#1AFD9C','#F5DEB3','#E6E6FA','#FFE7BA',
                               '#AE57A4','#008B8B','#B0C4DE','#8E8E8E','#bf812d','#D1EEEE',
                               '#FF8EFF','#FF0000','#FFC1C1','#9ACD32','#CD6839'))+
  labs(x='COG function classes',y='Relative proportion (%)',color='Type')+theme_test()+
  theme( axis.title.x = element_text(size=6.5),
         axis.title.y = element_text(size=6.5),
         axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
         axis.text.y=element_text(size=6,colour = 'black'),
         axis.ticks = element_line(size = 0.25),
         axis.ticks.length = unit(0.04, "cm"),
         panel.border = element_rect(size=0.4))+
  theme(legend.position='none')

#Fig.2b
data<-read_excel("Fig.2.xlsx") 
data$score=factor(data$score, levels=c('Low confidence','High confidence'))
data$gene=factor(data$gene, levels=c('Anaerobic oxidation of methane','Aerobic oxidation of methane',
                                     'Nitrification','Organic degradation and synthesis','Sulphur oxidation',
                                     'Dissimilatory sulphate reduction','Assimilatory sulphate reduction',
                                     'Phosphate transport system','Oxidative phosphorylation',
                                     'Organic phosphoester hydrolysis','Pentose phosphate pathway',
                                     'Phosphate starvation induction',
                                     'Ester degradation','Polysaccharide degradation'))
ggplot(data)+
  geom_bar(stat="identity",width=0.75, position="dodge", aes(x=gene, y=value,fill=class),alpha=1)+
  scale_fill_manual(values = c("#95D1D7","#6BB5FF","#FF9966","#FFD966","#AAAAD5"))+
  theme(panel.border = element_blank())+
  labs(x="",y="# of viral genes",color="Type")+
  theme(axis.line = element_line(color="black"))+coord_flip()+
  theme_test()+theme( axis.title.x = element_text(size=5.5),
                      axis.title.y = element_text(size=5.5),
                      axis.text.x = element_text(hjust =0.5,size=5,colour = 'black'),
                      axis.text.y=element_text(size=5,colour = 'black'),
                      axis.ticks = element_line(size = 0.25),
                      axis.ticks.length = unit(0.04, "cm"),
                      strip.text=element_text(size=5.5, margin = margin(2, 2, 2, 2)),
                      strip.background = element_rect(color = "black", size = 0.35),
                      panel.border = element_rect(size=0.4))+
  theme(legend.position='none')+ylim(0,50)+facet_wrap(~score)                

#Fig.2d
#TOC-methane oxidation
data<-read_excel("Fig.2.xlsx") 
ggplot(data, mapping = aes(x=factor,y=gene))+
  geom_point(fill="lightgray",size = 1.2, alpha = 0.85,shape=21,stroke=0.15)+
  labs(x="Total organic carbon (g/kg)",y="Abundance of methane oxidation genes")+
  theme(axis.line = element_line(color="black"))+
  geom_smooth(color="#95D1D7",method = 'lm',se=T,size=0.4,fullrange=T,fill="lightgray",alpha=0.25)+
  theme_test()+ theme(legend.position ="none")+
  theme( axis.title.x = element_text(size=5.5),
         axis.title.y = element_text(size=5.5),
         axis.text.x = element_text(hjust =0.5,size=5,colour = 'black'),
         axis.text.y=element_text(size=5,colour = 'black'),
         axis.ticks = element_line(size = 0.25),
         axis.ticks.length = unit(0.04, "cm"),
         panel.border = element_rect(size=0.4))+
  ylim(-30,160)+
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~`,`~")),size=2,
           label.x = )   

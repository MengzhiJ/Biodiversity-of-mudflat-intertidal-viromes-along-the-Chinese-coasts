library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
data<-read_excel("I:/Tidal/R_data/Fig.2a/Fig.2a.xlsx")
data_m <- melt(data)
data_m$COG=factor(data_m$COG, levels=c('S','L','M','O','K','U','F','T','E','G','H','D','J','N','Q','V','I','C','P','W','Z','A','B'))
ggplot(data_m, aes(x=COG, y=value),color=Type) + 
  geom_boxplot(outlier.size=0,outlier.colour="white",aes(fill=factor(Type))) + 
  scale_fill_manual(values = c("#00E5EE", "#00CD00", "#7AC5CD", "#FFD700","#35978f","#FF6347",
                               "#EE9A49","#E0EEEE","#EE9A00","#F5DEB3","#E6E6FA","#FFD700","#FFE7BA",
                               "#AE57A4","#008B8B","#B0C4DE","#8E8E8E","#bf812d","#D1EEEE","#FF8EFF","#FF0000","#FFC1C1","#9ACD32","#CD6839","#FFD700"))+
  labs(x="COG function class",y="# of viral genes",color="Type")+theme_test()+
  theme( axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),
                                                        axis.text.x = element_text(hjust =0.5,size=12,colour = 'black'),
                                                        axis.text.y=element_text(size=12,colour = 'black'),
                                                        panel.border = element_rect(size=1),
                                                        legend.text = element_text(size=10),legend.title = element_blank())+
  theme(legend.position="none")+guides(fill = guide_legend(ncol = 2, byrow = TRUE))
ggsave("Fig. 2a.tiff",width=6.3,height=4, path="I:/Tidal/R_data/Fig.2a")   

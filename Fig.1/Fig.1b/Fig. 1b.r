library(ggplot2)
library(RColorBrewer)
library(readxl)

data<-read_excel("I:/Tidal/R_data/Fig.1b/Fig.1b.xlsx",col_names = T)

ggplot(data = data, mapping = aes(length, abundance))+
  geom_point(aes(fill = quality),size = 2.8, alpha = 1,shape=21)+
  scale_fill_manual(breaks=c('Complete','High-quality','Medium-quality','Low-quality','Not-determined'),
                    values=c("#696969","#8470FF","#ff9d6f","#7AC5CD","#CFCFCF"))+
  labs(x="Genome size (kb)",y="Viral normalized abundance",color="Genome quality")+
  theme(axis.line = element_line(color="black"))+xlim(0,250)+ 
  theme_test()+theme(legend.position=c(0.75,0.8))+
  guides(fill=guide_legend(title="Genome quality"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.8),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))

ggsave("Fig.1B.tiff",width=5.5,height=5,path="I:/Tidal/R_data/Fig.1b") 

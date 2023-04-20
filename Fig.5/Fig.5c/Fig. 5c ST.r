library(readxl)
library(ggplot2)
data<-read_excel("I:/Tidal/R_data/Fig.5/NST.xlsx") 
data$letter=factor(data$letter, levels=c("vOTUs","mOTUs","Virus (host)","Host"))


ggplot(data,aes(x=letter,y=value))+
  stat_summary(aes(fill=letter), alpha=0.8,fun = mean,geom="bar",color="NA",width=0.55,size=0.3)+
  stat_summary(fun.data = mean_se,geom="errorbar",width=.08)+
  theme_bw()+labs(x="",y="Normalized stochastic ratio (%)")+
  scale_fill_manual(breaks=c("Host","vOTUs","mOTUs","Virus (host)"),
                     values=c("#7AC5CD","#FF8040","gray","#4DB2F0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+theme(legend.position = 'none')+ coord_cartesian(ylim = c(0,100))
geom_hline(yintercept=c(50), linetype="dotted") #add Horizontal Line 
theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) #remove yaxis
ggsave("Fig.5c.tiff",width=5.6,height=5.6,path="I:/Tidal/R_data/Fig.5/")


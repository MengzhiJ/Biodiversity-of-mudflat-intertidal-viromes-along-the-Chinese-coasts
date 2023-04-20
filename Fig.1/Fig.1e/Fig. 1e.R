library(vegan)
library(readxl)
otu  <- read.delim('I:/Tidal/R_data/Fig.1e/vOTU.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu1 <- read.delim('I:/Tidal/R_data/Fig.1e/vPCs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu2 <-read.delim('I:/Tidal/R_data/Fig.1e/mOTU.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
otu=otu[,-dim(otu)[1]]
otu1=otu1[,-dim(otu1)[1]]
otu2=otu2[,-dim(otu2)[1]]
otu=t(otu)
otu1=t(otu1)
otu2=t(otu2)
sp <- specaccum(otu,method="random")
sp1 <- specaccum(otu1,method="random")
sp2<-specaccum(otu2,method="random")
result <- with(sp,data.frame(sites,richness,sd))
result1<-with(sp1,data.frame(sites,richness,sd))
result2<-with(sp2,data.frame(sites,richness,sd))
write.csv (result1,file ="I:/Tidal/R_data/Fig.1e/culmulation_PCS.csv", row.names = T,  quote =FALSE) 
write.csv (result,file ="I:/Tidal/R_data/Fig.1e/culmulation_vOTU.csv", row.names = T,  quote =FALSE) 

result1 <-read.csv('I:/Tidal/R_data/Fig.1e/culmulation_PCS.csv') 
result <-read.csv('I:/Tidal/R_data/Fig.1e/culmulation_vOTU.csv') 
all_new<-data.frame(result,result1,result2)

ggplot(all_new)+geom_point(aes(x=sites.1,y=richness.1),shape=19,size=2.5,color="#7AC5CD",alpha=0.5)+
  geom_errorbar(aes(sites.1,ymin=richness.1-sd.1,ymax=richness.1+sd.1),lwd=0.7,width=1,alpha=0.8,position=position_dodge(0),
                color="#7AC5CD")+
  geom_point(aes(x=sites.2,y=richness.2),shape=19,size=2.5,color="gray",alpha=0.5)+
  geom_errorbar(aes(sites.2,ymin=richness.2-sd.2,ymax=richness.2+sd.2),lwd=0.7,width=1,alpha=0.8,position=position_dodge(0),
                color="gray")+
  geom_point(aes(x=sites,y=richness),shape=19,size=2.5,color="#FF8040",alpha=0.5)+
  labs(x="# of samples",y="Cumulative number")+
  geom_errorbar(aes(sites,ymin=richness-sd,ymax=richness+sd),lwd=0.7,width=1,alpha=0.8,position=position_dodge(0),
                color="#FF8040")+theme_test()+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18.5),axis.title.y = element_text(size=18.5),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.8),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  scale_x_continuous(limits =c(0,100),breaks = seq(0,100,20))+ylim(0,10000)

ggsave("Fig. 1e.tiff",width=5.8,height=5,path="I:/Tidal/R_data/Fig.1e") 

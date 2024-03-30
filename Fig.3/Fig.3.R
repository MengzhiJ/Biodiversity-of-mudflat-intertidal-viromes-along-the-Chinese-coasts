library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
library(ggpmisc)
library(nlme)
setwd('D:/Tidal_spatial/R_data/github/Fig.3')
data<-read_excel('Fig.3.xlsx')
data_m <- melt(data)
write.csv(data_m,file="D:/Tidal_spatial/R_data/github/Fig.3/host.csv")
#Fig.3b
data<-read_excel('Fig.3.xlsx')
ggplot(data, mapping = aes(x=Host,y=Virus))+
  geom_point(size = 3.3, alpha = 0.8,shape=21,fill="lightgray")+
  labs(x="Normalized host microbial abundance",y="Normalized viral abundance")+
  geom_smooth(method = 'lm',se=F,size=1.4,fullrange=F,formula =y ~ poly(x, 2),color="#7AC5CD")+
  theme_test()+ theme(legend.position ="none")+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.5),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+ylim(1500,15000)+xlim(1000,6700)+
  stat_poly_eq(aes(label = paste(..rr.label..,p.value.label, sep = "~`,`~")),size=5.5,
               formula =y ~ poly(x, 2),parse = TRUE)

object1<-gls(Virus~poly(Host,2), data=data) #low AIC
object2<-gls(Virus~poly(Host), data=data)
summary(object1)
summary(object2)

#Fig.3c
data<-read_excel('Fig.3.xlsx')
ggplot(data, mapping = aes(x=Host,y=Lifestyles))+
  geom_point(aes(fill=Lifestyle),size = 3.2, alpha = 0.8,shape=21)+
  labs(x="Normalized host microbial abundance",y="Relative proportion of viruses (%)")+
  geom_smooth(aes(color=Lifestyle),method = 'lm',se=F,size=1.2,fullrange=F,formula =y ~ poly(x, 2))+
  theme_test()+ theme(legend.position ="none")+scale_fill_manual(breaks=c("Lytic","Lysogenic"),
                                                                 values=c("#7AC5CD","#FF9B64"))+
  scale_color_manual(breaks=c("Lytic","Lysogenic"),
                     values=c("#7AC5CD","#FF9B64"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.5),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))+
  ylim(20,80)+ scale_x_continuous(breaks = seq(0,20000,2500))+xlim(1000,6700)
  stat_poly_eq(aes(color=Lifestyle,label = paste(..rr.label..,p.value.label, sep = "~`,`~")),
               formula =y ~ poly(x, 2),parse = TRUE)
ggsave("Fig.3c.tiff",width=4.8,height=5,path="D:/")

object1<-gls(Lifestyles~poly(Host,2), data=data) #low AIC
object2<-gls(Lifestyles~poly(Host), data=data)
summary(object1)
summary(object2)

#Fig.3d and e
library(Hmisc)
library(psych)
setwd('D:/Tidal_spatial/R_data/github/Fig.3/VHR')
#calculate the correlations between each virus and host pair
virus_host_abundance<-read.csv("virus_host.csv", row.names = 1)
pairs<-read.csv("pair.csv")
correlation_matrix<-rcorr(as.matrix(t(virus_host_abundance)),type='pearson')
correlation_matrix_r<-correlation_matrix$r
correlation_matrix_p<-correlation_matrix$P
correlation_r <- data.frame(
  virus = pairs$virus,
  host = pairs$host,
  Correlation = correlation_matrix_r[cbind(match(pairs$host, rownames(correlation_matrix_r)), 
                                           match(pairs$virus, colnames(correlation_matrix_r)))]
)
correlation_p <- data.frame(
  virus = pairs$virus,
  host = pairs$host,
  Correlation = correlation_matrix_p[cbind(match(pairs$host, rownames(correlation_matrix_p)), 
                                           match(pairs$virus, colnames(correlation_matrix_p)))]
)

correlation<-cbind(correlation_r,correlation_p[,3])
write.table(correlation_matrix_r,file = "virus-host-r-matrix.csv", quote = FALSE,sep = ",")
write.table(correlation_matrix_p,file = "virus-host-p-matrix.csv", quote = FALSE,sep = ",")
write.table(correlation,file = "virus-host-correlation.csv", quote = FALSE,sep = ",") 

#extract virus/host abundance according to significant virus-host pairs
sig_pair <- read.csv("sig_pair.csv")
extract <- match(sig_pair$host, rownames(virus_host_abundance))
sig_host <- virus_host_abundance[extract, ]
colnames(sig_host) <- colnames(virus_host_abundance)
write.table(sig_host,file = "D:/Tidal_spatial/R_data/github/Fig.3/VHR/pair/host_pair.csv", quote = FALSE,sep = ",")

sig_pair <- read.csv("sig_pair.csv")
extract <- match(sig_pair$virus, rownames(virus_host_abundance))
sig_virus <- virus_host_abundance[extract, ]
colnames(sig_virus) <- colnames(virus_host_abundance)
write.table(sig_virus,file = "D:/Tidal_spatial/R_data/github/Fig.3/VHR/pair/virus_pair.csv", quote = FALSE,sep = ",")

# calculate VHR
setwd('D:/Tidal_spatial/R_data/github/Fig.3/VHR/pair')
virus_abundance<-read.csv("virus_pair.csv", row.names = 1)
host_abundance<-read.csv("host_pair.csv", row.names = 1)
rows1 <- sig_pair$virus
rows2 <- sig_pair$host
division_results <- matrix(NA, nrow = length(rows1), ncol = ncol(virus_abundance))
rownames(division_results) <- rows2
colnames(division_results) <- colnames(virus_abundance)
for (i in 1:length(rows1)) {
  for (j in 1:ncol(virus_abundance)) {
    division_results[i, j] <- virus_abundance[rows1[i], j] / host_abundance[rows2[i], j]
  }
}
VHR<-as.data.frame(division_results)
write.table(VHR,file = "VHR.csv", quote = FALSE,sep = ",")

#calculate correlations between VHR and host of significant virus-host pairs
VHR<-as.data.frame(t(read.csv("VHR_LOG.csv", row.names = 1)))
HOST<-as.data.frame(t(read.csv("host_pair_LOG.csv", row.names = 1)))
correlation<-corr.test(HOST,VHR,use="pairwise",method="pearson")
correlation_R<-t(correlation$r)
correlation_P<-t(correlation$p)
R<- data.frame(RowName = rownames(correlation_R), Value = NA)
for (i in 1:nrow(correlation_R)) {
  R$Value[i] <- correlation_R[i, i]
}
P<- data.frame(RowName = rownames(correlation_P), Value = NA)
for (i in 1:nrow(correlation_P)) {
  P$Value[i] <- correlation_P[i, i]
}
correlation_RP<-cbind(R,P$Value)
write.table(correlation_RP,file = "VHR_host_correlation.csv", quote = FALSE,sep = ",")


library(ggplot2)  
data<-read_excel('Fig.3.xlsx')
ggplot(data, aes(x=Rvalue,y=..density..))+  
  geom_histogram(binwidth = 0.1,alpha=0.5,colour="black",size=0.5,fill="gray")+
  geom_density(alpha=.65,fill="gray",color="gray",lwd=0.75)+
  theme_test()+
  theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.3))+ylim(0,5)+xlim(-1,1.05)+
  labs(x="",y="")
ggsave("Fig.3d.tiff",width=4.5,height=4.6,path="D:/")


data<-read_excel('Fig.3.xlsx')
ggplot(data, aes(x=Rvalue,y=..density..))+  
  geom_histogram(binwidth = 0.1,alpha=0.5,colour="black",size=0.5,fill="#7AC5CD")+
  geom_density(alpha=.65,fill="#7AC5CD",color="#7AC5CD",lwd=0.8)+
  theme_test()+
  theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
        axis.text.x = element_text(hjust =0.5,size=16,colour = 'black'),
        axis.text.y=element_text(size=16,colour = 'black'),
        panel.border = element_rect(size=1.3))+ylim(0,1.3)+xlim(-1.05,1.05)+
  labs(x="",y="")
ggsave("Fig.3e.tiff",width=4.5,height=4.6,path="D:/")

#Fig.3f
data<-read.delim('HOST-virus-correlation.txt',header = T,row.names = 1)
library(Hmisc)
cor<-rcorr(as.matrix(data),type='pearson')
r<-cor$r
p<-cor$P

write.table (r,file ="r.txt", row.names = T, col.names =T, quote =FALSE,sep = "\t",) 
write.table (p,file ="p.txt", row.names = T, col.names =T, quote =F,sep = "\t",)
r<-read.delim('r_VHR.txt',header = T,row.names = 1)
p<-read.delim('p_VHR.txt',header = T,row.names = 1)
p<-as.matrix(p)
r<-as.matrix(r)
cols<-colorRampPalette(c(rgb(91,194,205,max=255),rgb(255,255,255,max=255),rgb(255,128,64,max=255)))(200)
library(corrplot)
corrplot(r,tl.srt=45,tl.col = 'black',method='square',tl.cex=0.8,
         p.mat = p,insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 1.2,pch.col = 'black',cl.cex = 1,
         col=cols,tl.pos="b",cl.pos = "n",cl.ratio = 0.25)
ggsave("1.tiff",width=5,height=5,path="F:/")
#Fig.3g
#DOM analysis
library(Hmisc)
library(psych)
library(data.table)
dom<-read.csv('DOM_vOTUs_combined.csv',row.names = 1)  
dom<-as.data.frame(t(dom))
cor<-rcorr(as.matrix(dom),type='spearman')
r<-cor$r
p<-cor$P
r<-r[1:20980,20981:41082]
p<-p[1:20980,20981:41082]
p<-p.adjust(p,method="bonferroni")
r<-as.data.frame(r)
p<-as.data.frame(p_adjust)
data1=melt(r,value.name = "r")
data2=melt(p,value.name = "p")
data<-cbind(data1,data2)
filtered_rows <- data[abs(data$r)>0.7&data$p< 0.05, ]
clean_data<-na.omit(filtered_rows)
sum(abs(data$r)<-0.7&data$p<0.05,na.rm=TRUE)
write.table(clean_data,file="dom_virus.csv", quote = FALSE,sep = ",")

#load packages
library(circlize)
library(viridis)
library(reshape2)

#info dataset
df <- read.csv("circle.CSV", header=TRUE,stringsAsFactors = FALSE,check.names = FALSE)
df_melt<-melt(df,id.vars = 'Type')
colnames(df_melt)<-c('from','to','value')
df_melt$to<-as.character(df_melt$to)

#define factor
df_sum<-apply(df[,2:ncol(df)],2,sum)+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)
df_melt$from<-factor(df_melt$from,levels=df$Type[order$ix],order=TRUE)

df_melt<-dplyr:: arrange (df_melt, from)

#define color
mycolor = c(Aliphatic="#81D1DA",Amino="#D4D7D5",
            Carbohydrate="#CE99FF",
            Aromatic="#FFC8D4",
            Phenolic="#F9F93E",
            Lignin="#FFC284",
            Lipid="#68ACCF",
            Polyphenol="#76EEAB",
            Protein="#FFA98F",
            Tannin="#00CD66",Gammaproteobacteria="#B1D8FF",Alphaproteobacteria="#51A0EE",
            Deltaproteobacteria="#8ACAFF",Chloroflexi="#A58BDB",Bacteroidetes="#EE8B78",	
            Acidobacteria="#E4F2E4",Actinobacteria="#94EED0",Planctomycetes="#EEEED1",	
            Gemmatimonadetes="#FFD700",	Cyanobacteria="#ADD8E6",Verrucomicrobia="#DAA520",
            Firmicutes="#EEE685",Betaproteobacteria="#69B5FF")

#circos plot
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))
chordDiagram(
  x = df_melt,
  grid.col = mycolor,order = c("Aliphatic","Amino","Carbohydrate","Aromatic","Phenolic","Lignin","Lipid","Polyphenol","Protein","Tannin",
                               "Acidobacteria","Actinobacteria","Bacteroidetes","Chloroflexi","Cyanobacteria","Firmicutes","Gemmatimonadetes","Planctomycetes",
                               "Alphaproteobacteria","Betaproteobacteria","Deltaproteobacteria","Gammaproteobacteria","Verrucomicrobia"),
  transparency = 0.65,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

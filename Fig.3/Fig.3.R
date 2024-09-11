#R_code (v4.2.0) for Figure 3
#load all need packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
library(ggpmisc)
library(nlme)
library(Hmisc)
library(psych)

setwd('D:/Tidal_spatial/R_data/github/Fig.3/raw_data_for_figure3/')
#Fig.3b and c
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
write.table(sig_host,file = "host_pair.csv", quote = FALSE,sep = ",")

sig_pair <- read.csv("sig_pair.csv")
extract <- match(sig_pair$virus, rownames(virus_host_abundance))
sig_virus <- virus_host_abundance[extract, ]
colnames(sig_virus) <- colnames(virus_host_abundance)
write.table(sig_virus,file = "virus_pair.csv", quote = FALSE,sep = ",")

# calculate VHR
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

#Fig.3b,c map
data<-read_excel('Fig.3.xlsx')
ggplot(data, aes(x=correlation_r,y=..density..))+  
  geom_histogram(binwidth = 0.1,alpha=0.5,colour="black",size=0.18,fill="gray")+
  geom_density(alpha=.65,fill="gray",color="gray",lwd=0.22)+
  theme_test()+
  theme(axis.title.x = element_text(size=6.5),
        axis.title.y = element_text(size=6.5),
        axis.text.x = element_text(hjust =0.5,size=5.5,colour = 'black'),
        axis.text.y=element_text(size=5.5,colour = 'black'),
        axis.ticks = element_line(size = 0.25),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size=0.4))+
  ylim(0,5)+xlim(-1,1.05)+
  labs(x="",y="")

data<-read_excel('Fig.3.xlsx')
ggplot(data, aes(x=VHR_host_Rvalue,y=..density..))+  
  geom_histogram(binwidth = 0.1,alpha=0.5,colour="black",size=0.18,fill="#7AC5CD")+
  geom_density(alpha=.45,fill="#7AC5CD",color="#7AC5CD",lwd=0.22)+
  theme_test()+
  theme(axis.title.x = element_text(size=6.5),
        axis.title.y = element_text(size=6.5),
        axis.text.x = element_text(hjust =0.5,size=5.5,colour = 'black'),
        axis.text.y=element_text(size=5.5,colour = 'black'),
        axis.ticks = element_line(size = 0.25),
        axis.ticks.length = unit(0.05, "cm"),
        panel.border = element_rect(size=0.4))+
  xlim(-1.05,1.05)+ylim(0,2.5)+
  labs(x="",y="")

#Fig.3d
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
         p.mat = p,insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 1.2,pch.col = 'black',cl.cex = 0.1,
         col=cols,tl.pos="b",cl.pos = "r",cl.ratio = 0.7)

#Fig.3e
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
